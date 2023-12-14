library(readr)
library(tidymodels)
library(bonsai)
library(probably)
library(lubridate)
source("00_functions.R")
imputeDF <- read_csv("data/processed/imputeDF.csv")
propensityDF <- imputeDF %>%  
  select(-`1st Draw`,-`2-3 Minute`,-`5 Minute`,-overOne_2,
         -HHsInternetPropBG,-pCompletePlumbingFacilitiesBG,
         -HHsHasComputerPropBG,-pOccupiedHousesBG,-sequential,
         -pRenterOccupiedHousesBG,-`Date Sampled`) %>%  #remove redundant features
  mutate(censusTract = factor(censusTract),
         CA = factor(CA),
         blockNum = factor(blockNum),
         blockGroup = factor(blockGroup),
         tested = factor(tested)
  )

split_df <- group_initial_split(propensityDF,group=blockNum) #keep blocks in same split
trainDF <- training(split_df)
testDF <- testing(split_df)
tree_rec <- recipe(tested ~ ., data = propensityDF) %>% 
  update_role(blockNum, new_role="ID")


m.out <- matchit(tested ~ blockGroup+censusTract+CA, data = propensityDF, method = "nearest")


# Identify categorical variables
categorical_vars <- c("censusTract","CA","blockGroup")
non_categorical_vars <- setdiff(names(propensityDF), c("tested", categorical_vars))
formula <- as.formula(paste("tested ~", paste(non_categorical_vars, collapse = " + "), "|", paste(categorical_vars, collapse = " + ")))

propensityDF$tested <- as.numeric(propensityDF$tested)-1
mod <- fixest::feglm(formula, data=propensityDF)

mod2 <- fixest::feglm(formula, data=propensityDF,
                      family="binomial")



#lightgbm specification
tune_spec <- boost_tree(
  min_n = tune(),
  trees = 1000,
  tree_depth = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm") #xgboost or lightgbm
tune_wf <- workflow() %>%
  #add_formula(overOne_2 ~ .-blockNum) %>% 
  add_recipe(tree_rec) %>% 
  add_model(tune_spec)


##model comparison
trees_folds3 <- group_vfold_cv(trainDF,group=blockNum,v=3)
trees_folds_final <- group_vfold_cv(trainDF,group=blockNum,v=5)
doParallel::registerDoParallel()
set.seed(127)
#lightgbm train
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds3,
  grid = 5
)
tune_res %>%
  tune::show_best(metric = "roc_auc",n = 5)


##retune using 5-fold + more exhaustive grid search
tune_res_final2 <- tune_grid(
  tune_wf2,
  resamples = trees_folds3,
  grid = 15
)
tune_res_final2 %>%
  tune::show_best(metric = "roc_auc",n = 5)

best_tree2 <- tune_res_final2 %>%
  select_best("roc_auc")


final_wf2 <- 
  tune_wf2 %>% 
  finalize_workflow(best_tree2)
saveRDS(final_wf2,"data/processed/outcomeWF.rds")

final_fit2 <- 
  final_wf2 %>%
  last_fit(split_df2)


final_fit2 %>% 
  collect_metrics()

myModel2 <- extract_fit_engine(final_fit2)
impObj2 <- lgb.importance(myModel2,percentage = F)


#make calibrated risk predictions
predSplits <- group_initial_split(propensityDF,group=blockNum,prop=1/2)
split1 <- training(predSplits)
split2 <- testing(predSplits)

splitDF1 <- fitSplit(dataSplit = split1, testData=split2,gridNum=10)
splitDF2 <- fitSplit(dataSplit = split2, testData=split1,gridNum=10)

riskDF <- rbind(splitDF1,splitDF2)

write_csv(riskDF,"data/processed/riskDF_pt1.csv")

#make outcome predictions on blocks without tests
imputeDF <- read_csv("data/processed/imputeDF.csv")
imputeDF <- imputeDF %>%
  select(-`1st Draw`,-`2-3 Minute`,-`5 Minute`,
         -HHsInternetPropBG,-pCompletePlumbingFacilitiesBG,
         -HHsHasComputerPropBG,-pOccupiedHousesBG,-sequential,
         -pRenterOccupiedHousesBG) %>%  #remove redundant features
  mutate(overOne_2 = factor(overOne_2),
         censusTract = factor(censusTract),
         CA = factor(CA),
         blockNum = factor(blockNum),
         blockGroup = factor(blockGroup))
propensity_train <- imputeDF %>% filter(tested==T) %>% 
  select(-tested)
propensity_test <- imputeDF %>% filter(tested==F) %>% 
  select(-tested)


withoutTestsPreds <- fitSplit(dataSplit = propensity_train,
                              testData=propensity_test,gridNum=5)


withoutTests <- withoutTestsPreds %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds)
withTests <- riskDF %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds)
riskDF2 <- rbind(withTests,withoutTests)
write_csv(riskDF2,"data/processed/riskDF.csv")

##construct shap
tree_rec2B <- recipe(overOne_2 ~ ., data = trainDF2) %>% 
  update_role(blockNum, new_role="ID") %>% 
  step_integer(all_nominal())
#step_dummy(all_nominal_predictors()) #previously step_integer
baked2 <- bake(
  prep(tree_rec2B), 
  has_role("predictor"),
  new_data = trainDF2, 
  composition = "matrix"
)
library(shapviz)
outcomeShap <- shapviz(myModel2, X_pred = baked2, x=trainDF2)
saveRDS(outcomeShap,file="data/processed/outcomeShap.rds")
write_csv(trainDF2,"data/processed/trainDF2.csv")
