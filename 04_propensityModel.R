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
  ) %>% distinct(blockNum,.keep_all=T)

split_df <- group_initial_split(propensityDF,group=blockNum) #keep blocks in same split
trainDF <- training(split_df)
testDF <- testing(split_df)
tree_rec <- recipe(tested ~ ., data = propensityDF) %>% 
  update_role(blockNum, new_role="ID")




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
tune_res_final <- tune_grid(
  tune_wf,
  resamples = trees_folds3,
  grid = 15
)
tune_res_final %>%
  tune::show_best(metric = "roc_auc",n = 5)

best_tree <- tune_res_final %>%
  select_best("roc_auc")


final_wf <- 
  tune_wf %>% 
  finalize_workflow(best_tree)
saveRDS(final_wf,"data/processed/propensityWF.rds")

final_fit <- 
  final_wf %>%
  last_fit(split_df)


final_fit %>% 
  collect_metrics()

# myModel2 <- extract_fit_engine(final_fit2)
# impObj2 <- lgb.importance(myModel2,percentage = F)


#make calibrated risk predictions
predSplits <- group_initial_split(propensityDF,group=blockNum,prop=1/2)
split1 <- training(predSplits)
split2 <- testing(predSplits)

splitDF1 <- fitSplit(dataSplit = split1, testData=split2,gridNum=10,propensity=T)
splitDF2 <- fitSplit(dataSplit = split2, testData=split1,gridNum=10,propensity=T)

propensityPredsDF <- rbind(splitDF1,splitDF2)

write_csv(propensityPredsDF,"data/processed/propensityPredsDF.csv")


m.out <- matchit(tested ~ .pred_TRUE, data = propensityPredsDF, method = "nearest")
data.matched <- match.data(m.out)
matchedTestedDF <- data.matched %>% filter(tested==T)

m.out2 <- matchit(1-as.logical(tested)~whitePropBG+blackPropBG+asianPropBG+hispanicPropBG+
                    educationHSPropBG+educationBachelorsPropBG+nBlocks+pOld,
                  data=propensityDF,method="nearest",calipter=.15,replace = T)
data.matched2 <- match.data(m.out2)
plot(m.out2, type = "jitter", interactive = FALSE)
matchedTestedDF2 <- data.matched2 %>% filter(tested==T)

riskDF2 <- read_csv("data/processed/riskDF.csv")

riskMatchDF <- riskDF2 %>% 
  mutate(blockNum=factor(blockNum)) %>% 
  filter(blockNum %in% matchedTestedDF2$blockNum)
write_csv(riskMatchDF,"data/processed/riskMatchDF.csv")

riskMatchDF$overOne_2 <- factor(riskMatchDF$overOne_2)
riskMatchDF$predClass <- factor(ifelse(1-riskMatchDF$calibPreds>=.5,TRUE,FALSE))
cmAdj <- conf_mat(riskMatchDF,truth=overOne_2,estimate=predClass)
false_discovery_rateAdj <- cmAdj$table[2,1]/(cmAdj$table[2,2]+cmAdj$table[2,1]) #same as 1-PPV
false_omission_rateAdj <- cmAdj$table[1,2]/(cmAdj$table[1,1]+cmAdj$table[1,2]) #same as 1-NPV


