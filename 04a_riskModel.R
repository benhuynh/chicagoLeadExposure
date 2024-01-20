library(readr)
library(tidymodels)
library(bonsai)
library(probably)
library(lubridate)
source("00_functions.R")
imputeDF <- read_csv("data/processed/imputeDF.csv")
mlDF <- imputeDF %>% filter(tested) %>% 
  select(-tested,-`1st Draw`,-`2-3 Minute`,-`5 Minute`,
         -HHsInternetPropBG,-pCompletePlumbingFacilitiesBG,
         -HHsHasComputerPropBG,-pOccupiedHousesBG,-sequential,
         -pRenterOccupiedHousesBG,-`Date Sampled`) %>%  #remove redundant features
  mutate(overOne_2 = factor(overOne_2),
         censusTract = factor(censusTract),
         CA = factor(CA),
         blockNum = factor(blockNum),
         blockGroup = factor(blockGroup),
         ) 
mlDF_grouped <- mlDF %>% group_by(blockNum) %>% 
  mutate(outcome = mean(as.numeric(overOne_2)-1),
         outcome = factor(ifelse(outcome>=0.5,TRUE,FALSE))) %>% 
  ungroup() %>% distinct(blockNum,.keep_all=T)
mlDF_grouped$overOne_2 <- mlDF_grouped$outcome
mlDF_grouped$outcome <- NULL

# split_df2 <- group_initial_split(mlDF,group=blockNum) #keep blocks in same split
split_df2 <- initial_split(mlDF_grouped)
trainDF2 <- training(split_df2)
testDF2 <- testing(split_df2)
tree_rec2 <- recipe(overOne_2 ~ ., data = trainDF2) %>% 
  update_role(blockNum, new_role="ID")

#lightgbm specification
tune_spec2 <- boost_tree(
  min_n = tune(),
  trees = 1000,
  tree_depth = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm") #xgboost or lightgbm
tune_wf2 <- workflow() %>%
  #add_formula(overOne_2 ~ .-blockNum) %>% 
  add_recipe(tree_rec2) %>% 
  add_model(tune_spec2)


#random forest specification
tune_spec2_rf <- rand_forest(
  trees=1000,mtry=tune()
) %>%   
  set_mode("classification") %>% 
  set_engine("ranger")
tune_wf2_rf <- workflow() %>%
  add_formula(overOne_2 ~ .-blockNum) %>% 
  add_model(tune_spec2_rf)

#elasticnet specification
tune_spec2_enet <- logistic_reg(penalty = tune(),
                               mixture = tune()) %>%
  set_mode("classification") %>% 
  set_engine("glmnet")
tune_wf2_enet <- workflow() %>%
  add_formula(overOne_2 ~ .-blockNum) %>%  #xgb needs formula instead of recipe
  add_model(tune_spec2_enet)

##model comparison
trees_folds3 <- group_vfold_cv(trainDF2,group=blockNum,v=3)
trees_folds_final <- group_vfold_cv(trainDF2,group=blockNum,v=5)
doParallel::registerDoParallel()
set.seed(127)
#lightgbm train
tune_res2 <- tune_grid(
  tune_wf2,
  resamples = trees_folds3,
  grid = 5
)
tune_res2 %>%
  tune::show_best(metric = "roc_auc",n = 5)

#rf train
tune_res2_rf <- tune_grid(
  tune_wf2_rf,
  resamples = trees_folds3,
  grid = 5
)
tune_res2_rf %>%
  tune::show_best(metric = "roc_auc",n = 5)

#glmnet train
tune_res2_enet <- tune_grid(
  tune_wf2_enet,
  resamples = trees_folds3,
  grid = 5
)
tune_res2_enet %>%
  tune::show_best(metric = "roc_auc",n = 5)

##retune using 5-fold + more exhaustive grid search
tune_res_final2 <- tune_grid(
  tune_wf2,
  resamples = trees_folds3,
  grid = 15
)
tune_res_final2 %>%
  tune::show_best(metric = "roc_auc",n = 5)

best_tree2 <- tune_res2 %>%
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
#predSplits <- group_initial_split(mlDF,group=blockNum,prop=1/2)
predSplits <- initial_split(mlDF_grouped,prop=1/2)
split1 <- training(predSplits)
split2 <- testing(predSplits)

splitDF1 <- fitSplit(dataSplit = split1, testData=split2,gridNum=10)
splitDF2 <- fitSplit(dataSplit = split2, testData=split1,gridNum=10)

riskDF <- rbind(splitDF1,splitDF2)

write_csv(riskDF,"data/processed/riskDF_pt1.csv")

#to-do: make grouped prediction code below
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
imputeDF_grouped <- imputeDF %>% group_by(blockNum) %>% 
  mutate(outcome = mean(as.numeric(overOne_2)-1),
         outcome = factor(ifelse(outcome>=0.5,TRUE,FALSE))) %>% 
  ungroup() %>% distinct(blockNum,.keep_all=T)
imputeDF_grouped$overOne_2 <- imputeDF_grouped$outcome
imputeDF_grouped$outcome <- NULL

propensity_train <- imputeDF_grouped %>% filter(tested==T) %>% 
  select(-tested)
propensity_test <- imputeDF_grouped %>% filter(tested==F) %>% 
  select(-tested)


withoutTestsPreds <- fitSplit(dataSplit = propensity_train,
                              testData=propensity_test,gridNum=10)


withoutTests <- withoutTestsPreds %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds,overOne_2)
withTests <- riskDF %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds,overOne_2)
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


#baseline ML predictions
baselineTrainDF2 <- trainDF2 %>% group_by(blockGroup) %>% 
  mutate(bgMean = mean(as.numeric(overOne_2)),
         bgMean = ifelse(bgMean >=0.5,TRUE,FALSE)) %>% 
  ungroup() %>% 
  group_by(censusTract) %>% 
  mutate(tractMean = mean(as.numeric(overOne_2)),
         tractMean = ifelse(tractMean >=0.5,TRUE,FALSE)) %>% 
  ungroup() %>% 
  group_by(CA) %>% 
  mutate(caMean = mean(as.numeric(overOne_2)),
         caMean = ifelse(caMean >=0.5,TRUE,FALSE))
  
baselineTrainDF3 <- baselineTrainDF2 %>% select(blockGroup,censusTract,CA,
                                                bgMean,tractMean,caMean)

#grouped ML test
#make calibrated risk predictions
mlDF_grouped$overOne_2 <- mlDF_grouped$outcome
predSplits <- initial_split(mlDF_grouped,prop=1/2)
split1 <- training(predSplits)
split2 <- testing(predSplits)

splitDF1 <- fitSplit(dataSplit = split1, testData=split2,gridNum=5)
splitDF2 <- fitSplit(dataSplit = split2, testData=split1,gridNum=5)

riskDF_grouped <- rbind(splitDF1,splitDF2)

write_csv(riskDF_grouped,"data/processed/riskDF_pt1_grouped.csv")

#make outcome predictions on blocks without tests
imputeDF <- read_csv("data/processed/imputeDF.csv")
imputeDF_grouped <- imputeDF %>%
  select(-`1st Draw`,-`2-3 Minute`,-`5 Minute`,
         -HHsInternetPropBG,-pCompletePlumbingFacilitiesBG,
         -HHsHasComputerPropBG,-pOccupiedHousesBG,-sequential,
         -pRenterOccupiedHousesBG,-`Date Sampled`) %>%  #remove redundant features
  mutate(overOne_2 = factor(overOne_2),
         censusTract = factor(censusTract),
         CA = factor(CA),
         blockNum = factor(blockNum),
         blockGroup = factor(blockGroup)) %>% group_by(blockNum) %>% 
  mutate(outcome = mean(as.numeric(overOne_2)-1),
         outcome = factor(ifelse(outcome>=0.5,TRUE,FALSE))) %>% 
  ungroup() %>% distinct(blockNum,.keep_all=T)
imputeDF_grouped$overOne_2 <- imputeDF_grouped$outcome
imputeDF_grouped$outcome <- NULL
propensity_train <- imputeDF_grouped %>% filter(tested==T) %>% 
  select(-tested)
propensity_test <- imputeDF_grouped %>% filter(tested==F) %>% 
  select(-tested)


withoutTestsPreds <- fitSplit(dataSplit = propensity_train,
                              testData=propensity_test,gridNum=5)


withoutTests_grouped <- withoutTestsPreds %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds,overOne_2)
withTests_grouped <- riskDF_grouped %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds,overOne_2)
riskDF2_grouped <- rbind(withTests_grouped,withoutTests_grouped)
write_csv(riskDF2_grouped,"data/processed/riskDF_grouped.csv")

riskDF2_grouped$tested <- !is.na(riskDF2_grouped$overOne_2)
riskDF2_grouped$predClass <- ifelse((1-riskDF2_grouped$calibPreds)>0.5,TRUE,FALSE)

cmDF_grouped <- riskDF2_grouped %>% filter(!is.na(overOne_2))
cmDF_grouped$predClass <- factor(cmDF_grouped$predClass)
cmDF_grouped$overOne_2 <- factor(cmDF_grouped$overOne_2,levels=c("FALSE","TRUE"))

cm_grouped <- conf_mat(cmDF_grouped,truth=overOne_2,estimate=predClass)
false_discovery_rate <- cm_grouped$table[2,1]/(cm_grouped$table[2,2]+cm_grouped$table[2,1]) #same as 1-PPV
false_omission_rate <- cm_grouped$table[1,2]/(cm_grouped$table[1,1]+cm_grouped$table[1,2]) #same as 1-NPV

falseNegatives <- riskDF2_grouped %>% 
  filter(predClass=="FALSE") %>% nrow() * false_omission_rate
falsePositives <- riskDF2_grouped %>% 
  filter(predClass=="TRUE") %>% nrow() * false_discovery_rate
(riskDF2_grouped %>% filter(predClass=="TRUE") %>% nrow() + falseNegatives-
    falsePositives)/nrow(riskDF2_grouped)


riskDF2_grouped %>% filter(tested) %>%  filter(predClass=="TRUE") %>% nrow() /
  riskDF2_grouped %>% filter(tested) %>% nrow()
riskDF2_grouped %>% filter(!tested) %>%  filter(predClass=="TRUE") %>% nrow() /
  riskDF2_grouped %>% filter(!tested) %>% nrow() 
