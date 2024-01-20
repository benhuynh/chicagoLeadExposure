library(readr)
library(tidymodels)
library(bonsai)
library(probably)
library(lubridate)
source("00_functions.R")
imputeDF <- read_csv("data/processed/imputeDF.csv")

#different draws
testDF_1 <- getDrawPredictions(imputeDF,draw=1)
testDF_5 <- getDrawPredictions(imputeDF,draw=5)

#test-level ML training
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

split_df2 <- group_initial_split(mlDF,group=blockNum) #keep blocks in same split
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

final_fit2 <- 
  final_wf2 %>%
  last_fit(split_df2)

final_fit2 %>% 
  collect_metrics()


#make calibrated risk predictions
predSplits <- group_initial_split(mlDF,group=blockNum,prop=1/2)
split1 <- training(predSplits)
split2 <- testing(predSplits)

splitDF1 <- fitSplit(dataSplit = split1, testData=split2,gridNum=10)
splitDF2 <- fitSplit(dataSplit = split2, testData=split1,gridNum=10)

riskDF <- rbind(splitDF1,splitDF2)

write_csv(riskDF,"data/processed/riskDF_pt1_testLevel.csv")

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
                              testData=propensity_test,gridNum=10)


withoutTests <- withoutTestsPreds %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds,overOne_2)
withTests <- riskDF %>% 
  rename(preds = rawPreds) %>% 
  select(blockNum,preds,calibPreds,overOne_2)
riskDF2 <- rbind(withTests,withoutTests)
write_csv(riskDF2,"data/processed/riskDF_testLevel.csv")
