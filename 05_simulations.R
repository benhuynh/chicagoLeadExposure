#Purpose: Generates microsimulation data
library(readr)
library(dplyr)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(janitor)

source("00_functions.R")
outcomeWF <- readRDS("data/processed/outcomeWF.rds")
riskDF <- read_csv("data/processed/riskDF.csv")
riskDF_testLevel <- read_csv("data/processed/riskDF_testLevel.csv")

imputeDF <- read_csv("data/processed/imputeDF.csv")
bgMOE <- read_csv("data/processed/blockGroupMOEData2.csv")
tractsToCA <- read_csv("data/2020 Census Tracts to Chicago Community Area Equivalency File - Sheet1.csv")
hcsDF <- read_csv("data/processed/hcsResults.csv")
matchedTestedDF2 <- read_csv("data/processed/matchedTestedDF2.csv")

hcsDF2 <- hcsDF %>% select(CA,percent_Unfiltered,lower_Unfiltered,upper_Unfiltered)

childrenDF <- imputeDF %>% select(blockPopulation,blockGroup,totalPopulationBG,
                                             propWhiteBlockPop,censusTract,Population,
                                             propBlackBlockPop,
                                             propAianBlockPop,
                                             propAsianBlockPop,
                                             propHispanicBlockPop,
                                             whitePropBG,blackPropBG,
                                             aianPropBG,asianPropBG,
                                             hispanicPropBG,pAllChildrenUnder5BG,
                                             pAllChildrenUnder10BG,
                                             censusTract,blockNum,tested,overOne_2) %>% 
  distinct() %>% 
  mutate(pAllChildrenUnder6BG = (pAllChildrenUnder10BG-pAllChildrenUnder5BG)/5+
           pAllChildrenUnder5BG)
childrenDF <- childrenDF %>% 
  left_join(bgMOE %>% select(blockGroup,
                             totalPopulation_MOE,
                             pAllChildrenUnder5BG_MOE,
                             pAllChildrenUnder10BG_MOE)) %>% 
  mutate(pAllChildren6BG_MOE = sqrt(pAllChildrenUnder10BG_MOE^2+pAllChildrenUnder5BG_MOE^2)/5,
         pAllChildrenUnder6BG_MOE = sqrt(pAllChildren6BG_MOE^2+pAllChildrenUnder5BG_MOE^2))
childrenDF$blockNum <- as.character(childrenDF$blockNum)

convertToSimDF <- function(rDF) {
  rDF <- rDF %>% distinct(blockNum,.keep_all=T)
  rDF$blockNum <- as.character(rDF$blockNum)
  simDF <- childrenDF %>% left_join(rDF,by=c("blockNum"="blockNum")) %>% 
    distinct(blockNum,.keep_all=T)
  simDF <- simDF %>% left_join(tractsToCA,by=c("censusTract"="GEOID20")) %>% 
    left_join(hcsDF2,by=c("CA"="CA"))
  simDF$calibPreds <- 1-simDF$calibPreds #originally predicting FALSE, switch signs
  simDF$preds <- 1-simDF$preds
  simDF$predClass <- simDF$preds>=.5
  return(simDF)
}

simDF <- convertToSimDF(riskDF)
write_csv(simDF,"data/processed/simDF.csv")
simDF_testLevel <- convertToSimDF(riskDF_testLevel)
write_csv(simDF_testLevel,"data/processed/simDF_testLevel.csv")


concDF <- read_csv("data/processed/imputeDF.csv") %>% filter(tested)
concDF2 <- concDF %>% select(blockGroup,censusTract,CA,blockNum,`1st Draw`,
                             `2-3 Minute`, `5 Minute`,LDPP_2021) %>% 
  filter(`1st Draw` > 0,
         `2-3 Minute` > 0,
         `5 Minute` > 0)
concDF3 <- concDF2 %>% group_by(blockGroup) %>% 
  mutate(concList = list(unlist(c(`1st Draw`,
                                  `2-3 Minute`,
                                  `5 Minute`)))) %>%  
  select(blockGroup,concList) %>% distinct()
concDF4 <- concDF2 %>% group_by(censusTract) %>% 
  mutate(concTractList = list(unlist(c(`1st Draw`,
                                       `2-3 Minute`,
                                       `5 Minute`)))) %>% 
  select(censusTract,concTractList) %>% distinct()
concDF5 <- concDF2 %>% group_by(CA) %>% 
  mutate(concCAList = list(unlist(c(`1st Draw`,
                                       `2-3 Minute`,
                                       `5 Minute`)))) %>% 
  select(CA,concCAList,LDPP_2021) %>% distinct(CA,concCAList,.keep_all=T)

convertToSimDF2 <- function(sDF) {
  sDF2 <- sDF %>% 
    left_join(concDF3, by=c("blockGroup"="blockGroup")) %>% 
    left_join(concDF4, by=c("censusTract"="censusTract")) %>%
    left_join(concDF5, by=c("CA"="CA")) %>%
    mutate(concList = ifelse(blockGroup %in% concDF2$blockGroup,
                             concList,concTractList),
           concList = ifelse(censusTract %in% concDF2$censusTract,
                             concList,concCAList))
  print(paste0(sum(!(sDF2$blockGroup %in% concDF2$blockGroup))," block groups aren't represented in the lead screening data.")) #595 BGs in Chicago aren't represented in the concentration dataset -- need to use census tract instead
  print(paste0(sum(!(sDF2$censusTract %in% concDF2$censusTract))," tracts aren't represented in the lead screening data.")) #41 BGs in Chicago aren't represented in the concentration dataset -- need to use census tract instead
  print(paste0(sum(!(sDF2$CA %in% concDF2$CA))," CAs aren't represented in the lead screening data.")) #0 CAs in Chicago aren't represented in the concentration dataset
  sDF2$totalPopulation_MOE95 <- sDF2$totalPopulation_MOE/qnorm(.95)*qnorm(.975)
  sDF2$pAllChildrenUnder6BG_MOE95 <- sDF2$pAllChildrenUnder6BG_MOE/qnorm(.95)*qnorm(.975)
  
  return(sDF2)
}
simDF2 <- convertToSimDF2(simDF)
simDF2_testLevel <- convertToSimDF2(simDF_testLevel)



concentrationSampVec <- t(sapply(simDF2$concList,sample,10000,T))
concentrationSampVec <- ifelse(concentrationSampVec>10,10,concentrationSampVec)

#convert MoEs from 90% CIs to 95% CIs

metricVec <- getPrevalenceEstimate(matchedTestedDF2,riskDF,output="metrics")
metricVec_testLevel <- getPrevalenceEstimate(matchedTestedDF2,riskDF_testLevel,output="metrics")



#% of blocks predicted as having lead > 1 ppb
nrow(simDF %>% filter(predClass))/nrow(simDF)
#% of tests that have lead >1 ppb
(imputeDF %>% filter(tested,overOne_2==TRUE) %>% nrow())/
  (imputeDF %>% filter(tested) %>% nrow())
#number of blocks predicted as positive adjusted for training-set FDR and FOR
(simDF %>% filter(predClass) %>% nrow() + falseNegatives-
    falsePositives)/nrow(simDF)
#number of blocks predicted as positive adjusted for FDR and FOR, split by tested/untested distributions
(simDF %>% filter(predClass) %>% nrow() + falseNegativesAdj-
    falsePositivesAdj)/nrow(simDF)


simMatDF <- simFunc(simDF=simDF2,errorMetrics=metricVec)
write_csv(simMatDF,paste0("data/processed/simResults_",now(),".csv"))

simMatDF_classif <- simFunc(simDF=simDF2,errorMetrics=metricVec,calib="classification")
write_csv(simMatDF,paste0("data/processed/simResults_classif_",now(),".csv"))

simMatDF_testLevel <- simFunc(simDF=simDF2_testLevel,errorMetrics=metricVec_testLevel,calib="classification")
write_csv(simMatDF_testLevel,paste0("data/processed/simResults_testLevel_",now(),".csv"))

simMatDF_bll_unadj <- simFunc(simDF=simDF2,adjustBLL=F,calib="classification",errorMetrics=metricVec)
write_csv(simMatDF_bll_unadj,paste0("data/processed/simResults_bll_unadj_",now(),".csv"))

summary(simMatDF)

simAggTable2 <- generateSimAggTable(simMatDF)
simAggTable_classif <- generateSimAggTable(simMatDF_classif)


simAggTable_bll_unadj <- generateSimAggTable(simMatDF_bll_unadj)
simAggTable_testLevel <- generateSimAggTable(simMatDF_testLevel)


write_csv(simAggTable2,paste0("data/processed/simAggTable_",now(),".csv"))
write_csv(simAggTable_classif,paste0("data/processed/simAggTable_classif_",now(),".csv"))
write_csv(simAggTable_bll_unadj,paste0("data/processed/simAggTable_bll_unadj_",now(),".csv"))
write_csv(simAggTable_testLevel,paste0("data/processed/simAggTable_testLevel_",now(),".csv"))


#calculate block-level outcomes
testedDFC <- testedDF %>% 
  group_by(blockNum) %>% 
  mutate(outcomeCheck = mean(overOne_2), 
         outcheck = ifelse((outcomeCheck != 0 & outcomeCheck != 1),TRUE,FALSE),
         blockOutcome = ifelse(outcomeCheck >= .5,TRUE,FALSE)) %>% ungroup()
testedDFC2 <- testedDFC %>% distinct(blockNum,.keep_all=T)
table(testedDFC2$blockOutcome)
testedDFC$overOne_2 <- factor(testedDFC$overOne_2)
testedDFC$blockOutcome <- factor(testedDFC$blockOutcome)

cmDFC <- conf_mat(testedDFC,truth=overOne_2,estimate=blockOutcome)
roc_auc(testedDFC,truth=overOne_2,estimate=outcomeCheck)

