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
propensitiesDF <- read_csv("data/processed/imputeDF.csv")
bgMOE <- read_csv("data/processed/blockGroupMOEData2.csv")
tractsToCA <- read_csv("data/2020 Census Tracts to Chicago Community Area Equivalency File - Sheet1.csv")
hcsDF <- read_csv("data/processed/hcsResults.csv")

hcsDF2 <- hcsDF %>% select(CA,percent_Unfiltered,lower_Unfiltered,upper_Unfiltered)

childrenDF <- propensitiesDF %>% select(blockPopulation,blockGroup,totalPopulationBG,
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
riskDF <- riskDF %>% distinct()
riskDF$blockNum <- as.character(riskDF$blockNum)
childrenDF$blockNum <- as.character(childrenDF$blockNum)
simDF <- childrenDF %>% left_join(riskDF,by=c("blockNum"="blockNum")) %>% 
  distinct(blockNum,.keep_all=T)
simDF <- simDF %>% left_join(tractsToCA,by=c("censusTract"="GEOID20")) %>% 
  left_join(hcsDF2,by=c("CA"="CA"))
simDF$calibPreds <- 1-simDF$calibPreds #originally predicting FALSE, switch signs
simDF$preds <- 1-simDF$preds
simDF$predClass <- simDF$preds>=.5

concDF <- read_csv("data/processed/imputeDF.csv") %>% filter(tested) #switch with leDF?
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

simDF2 <- simDF %>% 
  left_join(concDF3, by=c("blockGroup"="blockGroup")) %>% 
  left_join(concDF4, by=c("censusTract"="censusTract")) %>%
  left_join(concDF5, by=c("CA"="CA")) %>%
  mutate(concList = ifelse(blockGroup %in% concDF2$blockGroup,
                           concList,concTractList),
         concList = ifelse(censusTract %in% concDF2$censusTract,
                           concList,concCAList))

print(paste0(sum(!(simDF2$blockGroup %in% concDF2$blockGroup))," block groups aren't represented in the lead screening data.")) #595 BGs in Chicago aren't represented in the concentration dataset -- need to use census tract instead
print(paste0(sum(!(simDF2$censusTract %in% concDF2$censusTract))," tracts aren't represented in the lead screening data.")) #41 BGs in Chicago aren't represented in the concentration dataset -- need to use census tract instead
print(paste0(sum(!(simDF2$CA %in% concDF2$CA))," CAs aren't represented in the lead screening data.")) #0 CAs in Chicago aren't represented in the concentration dataset


concentrationSampVec <- t(sapply(simDF2$concList,sample,10000,T))
concentrationSampVec <- ifelse(concentrationSampVec>10,10,concentrationSampVec)

length(simDF2$LDPP_2021[simDF2$LDPP_2021==0]) #821 blocks (3 CAs) with 0 LDPP
meansVec <- 5-SD*qnorm(1-simDF2$LDPP_2021/100) #calculate new means for each CA, keeping SD constant
meansVec <- ifelse(is.infinite(meansVec),NA,meansVec)
meansVec <- ifelse(is.na(meansVec),min(meansVec,na.rm=T)/2,meansVec)

#convert MoEs from 90% CIs to 95% CIs
simDF2$totalPopulation_MOE95 <- simDF2$totalPopulation_MOE/qnorm(.95)*qnorm(.975)
simDF2$pAllChildrenUnder6BG_MOE95 <- simDF2$pAllChildrenUnder6BG_MOE/qnorm(.95)*qnorm(.975)

simMatDF <- simFunc(simDF=simDF2,meansVec=meansVec)
write_csv(simMatDF,paste0("data/processed/simResults_",now(),".csv"))
simMatDF_bll_unadj <- simFunc(simDF=simDF2,meansVec=meansVec,adjustBLL=F)
write_csv(simMatDF_bll_unadj,paste0("data/processed/simResults_bll_unadj_",now(),".csv"))
simMatDF_uncalib <- simFunc(simDF=simDF2,meansVec=meansVec,calib=F)
write_csv(simMatDF_uncalib,paste0("data/processed/simResults_uncalib_",now(),".csv"))

summary(simMatDF)

simAggTable2 <- generateSimAggTable(simMatDF)
simAggTable_uni <- generateSimAggTable(simMatDF_uni)
simAggTable_bll_unadj <- generateSimAggTable(simMatDF_bll_unadj)
simAggTable_uncalib <- generateSimAggTable(simMatDF_uncalib)


write_csv(simAggTable2,paste0("data/processed/simAggTable_",now(),".csv"))
write_csv(simAggTable_uni,paste0("data/processed/simAggTable_uni_",now(),".csv"))
write_csv(simAggTable_bll_unadj,paste0("data/processed/simAggTable_bll_unadj_",now(),".csv"))
write_csv(simAggTable_uncalib,paste0("data/processed/simAggTable_uncalib_",now(),".csv"))

