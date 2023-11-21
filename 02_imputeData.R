#Purpose:imputes missing data
library(readr)
library(dplyr)
library(stringr)
library(tidymodels)
library(mice)
library(naniar)

tractDF <- read_csv("data/processed/tractDF.csv")
buildingAge2 <- read_csv("data/processed/buildingAge2.csv")
leadExposure <- read_csv("data/processed/leadExposure_final.csv")
communityAreaDF <- read_csv("data/processed/communityAreaData.csv")
ilBlockPopCleaned <- read_csv("data/processed/ilBlockPopCleaned.csv")
bgDF <- read_csv("data/processed/blockGroupData_rfImputed_cleaned.csv")
chicagoBlocks <- read_csv("data/processed/chicagoBlocks.csv")

ilBlockPopCleaned$GEOID <- as.character(ilBlockPopCleaned$GEOID)
ilBlockPopCleaned$NAME <- NULL
bgDF$GEOID <- as.character(bgDF$GEOID)
bgDF$Name <- NULL
buildingAge2$blockNum <- as.character(buildingAge2$blockNum)
chicagoBlocks2 <- chicagoBlocks %>% 
  mutate(blockNum = as.character(GEOID),
    blockGroup = str_sub(GEOID,start=1,end=12)) %>% 
  rename(censusTract = GEOID20) %>% 
  mutate(censusTract = as.character(censusTract))

leadExposure2 <- leadExposure %>% 
  mutate(blockGroup = paste0(CensusTract,str_sub(cxy_block_id,start=1,end=1)),
         blockNum = paste0(CensusTract,cxy_block_id),
         censusTract = str_sub(blockNum,start=1,end=11),
         overOne_2 = `2-3 Minute`>=1) %>% 
  select(blockGroup,blockNum,censusTract,`Date Sampled`,overOne_2,sequential,`1st Draw`,
         `2-3 Minute`, `5 Minute`) %>% 
  mutate(`5 Minute` = ifelse(sequential & `5 Minute`==0,`2-3 Minute`,`5 Minute`))
tractDF$GEOID <- as.character(tractDF$GEOID)

chicagoBlocks3 <- chicagoBlocks2 %>% 
  left_join(buildingAge2) %>% #~5k missing
  left_join(communityAreaDF %>% select(COMMUNIT_1,CensusTract,28,30,32,34,36,38)) %>% 
  left_join(tractDF,by=c("censusTract"="GEOID")) %>% 
  left_join(bgDF, by=c("blockGroup"="GEOID")) %>% 
  filter(totalPopulationBG > 0) %>% 
  filter(blockPopulation > 0) %>%
  select(-COMMUNIT_1,-NAME,-GEOID,-Longitude,-Latitude,-CensusTract)
set.seed(101)
miceOb0 <- mice(chicagoBlocks3,method="",maxit=0)#blank imputation to get predictor matrix
predmat = miceOb0$predictorMatrix
predmat[,1:2] <- 0 #censusTract and CA
predmat[,10] <- 0 #blockGroup


miceObRF <- mice(chicagoBlocks3,predictorMatrix=predmat,method="rf")
completedDFRF <- complete(miceObRF)
completedDFRF$medianYearStructureBuiltBG[completedDFRF$medianYearStructureBuiltBG==0] <- NA 
completedDFRF$medianYearStructureBuiltBG[is.na(completedDFRF$medianYearStructureBuiltBG)] <- median(completedDFRF$medianYearStructureBuiltBG,na.rm=T)
completedDFRF$medianBuildingAgeBG <- 2023-completedDFRF$medianYearStructureBuiltBG
completedDFRF$medianYearStructureBuiltBG <- NULL

write_csv(completedDFRF,"data/processed/completedDFRF.csv")
completedDFRF <- read_csv("data/processed/completedDFRF.csv")
completedDFRF$blockNum <- as.character(completedDFRF$blockNum)
completedDFRF$blockGroup <- as.character(completedDFRF$blockGroup)
completedDFRF$censusTract <- as.character(completedDFRF$censusTract)


chicagoBlocks4 <- completedDFRF %>% 
  left_join(leadExposure2,by=c("blockNum"="blockNum",
                               "blockGroup"="blockGroup",
                               "censusTract"="censusTract")) %>%
  mutate(tested = !is.na(overOne_2))
  
  


write_csv(chicagoBlocks4,"data/processed/imputeDF.csv")


