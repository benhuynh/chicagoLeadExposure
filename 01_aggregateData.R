#Purpose: Aggregates lead exposure data with building age and census/ACS data
library(readxl)
library(stringr)
library(dplyr)
library(censusxy)
library(rusps)
library(XML)
library(readr)
library(tidycensus)

leadDF <- read.csv("data/Chicago Health Atlas Data Download - Community areas.csv",header=T)[-1,]
tractsToCA <- read_csv("data/2020 Census Tracts to Chicago Community Area Equivalency File - Sheet1.csv")
buildings <- read_csv("data/buildings.csv")
buildings$the_geom <- NULL
leDF <- read_csv("data/processed/leadExposure_complete.csv")


#clean up census tract IDs, add community areas
aggDF <- leDF %>%
  filter(!is.na(cxy_tract_id)) %>% 
  mutate(cxy_county_id = "031",
         cxy_tract_id = ifelse(str_length(cxy_tract_id)==5,
                               paste0("0",cxy_tract_id),
                               cxy_tract_id), #re-add leading zero
         CensusTract = as.numeric(paste0(cxy_state_id,cxy_county_id,cxy_tract_id))) %>% 
  left_join(tractsToCA,by=c("CensusTract"="GEOID20"))

print(paste0(
  sum(is.na(aggDF$`2-3 Minute`)),
  " NA values in lead concentration."))
print(paste0(
  sum(is.na(aggDF$COMMUNIT_1)),
  " NA values in community area."
  ))


aggDF2 <- aggDF %>% 
  filter(!is.na(COMMUNIT_1)) %>% 
  filter(!is.na(`2-3 Minute`)) %>%
  mutate(across(11:13, as.numeric),
         across(11:13, ~ ifelse(is.na(.x),"0",.x))) #introduce NAs for "<1", zero them
aggDF3 <- aggDF2 %>% 
  group_by(COMMUNIT_1) %>% 
  mutate(over1 = (as.numeric(`1st Draw`) >= 1),
         sumOver1 = sum(over1),
         n = n(),
         rate = sumOver1/n,
         mean=mean(as.numeric(`2-3 Minute`))) %>% 
  ungroup() %>%
  distinct(COMMUNIT_1,.keep_all=T) %>% 
  left_join(leadDF,
            by=c("COMMUNIT_1"="Name"))

write_csv(aggDF2,"data/processed/leadExposure_final.csv")
write_csv(aggDF3,"data/processed/communityAreaData.csv")

buildings2 <- buildings %>% filter(!is.na(ST_NAME1)) %>% 
  select(T_ADD1,PRE_DIR1,ST_NAME1,ST_TYPE1,YEAR_BUILT) %>% 
  mutate(address = paste(T_ADD1,PRE_DIR1,ST_NAME1,ST_TYPE1)) %>% 
  filter(YEAR_BUILT>0)

preDF <- data.frame(buildings2$address,"Chicago","IL","")
colnames(preDF) <- c("Address","City","State","Zip")

#geocoding all blocks, this may take a while
gcDF <- cxy_geocode(preDF, street = "Address", 
                    city = "City", 
                    state = "State", 
                    zip = "Zip", 
                    class = "dataframe",
                    return="geographies",
                    vintage="Current_Current")
gcDF$year <- buildings2$YEAR_BUILT

write_csv(gcDF,"data/processed/buildingAge.csv")
buildingAge <- read_csv("data/processed/buildingAge.csv")
buildingAge$age <- 2022-buildingAge$year
buildingAge2 <- buildingAge %>% 
  filter(!is.na(cxy_block_id)) %>% 
  mutate(blockNum = paste0("17031",cxy_tract_id,cxy_block_id)) %>% 
  group_by(blockNum) %>% 
  summarize(medianAge = median(age),
            meanAge = mean(age),
            nBlocks = n(), #nBuildings
            minAge = min(age),
            maxAge = max(age),
            pOld = length(age[age > 36])/nBlocks)
write_csv(buildingAge2,"data/processed/buildingAge2.csv")



ilBlockPop <- get_decennial(geography="block",
              variables=c("P1_001N",
                          "P1_003N",
                          "P1_004N",
                          "P1_005N",
                          "P1_006N",
                          "P2_002N"),
              year=2020,
              state="IL",output="wide")
ilBlockPop <- ilBlockPop %>% 
  rename(blockPopulation = P1_001N,
         whiteBlockPop = P1_003N,
         blackBlockPop = P1_004N,
         aianBlockPop = P1_005N,
         asianBlockPop = P1_006N,
         hispanicBlockPop = P2_002N)
write_csv(ilBlockPop,"data/processed/ilBlockPop.csv")
ilBlockPop <- read_csv("data/processed/ilBlockPop.csv")

library(dplyr)

ilBlockPopCleaned <- ilBlockPop %>%
  mutate(propWhiteBlockPop = whiteBlockPop/blockPopulation,
         propBlackBlockPop = blackBlockPop/blockPopulation,
         propAianBlockPop = aianBlockPop/blockPopulation,
         propAsianBlockPop = asianBlockPop/blockPopulation,
         propHispanicBlockPop = hispanicBlockPop/blockPopulation) %>%
  select(-whiteBlockPop, -blackBlockPop, -aianBlockPop, -asianBlockPop, -hispanicBlockPop)
write_csv(ilBlockPopCleaned,"data/processed/ilBlockPopCleaned.csv")
ilBlockPopCleaned2 <- ilBlockPopCleaned %>% 
  mutate(GEOID_11 = substr(GEOID, 1, 11))
tractsToCA$GEOID20 <- as.character(tractsToCA$GEOID20)
chicagoBlocks <- tractsToCA %>% 
  left_join(ilBlockPopCleaned2,by=c("GEOID20"="GEOID_11")) %>% 
  filter(blockPopulation > 0)
write_csv(chicagoBlocks,"data/processed/chicagoBlocks.csv")

census_vars <- load_variables(2021, "acs5", cache = FALSE)
acsVars <- c("B02008_001","B02009_001","B02010_001","B02011_001",
"B02014_001","B03001_003","B03003_003","B01001_001","B15003_017",
"B15003_018","B15003_019","B15003_020","B15003_021","B15003_022",
"B15003_023","B15003_024","B15003_025","B17010_002","B19013_001",
"B25001_001","B25002_002","B25002_003","B25003_002","B25003_003",
"B25035_001","B25047_002","B25047_003","B25057_001","B25058_001",
"B25059_001","B25064_001","B25076_001","B25077_001","B25078_001",
"B25079_001","B25081_002","B25081_009","B25081_001","B25088_001",
"B28001_011","B28002_002","B28002_013","B28003_002","B28003_001",
"B99051_002","B99051_005","B99162_002","B99162_003","B01001_001",
"B01001_002","B01001_003","B01001_004","B01001_005","B01001_006",
"B01001_026","B01001_027","B01001_028","B01001_029","B01001_030")
acsData <- get_acs(geography="block group",
                  year=2021,
                  state="IL",
                  variables=acsVars,
                  output="wide")
write_csv(acsData,"data/processed/originalBlockGroupData.csv")
acsData <- read_csv("data/processed/originalBlockGroupData.csv")

acsData2 <- acsData[,!grepl("M$", names(acsData))]
colnames(acsData2)[3:length(colnames(acsData2))] <- gsub("E|M", "", colnames(acsData2)[3:length(colnames(acsData2))])
census_vars$conceptLabel <- paste0(census_vars$concept,"_",census_vars$label)
new_names <- census_vars$conceptLabel[match(names(acsData2), census_vars$name)]
colnames(acsData2) <- ifelse(is.na(new_names), names(acsData2), new_names)

acsDataMoE <- acsData[,!grepl("E$", names(acsData))]
colnames(acsDataMoE)[3:length(colnames(acsDataMoE))] <- gsub("E|M", "", colnames(acsDataMoE)[3:length(colnames(acsDataMoE))])
new_names2 <- census_vars$conceptLabel[match(names(acsDataMoE), census_vars$name)]
colnames(acsDataMoE) <- ifelse(is.na(new_names2), names(acsDataMoE), new_names2)
acsDataMoE2 <- acsDataMoE %>% select(GEOID,`SEX BY AGE_Estimate!!Total:`,
                                     `SEX BY AGE_Estimate!!Total:!!Female:!!Under 5 years`,
                                     `SEX BY AGE_Estimate!!Total:!!Female:!!5 to 9 years`,
                                     `SEX BY AGE_Estimate!!Total:!!Male:!!Under 5 years`,
                                     `SEX BY AGE_Estimate!!Total:!!Male:!!5 to 9 years`,
                                     )
colnames(acsDataMoE2) <- c("blockGroup","totalPopulation_MOE",
                           "femaleUnder5_MOE","female5to9_MOE",
                           "maleUnder5_MOE","male5to9_MOE")
acsDataMoE2 <- acsDataMoE2 %>%
  mutate(totalUnder5_MOE = sqrt(femaleUnder5_MOE^2+
                                  maleUnder5_MOE^2),
         totalUnder10_MOE = sqrt(femaleUnder5_MOE^2+
                                   maleUnder5_MOE^2+
                                   female5to9_MOE^2+
                                   male5to9_MOE^2)
  )

write_csv(acsData2,"data/processed/blockGroupData.csv")
write_csv(acsDataMoE2,"data/processed/blockGroupMOEData.csv")
acsData2 <- read_csv("data/processed/blockGroupData.csv")
library(naniar)
#remove columns that are mostly missing -- they have substitutes in other columns anyway
acsData2$`AMERICAN INDIAN AND ALASKA NATIVE ALONE FOR SELECTED TRIBAL GROUPINGS_Estimate!!Total:` <- NULL
acsData2$`HISPANIC OR LATINO ORIGIN BY SPECIFIC ORIGIN_Estimate!!Total:!!Hispanic or Latino:` <- NULL
acsData2$`AGGREGATE VALUE (DOLLARS) BY AGE OF HOUSEHOLDER_Estimate!!Aggregate value (dollars):` <- NULL
acsData2$`MEDIAN CONTRACT RENT (DOLLARS)_Estimate!!Median contract rent` <- NULL
acsData2$`MEDIAN GROSS RENT (DOLLARS)_Estimate!!Median gross rent` <- NULL
acsData2$`UPPER CONTRACT RENT QUARTILE (DOLLARS)_Estimate!!Upper contract rent quartile` <- NULL
acsData2$`LOWER CONTRACT RENT QUARTILE (DOLLARS)_Estimate!!Lower contract rent quartile` <- NULL
acsData2$`PRESENCEOFACOMPUTERANDTYPEOFINTERNETSUBSCRIPTIONINHOUSEHOLD_EstimateTotal` <- NULL
sum(is.na(acsData2$`MEDIAN GROSS RENT (DOLLARS)_Estimate!!Median gross rent`))
library(mice)
gg_miss_case(acsData2)
gg_miss_upset(acsData2)


colnames(acsData2) <- gsub("[ :!,'()-]", "", colnames(acsData2))

miceData2 <- mice(acsData2,method="rf")

imputedDF2 <- complete(miceData2)

write_csv(imputedDF2,"data/processed/blockGroupData_rfImputed.csv")

imputedDF2 <- read_csv("data/processed/blockGroupData_rfImputed.csv")
imputedDF2$MORTGAGESTATUS_EstimateTotal <- NULL
imputedDF2$PRESENCEOFACOMPUTERANDTYPEOFINTERNETSUBSCRIPTIONINHOUSEHOLD_EstimateTotal <- NULL
newColNames <- c("GEOID","Name",
                          "whitePopBG","blackPopBG",
                          "aianPopBG","asianPopBG",
                          "hispanicPopBG","totalPopulationBG",
                          "educationHSBG","educationGEDBG",
                          "educationSomeCollegeLess1BG",
                          "educationSomeCollegeMore1BG",
                          "educationAssociatesBG","educationBachelorsBG",
                          "educationMastersBG","educationPrfsnlSchoolBG",
                          "educationDoctorateBG","povertyBG",
                          "medianHouseholdIncomeBG","numHousingUnitsBG",
                          "numOccupiedHousingUnitsBG","numVacantHousingUnitsBG",
                          "numOwnerOccupiedBG","numRenterOccupiedBG",
                          "medianYearStructureBuiltBG","numCompletePlumbingFacilitiesBG",
                          "numLackingCompletePlumbingBG","houseValueLowerQBG",
                          "houseValueMedianBG","houseValueUpperQBG",
                          "numHousingUnitsWithMortgageBG",
                          "numHousingUnitsNoMortgageBG",
                          "medianHomeOwnerCostsBG",
                 "numHHsNoComputerBG",
                 "numHHsInternetBG",
                 "numHHsNoInternetBG","numHHsHasComputerBG",
                 "totalMaleBG","totalMaleUnder5BG",
                 "totalMale5to9BG","totalMale10to14BG",
                 "totalMale15to17BG","totalFemaleBG",
                 "totalFemaleUnder5BG","totalFemale5to9BG",
                 "totalFemale10to14BG","totalFemale15to17BG",
                 "numNativeBornBG","numForeignBornBG",
                 "numSpeakOnlyEnglishBG","numSpeakOtherLanguagesBG")
colnames(imputedDF2) <- newColNames



imputedDF3 <- imputedDF2 %>% 
  mutate(whitePropBG = whitePopBG / totalPopulationBG,
         blackPropBG = blackPopBG / totalPopulationBG,
         aianPropBG = aianPopBG / totalPopulationBG,
         asianPropBG = asianPopBG / totalPopulationBG,
         hispanicPropBG = hispanicPopBG / totalPopulationBG,
         educationHSPropBG = educationHSBG / totalPopulationBG,
         educationGEDPropBG = educationGEDBG / totalPopulationBG,
         educationSomeCollegeLess1PropBG = educationSomeCollegeLess1BG / totalPopulationBG,
         educationSomeCollegeMore1PropBG = educationSomeCollegeMore1BG / totalPopulationBG,
         educationAssociatesPropBG = educationAssociatesBG / totalPopulationBG,
         educationBachelorsPropBG = educationBachelorsBG / totalPopulationBG,
         educationMastersPropBG = educationMastersBG / totalPopulationBG,
         educationPrfsnlSchoolPropBG = educationPrfsnlSchoolBG / totalPopulationBG,
         educationDoctoratePropBG = educationDoctorateBG / totalPopulationBG,
         povertyPropBG = povertyBG / totalPopulationBG,
         nativeBornPropBG = numNativeBornBG / totalPopulationBG,
         foreignBornPropBG = numForeignBornBG / totalPopulationBG,
         speakOnlyEnglishPropBG = numSpeakOnlyEnglishBG / totalPopulationBG,
         speakOtherLanguagesPropBG = numSpeakOtherLanguagesBG / totalPopulationBG,
         HHsNoComputerPropBG = numHHsNoComputerBG / numOccupiedHousingUnitsBG,
         HHsInternetPropBG = numHHsInternetBG / numOccupiedHousingUnitsBG,
         HHsNoInternetPropBG = numHHsNoInternetBG / numOccupiedHousingUnitsBG,
         HHsHasComputerPropBG = numHHsHasComputerBG / numOccupiedHousingUnitsBG,
         pCompletePlumbingFacilitiesBG = numCompletePlumbingFacilitiesBG / numOccupiedHousingUnitsBG,
         pLackingCompletePlumbingBG = numLackingCompletePlumbingBG / numOccupiedHousingUnitsBG,
         pVacantHousesBG = numVacantHousingUnitsBG / numHousingUnitsBG,
         pOccupiedHousesBG = numOccupiedHousingUnitsBG / numHousingUnitsBG,
         pOwnerOccupiedHousesBG = numOwnerOccupiedBG / numOccupiedHousingUnitsBG,
         pRenterOccupiedHousesBG = numRenterOccupiedBG / numOccupiedHousingUnitsBG,
         )
imputedDF3 <- imputedDF3 %>% 
  mutate(pAllChildrenUnder5BG = (totalMaleUnder5BG + totalFemaleUnder5BG) / totalPopulationBG,
         pAllChildrenUnder10BG = (totalMaleUnder5BG + totalFemaleUnder5BG +
                                       totalMale5to9BG + totalFemale5to9BG) / totalPopulationBG,
         pAllChildrenUnder18BG = (totalMaleUnder5BG + totalFemaleUnder5BG +
                                       totalMale5to9BG + totalFemale5to9BG +
                                       totalMale10to14BG + totalFemale10to14BG +
                                       totalMale15to17BG + totalFemale15to17BG) / totalPopulationBG)


imputedDF3 <- imputedDF3 %>% 
  select(-c("totalMaleBG", "totalMaleUnder5BG", "totalMale5to9BG", "totalMale10to14BG",
            "totalMale15to17BG", "totalFemaleBG", "totalFemaleUnder5BG", "totalFemale5to9BG",
            "totalFemale10to14BG", "totalFemale15to17BG")) %>% 
  select(-c("whitePopBG","blackPopBG","aianPopBG","asianPopBG",
            "hispanicPopBG","educationHSBG",
            "educationGEDBG","educationSomeCollegeLess1BG",
            "educationSomeCollegeMore1BG","educationAssociatesBG",
            "educationBachelorsBG","educationMastersBG",
            "educationPrfsnlSchoolBG","educationDoctorateBG",
            "povertyBG","medianHouseholdIncomeBG",
            "numOccupiedHousingUnitsBG","numVacantHousingUnitsBG",
            "numOwnerOccupiedBG","numRenterOccupiedBG",
            "numCompletePlumbingFacilitiesBG",
            "numLackingCompletePlumbingBG",
            "numHousingUnitsWithMortgageBG","numHousingUnitsNoMortgageBG",
            "numHHsNoComputerBG",
            "numHHsInternetBG","numHHsNoInternetBG","numHHsHasComputerBG",
            "numNativeBornBG","numForeignBornBG","numSpeakOnlyEnglishBG",
            "numSpeakOtherLanguagesBG"))

write_csv(imputedDF3,"data/processed/blockGroupData_rfImputed_cleaned.csv")
acsDataMoE3 <- acsDataMoE2 %>% left_join(imputedDF3 %>% select(GEOID,totalPopulationBG,
                                                               pAllChildrenUnder5BG,
                                                               pAllChildrenUnder10BG),
                                         by=c("blockGroup"="GEOID"))
acsDataMoE4 <- acsDataMoE3 %>% 
  mutate(totalChildrenUnder5BG = pAllChildrenUnder5BG*totalPopulationBG,
         totalChildrenUnder10BG = pAllChildrenUnder10BG*totalPopulationBG,
         pAllChildrenUnder5BG_MOE = (1/totalPopulationBG)*
           sqrt(totalUnder5_MOE^2 - totalChildrenUnder5BG^2/
                  totalPopulationBG^2*
                  totalPopulation_MOE^2),
         pAllChildrenUnder5BG_MOE2 = (1/totalPopulationBG)*
           sqrt(totalUnder5_MOE^2 + totalChildrenUnder5BG^2/
                  totalPopulationBG^2*
                  totalPopulation_MOE^2),
         pAllChildrenUnder5BG_MOE = ifelse(totalPopulationBG==0,
                                           0,pAllChildrenUnder5BG_MOE),
         pAllChildrenUnder5BG_MOE = ifelse(is.nan(pAllChildrenUnder5BG_MOE),
                                           pAllChildrenUnder5BG_MOE2,
                                           pAllChildrenUnder5BG_MOE),
         pAllChildrenUnder10BG_MOE = (1/totalPopulationBG)*
           sqrt(totalUnder10_MOE^2 - totalChildrenUnder10BG^2/
                  totalPopulationBG^2*
                  totalPopulation_MOE^2),
         pAllChildrenUnder10BG_MOE2 = (1/totalPopulationBG)*
           sqrt(totalUnder10_MOE^2 + totalChildrenUnder10BG^2/
                  totalPopulationBG^2*
                  totalPopulation_MOE^2),
         pAllChildrenUnder10BG_MOE = ifelse(totalPopulationBG==0,
                                           0,pAllChildrenUnder10BG_MOE),
         pAllChildrenUnder10BG_MOE = ifelse(is.nan(pAllChildrenUnder10BG_MOE),
                                           pAllChildrenUnder10BG_MOE2,
                                           pAllChildrenUnder10BG_MOE),
         ) 
#some NaNs produced due to 0 block group population, will be removed downstream
write_csv(acsDataMoE4,"data/processed/blockGroupMOEData2.csv")



tractDF <- read_csv("data/Chicago Health Atlas Data Download - Census Tracts.csv")
tractDF <- tractDF[-(1:4),]
tractDF$GEOID <- as.numeric(tractDF$GEOID)
tractDF <- tractDF %>% 
  rename(pctW = `PCT-W_2016-2020`,
         pctB = `PCT-B_2016-2020`,
         pctA = `PCT-A_2016-2020`,
         pctH = `PCT-H_2016-2020`,
         pctP = `PCT-P_2016-2020`,
         pctQ = `PCT-Q_2016-2020`) %>% 
  rename_with(~ str_replace(., "_2015-2019","")) %>% 
  rename_with(~ str_replace(., "_2016-2020","")) %>% 
  select(-Layer,-Name,-POP) %>% 
  mutate_if(is.character,as.numeric)
write_csv(tractDF,"data/processed/tractDF.csv")
