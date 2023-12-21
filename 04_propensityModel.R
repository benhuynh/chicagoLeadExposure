library(readr)
library(tidymodels)
library(bonsai)
library(probably)
library(lubridate)
source("00_functions.R")
imputeDF <- read_csv("data/processed/imputeDF.csv")
riskDF2 <- read_csv("data/processed/riskDF.csv")

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

propensityPredsDF <- read_csv("data/processed/propensityPredsDF.csv")

m.out1 <- matchit(1-as.logical(tested)~.pred_TRUE,
                  data=propensityPredsDF,method="nearest",caliper=.001,replace = T)
data.matched1 <- match.data(m.out1)
matchedTestedDF1 <- data.matched1 %>% filter(tested==T)


m.out2 <- matchit(1-as.logical(tested)~whitePropBG+blackPropBG+asianPropBG+hispanicPropBG+
                    educationHSPropBG+educationBachelorsPropBG+nBlocks+pOld,
                  data=propensityDF,method="nearest",caliper=.15,replace = T)
data.matched2 <- match.data(m.out2)
plot(m.out2, type = "jitter", interactive = FALSE)
matchedTestedDF2 <- data.matched2 %>% filter(tested==T)


prevEstimate <- getPrevalenceEstimate(matchedTestedDF2,riskDF2)


testedDFC <- imputeDF %>% filter(tested) %>% 
  group_by(blockNum) %>% 
  mutate(outcomeCheck = mean(overOne_2), 
         outcheck = ifelse((outcomeCheck != 0 & outcomeCheck != 1),TRUE,FALSE),
         blockOutcome = ifelse(outcomeCheck >= .5,TRUE,FALSE)) %>% ungroup()
testedDFC2 <- testedDFC %>% distinct(blockNum,.keep_all=T)
table(testedDFC2$blockOutcome)
testedDFC$overOne_2 <- factor(testedDFC$overOne_2)
testedDFC$blockOutcome <- factor(testedDFC$blockOutcome)



##population weighted testing
sum(simDF$predClass*simDF$blockPopulation)/sum(simDF$blockPopulation)
testedSimDF <- simDF %>% filter(tested)
sum(testedSimDF$predClass*testedSimDF$blockPopulation)/sum(testedSimDF$blockPopulation)
untestedSimDF <- simDF %>% filter(!tested)
sum(untestedSimDF$predClass*untestedSimDF$blockPopulation)/sum(untestedSimDF$blockPopulation)

