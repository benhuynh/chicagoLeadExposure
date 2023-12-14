#calculates summary statistics of lead data, plots seasonality
library(readr)
library(lubridate)
library(ggplot2)
imputeDF <- read_csv("data/processed/imputeDF.csv")
imputeDF$month <- month(as.Date(imputeDF$`Date Sampled`))
testedDF <- imputeDF %>% filter(tested)
untestedDF <- imputeDF %>% filter(!tested)
plot(density(testedDF$month))

plotDF <- testedDF %>% group_by(month) %>%
  mutate(overOne = ifelse(`2-3 Minute` > 0,1,0)) %>% 
  summarize(med = median(`2-3 Minute`),
            upper = quantile(`2-3 Minute`,.75),
            lower = quantile(`2-3 Minute`,.25),
            rate = sum(overOne)/n()
            )
plotDF$month <- factor(plotDF$month, labels = month.abb)

ggplot(plotDF, aes(x=month,y=med,group=1)) +
  geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.2) +
  theme_classic()

ggplot(plotDF, aes(x=month,y=rate,group=1)) +
  geom_line() + 
  theme_classic()

sum(testedDF$`1st Draw` >= 1)/nrow(testedDF)
sum(testedDF$`1st Draw` >= 5)/nrow(testedDF)
sum(testedDF$`1st Draw` >= 15)/nrow(testedDF)

sum(testedDF$`2-3 Minute` >= 1)/nrow(testedDF)
sum(testedDF$`2-3 Minute` >= 5)/nrow(testedDF)
sum(testedDF$`2-3 Minute` >= 15)/nrow(testedDF)

sum(testedDF$`5 Minute` >= 1)/nrow(testedDF)
sum(testedDF$`5 Minute` >= 5)/nrow(testedDF)
sum(testedDF$`5 Minute` >= 15)/nrow(testedDF)



#tests representativeness of data
allBlocksDF <- imputeDF %>% distinct(blockNum, .keep_all=T)

untestedDemographics <- getRaceEstimates(untestedDF)
testedDemographics <- getRaceEstimates(testedDF)
#allBlocksDemographics <- getRaceEstimates(allBlocksDF) this isn't correct since not population weighted
untestedDemographicsUI <- getRaceEstimatesUI(untestedDemographics)
testedDemographicsUI <- getRaceEstimatesUI(testedDemographics)
#allBlocksDemographicsUI <- getRaceEstimatesUI(allBlocksDemographics)  


write_csv(as.data.frame(untestedDemographicsUI),"data/processed/untestedDemographics.csv")
write_csv(as.data.frame(testedDemographicsUI),"data/processed/testedDemographics.csv")
write_csv(as.data.frame(allBlocksDemographicsUI),"data/processed/allBlocksDemographics.csv")

#percentage of tests and blocks with lead > 1ppb

nrow(testedDF %>% filter(overOne_2))/nrow(testedDF)

blockLeadDF <- testedDF %>% 
  mutate(blockNum==factor(blockNum)) %>% 
  group_by(blockNum) %>% 
  summarize(blockLead = mean(as.numeric(overOne_2))) %>% 
  mutate(blockClass = ifelse(blockLead>=.5,T,F))

