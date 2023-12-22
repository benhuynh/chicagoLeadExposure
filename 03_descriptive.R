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

#create table of lead concentrations
# Function to calculate the required statistics for a given column
calculate_stats <- function(column) {
  n <- nrow(testedDF)
  median_val <- median(column)
  iqr_val <- IQR(column)
  ge1 <- sum(column >= 1) / n
  ge5 <- sum(column >= 5) / n
  ge15 <- sum(column >= 15) / n
  
  c(Median = paste(median_val, "(", iqr_val, ")", sep = ""),
    GE1 = paste(sum(column >= 1), " (", round(ge1 * 100, 2), "%)", sep = ""),
    GE5 = paste(sum(column >= 5), " (", round(ge5 * 100, 2), "%)", sep = ""),
    GE15 = paste(sum(column >= 15), " (", round(ge15 * 100, 2), "%)", sep = ""))
}

# Creating a table with the required statistics for each column
results <- data.frame(
  `1st Draw` = calculate_stats(testedDF$`1st Draw`),
  `2-3 Minute` = calculate_stats(testedDF$`2-3 Minute`),
  `5 Minute` = calculate_stats(testedDF$`5 Minute`)
)

# Transposing the table to get the desired format
results_table <- t(results)
rownames(results_table) <- c("1st Draw", "2-3 Minute", "5 Minute")

write_csv(as.data.frame(results_table),"data/processed/leadTestConcentrations.csv")


median(testedDF$`1st Draw`)
iqr(testedDF$`1st Draw`)
sum(testedDF$`1st Draw` >= 1)/nrow(testedDF)
sum(testedDF$`1st Draw` >= 5)/nrow(testedDF)
sum(testedDF$`1st Draw` >= 15)/nrow(testedDF)

median(testedDF$`2-3 Minute`)
iqr(testedDF$`2-3 Minute`)
sum(testedDF$`2-3 Minute` >= 1)/nrow(testedDF)
sum(testedDF$`2-3 Minute` >= 5)/nrow(testedDF)
sum(testedDF$`2-3 Minute` >= 15)/nrow(testedDF)

median(testedDF$`5 Minute`)
iqr(testedDF$`5 Minute`)
sum(testedDF$`5 Minute` >= 1)/nrow(testedDF)
sum(testedDF$`5 Minute` >= 5)/nrow(testedDF)
sum(testedDF$`5 Minute` >= 15)/nrow(testedDF)



#tests representativeness of data
allBlocksDF <- imputeDF %>% distinct(blockNum, .keep_all=T)
allBlocksDF$blockNum <- as.character(allBlocksDF$blockNum)


untestedDemographics <- getRaceEstimates(untestedDF,tested=F)
testedDemographics <- getRaceEstimates(testedDF,tested=F)
allBlocksDemographics <- getRaceEstimates(allBlocksDF,tested=F)


demoDF <- as.data.frame(cbind(t(as.data.frame(testedDemographics)),
                t(as.data.frame(untestedDemographics)))
                )
colnames(demoDF) <- c("Tested","Untested")
demoDF$Total <- t(as.data.frame(allBlocksDemographics))

write_csv(demoDF,"data/processed/demoDF.csv")


#building characteristics
buildingAge <- read_csv("data/processed/buildingAge.csv")
buildingAge$age <- 2023-buildingAge$year
buildingAge2 <- buildingAge %>% 
  filter(!is.na(cxy_block_id)) %>% 
  mutate(blockNum = paste0("17031",cxy_tract_id,cxy_block_id)) %>% 
  left_join(allBlocksDF %>% select(blockNum,tested)) %>% 
  group_by(blockNum) %>% 
  mutate(nBlocks = n()) %>% ungroup() %>% 
  filter(!is.na(tested))
buildingAge3 <- buildingAge2 %>% 
  group_by(tested) %>% 
  summarize(medianBuildingsPerBlock = median(nBlocks),
            iqrBuildingsPerBlock = iqr(nBlocks),
            medianAge = median(age),
            iqrAge = iqr(age))
totalVec <- c("Total",median(buildingAge2$nBlocks),
              iqr(buildingAge2$nBlocks),
              median(buildingAge2$age),
              iqr(buildingAge2$age))
buildingDF <- as.data.frame(t(rbind(buildingAge3,totalVec)))
write_csv(t(buildingDF),"data/processed/buildingAgeStats.csv")



#percentage of tests and blocks with lead > 1ppb

nrow(testedDF %>% filter(overOne_2))/nrow(testedDF)

blockLeadDF <- testedDF %>% 
  mutate(blockNum==factor(blockNum)) %>% 
  group_by(blockNum) %>% 
  summarize(blockLead = mean(as.numeric(overOne_2))) %>% 
  mutate(blockClass = ifelse(blockLead>=.5,T,F))

