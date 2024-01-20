#Purpose: Generates density plots for Figure 1
library(readr)
library(ggplot2)
library(dplyr)
buildingAge <- read_csv("data/processed/buildingAge.csv")
buildingAge$age <- 2022-buildingAge$year
leadDF <- read_csv("data/processed/leadExposure_final.csv")
leadDF$`2-3 Minute` <- as.numeric(leadDF$`2-3 Minute`)


ggplot(data=buildingAge,aes(x=age)) + 
  geom_density(fill="#66CCCC", color="#66CCCC") + theme_classic() +
  xlab("Building Age") + ylab("Density") + geom_vline(xintercept=37,
                                                      linetype=2,
                                                      color="black") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

ggsave("figs/buildingAgeDensity.pdf",width=3,height=3)

out <- boxplot(leadDF$`2-3 Minute`)
dt <- leadDF$`2-3 Minute`
leadDF2 <- leadDF %>% filter(!`2-3 Minute` %in% out$out)
leadDF2 <- leadDF %>% filter(`2-3 Minute` < 50)
leadDF3 <- leadDF %>% mutate(`2-3 Minute` = ifelse(`2-3 Minute` > 15,15,`2-3 Minute`))
ggplot(data=leadDF3,aes(x=`2-3 Minute`)) + 
  geom_density(fill="#FF9B92", color=alpha("#FF9B92",1), alpha=1) + theme_classic() +
  xlab("Lead concentration (ppb)") + ylab("Density") + 
  scale_x_continuous(expand = c(0, 0),
                     breaks=c(0,5,10,15)) + scale_y_continuous(expand = c(0, 0))
ggsave("figs/leadPPBDensity.pdf",width=3,height=3)


#number of tests per census block
imputeDF <- read_csv("data/processed/imputeDF.csv")
numTestDF <- imputeDF %>% group_by(blockNum) %>% 
  mutate(numTests = sum(tested)) %>% ungroup() %>% 
  distinct(blockNum, .keep_all=T) %>% filter(tested) %>% 
  mutate(numTests2 = ifelse(numTests>=15,15,numTests))

ggplot(numTestDF,aes(x=numTests2)) +
  geom_histogram(binwidth = 1,fill="#b5bf7b") + theme_classic() +
  scale_x_continuous(expand=c(0,0),breaks=seq(1:15)) + xlab("Tests per block") +
  ylab("Blocks") + scale_y_continuous(expand=c(0,0))
ggsave("figs/numTestsPerBlock.pdf",width=3,height=3)

