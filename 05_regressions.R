#calculates regressions for risk of lead exposure and screening odds
library(readr)
library(ggplot2)
library(ggdensity)
library(dplyr)
library(tidyverse)
riskDF <- read_csv("data/processed/riskDF.csv")
imputeDF <- read_csv("data/processed/imputeDF.csv")
imputeDF2 <- imputeDF %>% select(blockPopulation,
                                             propWhiteBlockPop,
                                             propBlackBlockPop,
                                             propAianBlockPop,
                                             propAsianBlockPop,
                                             propHispanicBlockPop,
                                             Population,
                                             pctW,pctB,pctA,
                                             pctH,pctP,pctQ,
                                             whitePropBG,blackPropBG,
                                             aianPropBG,asianPropBG,
                                             hispanicPropBG,
                                             educationHSPropBG,
                                             educationAssociatesPropBG,
                                             educationBachelorsPropBG,
                                             educationSomeCollegeLess1PropBG,
                                             educationSomeCollegeMore1PropBG,
                                             educationMastersPropBG,
                                             educationPrfsnlSchoolPropBG,
                                             educationDoctoratePropBG,blockGroup,
                                             censusTract,blockNum,tested,overOne_2) %>% 
  distinct(blockNum,.keep_all=T)
riskDF <- riskDF %>% select(-overOne_2) %>% distinct()
riskDF$blockNum <- as.character(riskDF$blockNum)
imputeDF2$blockNum <- as.character(imputeDF2$blockNum)
rarDF <- imputeDF2 %>% left_join(riskDF,by=c("blockNum"="blockNum"))
outcomeDF <- imputeDF %>% filter(tested) %>%
  group_by(blockNum) %>% mutate(meanOutcome = mean(as.numeric(overOne_2)),
                                meanOutcome = ifelse(meanOutcome>=0.5,TRUE,FALSE)) %>% 
  ungroup() %>% distinct(blockNum,.keep_all=T)

outcomeTestsDF <- imputeDF %>% filter(tested)

#risk adjusted regressions
rarW <- glm(tested~propWhiteBlockPop+preds+blockPopulation,data=rarDF,family="binomial") %>% summary()

rarB <- glm(tested~propBlackBlockPop+preds+blockPopulation,data=rarDF,family="binomial") %>% summary()

rarA <- glm(tested~propAsianBlockPop+preds+blockPopulation,data=rarDF,family="binomial") %>% summary()

rarH <- glm(tested~propHispanicBlockPop+preds+blockPopulation,data=rarDF,family="binomial") %>% summary()
#test regressions

rrW <- glm(tested~propWhiteBlockPop+blockPopulation,data=rarDF,family="binomial") %>% summary()

rrB <- glm(tested~propBlackBlockPop+blockPopulation,data=rarDF,family="binomial") %>% summary()

rrA <- glm(tested~propAsianBlockPop+blockPopulation,data=rarDF,family="binomial") %>% summary()

rrH <- glm(tested~propHispanicBlockPop+blockPopulation,data=rarDF,family="binomial") %>% summary()

#outcome regressions using mean outcome, with blocks as observation
outcomeW <- glm(meanOutcome~propWhiteBlockPop,data=outcomeDF,family="binomial") %>% summary()
outcomeB <- glm(meanOutcome~propBlackBlockPop,data=outcomeDF,family="binomial") %>% summary()
outcomeA <- glm(meanOutcome~propAsianBlockPop,data=outcomeDF,family="binomial") %>% summary()
outcomeH <- glm(meanOutcome~propHispanicBlockPop,data=outcomeDF,family="binomial") %>% summary()

#outcome regressions using tests as observations
outcomeW_tests <- glm(overOne_2~propWhiteBlockPop,data=outcomeTestsDF,family="binomial") %>% summary()
outcomeB_tests <- glm(overOne_2~propBlackBlockPop,data=outcomeTestsDF,family="binomial") %>% summary()
outcomeA_tests <- glm(overOne_2~propAsianBlockPop,data=outcomeTestsDF,family="binomial") %>% summary()
outcomeH_tests <- glm(overOne_2~propHispanicBlockPop,data=outcomeTestsDF,family="binomial") %>% summary()



getGLMResults <- function(mod) {
  c <- mod$coefficients[2]
  std.err <- mod$coefficients[2,2]
  CI <- (exp(std.err*qnorm(0.975))-1)*10
  cExp <- (exp(c)-1)*10 #interpret as per 10% increase in pop, not 100%
  upper <- cExp + CI
  lower <- cExp - CI
  pValue <- mod$coefficients[2,4]
  rr <- sqrt(exp(c))
  if(rr >= 1) {
    EValue <- rr + sqrt(rr*(rr-1))
  } else {
    EValue <- (1/rr) + sqrt((1/rr)*((1/rr)-1))
  }
  return(round(c(c,std.err,cExp,lower,upper,pValue,EValue),2))
}

#rar race results
rarWResults <- getGLMResults(rarW)
rarBResults <- getGLMResults(rarB)
rarHResults <- getGLMResults(rarH)
rarAResults <- getGLMResults(rarA)

#supplementary test regressions
rrWResults <- getGLMResults(rrW)
rrBResults <- getGLMResults(rrB)
rrHResults <- getGLMResults(rrH)
rrAResults <- getGLMResults(rrA)


#outcome race results
outcomeWResults <- getGLMResults(outcomeW)
outcomeBResults <- getGLMResults(outcomeB)
outcomeHResults <- getGLMResults(outcomeH)
outcomeAResults <- getGLMResults(outcomeA)

#supplementary test-level outcome regressions
outcomeWResults_tests <- getGLMResults(outcomeW_tests)
outcomeBResults_tests <- getGLMResults(outcomeB_tests)
outcomeHResults_tests <- getGLMResults(outcomeH_tests)
outcomeAResults_tests <- getGLMResults(outcomeA_tests)



generateRegressionTable <- function(rarAResults,rarBResults,
                                    rarHResults,rarWResults,
                                    outcomeAResults,outcomeBResults,
                                    outcomeHResults,outcomeWResults) {
rarTable <- as.data.frame(rbind(rarAResults,rarBResults,
                  rarHResults,rarWResults))
colnames(rarTable) <- c("Coefficient","Std. err",
                        "Increase","Lower","Upper",
                        "p","E")
outcomeTable <- as.data.frame(rbind(
  outcomeAResults,outcomeBResults,
  outcomeHResults,outcomeWResults
))
colnames(outcomeTable) <- c("Coefficient_O","Std. err_O",
                        "Increase_O","Lower_O","Upper_O",
                        "p_O","E_0")
rarTable$Increase <- paste0(rarTable$Increase," (",
                            rarTable$Lower,",",
                            rarTable$Upper,")")
rarTable$Lower <- NULL
rarTable$Upper <- NULL
outcomeTable$Increase_O <- paste0(outcomeTable$Increase_O," (",
                                outcomeTable$Lower_O,",",
                                outcomeTable$Upper_O,")")
outcomeTable$Lower_O <- NULL
outcomeTable$Upper_O <- NULL
regTable <- cbind(rarTable,outcomeTable)
regTable$p <- ifelse(regTable$p==0,"<0.001",regTable$p)
regTable$p_O <- ifelse(regTable$p_O==0,"<0.001",regTable$p_0)
return(regTable)
}

regTable <- generateRegressionTable(rarAResults,rarBResults,
                                    rarHResults,rarWResults,
                                    outcomeAResults,outcomeBResults,
                                    outcomeHResults,outcomeWResults)
regTable2 <- generateRegressionTable(rrAResults,rrBResults,
                                    rrHResults,rrWResults,
                                    outcomeAResults_tests,outcomeBResults_tests,
                                    outcomeHResults_tests,outcomeWResults_tests)


write_csv(regTable,"data/processed/regressionTable.csv")
write_csv(regTable2,"data/processed/regressionTableSupplement.csv")


rarDF2 <- rarDF %>% pivot_longer(cols=c("asianPropBG",
                                        "blackPropBG",
                                        "hispanicPropBG",
                                        "whitePropBG"))

probs = c(0.50,0.75, 0.95, 0.99)
probs.lab = paste0(rev(probs)*100, "%")

ggplot(rarDF2,aes(x=value,y=1-calibPreds)) +
  #geom_point()+
  geom_hdr(fill="#01659f",probs=probs) +
  facet_wrap(vars(name),
             labeller = labeller(name=c("asianPropBG"="Asian",
                                        "blackPropBG"="Black",
                                        "hispanicPropBG"="Hispanic",
                                        "whitePropBG"="White"))) + xlab("Population by race/ethnicity") +
  ylab("Estimated risk of lead exposure") +
  theme_bw() +
  scale_alpha_discrete("Density region", labels = probs.lab) + 
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("figs/raceDensityPlotBG.png",height=4,width=6)
