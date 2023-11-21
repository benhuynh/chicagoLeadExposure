#calculates summary statistics of lead data, plots seasonality
library(readr)
library(lubridate)
library(ggplot2)
imputeDF <- read_csv("data/processed/imputeDF.csv")
imputeDF$month <- month(as.Date(imputeDF$`Date Sampled`))
testedDF <- imputeDF %>% filter(tested)
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
  