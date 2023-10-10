#Purpose: Generates calibration plots
source("00_functions.R")
library(tidymodels)
library(probably)

riskDF <- read_csv("data/processed/riskDF_pt1.csv")
riskDF$overOne_2 <- factor(riskDF$overOne_2)
calibrationPlots <- calibPlot(riskDF)
ggsave("figs/calibrationPlots.png",plot=calibrationPlots,
       width=6,height=3)
