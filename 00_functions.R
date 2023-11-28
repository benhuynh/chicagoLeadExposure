library(cowplot)
library(EnvStats)

special_transform <- function(shp) {
  b <- get_baseline(shp)
  S <- get_shap_values(shp)
  X <- get_feature_values(shp)
  
  # calculate prediction:
  p <- exp(b + rowSums(S)) / ( 1 + exp(b + rowSums(S)) )
  
  # transforming the baseline and shap values:
  b_new <- exp(b) / (1 + exp(b))
  S_new <- S / rowSums(S)*(p - b_new)
  
  return(shapviz(S_new, X, b_new))
}

cleanPropensityShap <- function(mat,noFactors=T) {
  shapDF <- as.data.frame(mat)
  if(noFactors) {
    shapDF$censusTract <- NULL
    shapDF$blockGroup <- NULL
  } else {
    shapDF <- shapDF %>% 
      rename("Census Tract" = censusTract,
             "Block Group" = blockGroup) 
  }
  shapDF <- shapDF %>% 
    rename("Block population" = blockPopulation,
           "% White (Block)" = propWhiteBlockPop,
           "% White (Tract)" = pctW,
           "% Hispanic (Block)" = propHispanicBlockPop,
           "% Black (Block)" = propBlackBlockPop,
           "% Asian (Block)" = propAsianBlockPop,
           "% AIAN (Block)" = propAianBlockPop,
           "Hardship Index" = HDX,
           "HS Graduation Rate" = EDB,
           "% Owner-occupied house" = pOwnerOccupiedHousesBG,
           "Min. building age (block)" = minAge,
           "Mean building age (block)" = meanAge,
           "Max building age (block)" = maxAge)
  shapDF <- as.matrix(shapDF)
  return(shapDF)
}
cleanOutcomeShap <- function(mat,noFactors=T,treeshap=F) {
  mat <- as.matrix(mat)
  if(treeshap) {
    tractNames <- colnames(mat)[76:857] #if columns change, indices will need to be fixed
    caNames <- colnames(mat)[858:933]
    bgNames <- colnames(mat)[934:3020]
    collapseList <- list(censusTract = tractNames,
                         CA = caNames,
                         blockGroup = bgNames)
    mat <- collapse_shap(mat,collapse=collapseList) 
  }
  shapDF <- as.data.frame(mat)
  if(noFactors) {
    shapDF$censusTract <- NULL
    shapDF$blockGroup <- NULL
    shapDF$CA <- NULL
  } else {
    shapDF <- shapDF %>%
      rename("Census Tract" = censusTract,
             "Block Group" = blockGroup,
             "Community Area" = CA) 
  }
  shapDF <- shapDF %>%
    rename("Block population" = blockPopulation,
           "% White (Block)" = propWhiteBlockPop,
           "% White (Tract)" = pctW,
           "% Hispanic (Block)" = propHispanicBlockPop,
           "% Black (Block)" = propBlackBlockPop,
           "% Asian (Block)" = propAsianBlockPop,
           "% AIAN (Block)" = propAianBlockPop,
           "Hardship Index" = HDX,
           "HS Graduation Rate" = EDB,
           "% Owner-occupied house" = pOwnerOccupiedHousesBG,
           "No. of buildings (Block)" = nBlocks,
           "% old buildings (Block)" = pOld,
           "Max building age (Block)" = maxAge,
           "Median building age (Block)" = medianAge,
           "Median building age (Block group)" = medianBuildingAgeBG,
           "No. of housing units (Block group)" = numHousingUnitsBG,
           "Minimum building age (Block)" = minAge,
           "Mean building age (Block)" = meanAge
           )
  shapDF <- as.matrix(shapDF)
  return(shapDF)
}
cleanXmat <- function(xmat) {
  xmat <- as.data.frame(xmat) %>%
    mutate(`Census Tract` = 0,
           `Block Group` = 0,
           `Community Area` = 0,
           `No. of buildings (Block)` = log(`No. of buildings (Block)`+1),
           `Block population` = log(`Block population`))
  return(as.matrix(xmat))
  
}
calcIQLoss2 <- function(bl,sim,c1,c2) {
  #calculates IQ loss attributable to lead-contaminated drinking water
  if(sim <= 10 & bl <= 10) {
    return((bl-sim)*c1) 
  }
  if(sim <= 10 & bl > 10 & bl <= 20) {
    return((10-sim)*c1+(bl-10)*c2)
  }
  if(sim > 10 & bl > 10 ) {
    return((bl-sim)*c2)
  }
}
rTruncNormCorrected <- function(n,m,SD) {
  normVec <- rnormTrunc(n,mean=m,sd=SD,min=0)
  correction <- mean(normVec)-m
  return(normVec-correction)
}

simIQDensity <- function(simDF) {
  iqMat <- matrix(nrow=nrow(simDF),ncol=1000)
  for(i in 1:1000) {
  coef1 <- runif(1,.38,.86)
  coef2 <- runif(1,.12,.26)
  coef3 <- runif(1,.07,.15)
  bllIncreaseConstant <- runif(1,exp(.09),exp(.33))
  simBGPopVec <- round(runif(nrow(simDF),simDF$totalPopulationBG-simDF$totalPopulation_MOE95,
                             simDF$totalPopulationBG+simDF$totalPopulation_MOE95))
  simPunder6Vec <- runif(nrow(simDF),simDF$pAllChildrenUnder6BG-simDF$pAllChildrenUnder6BG_MOE95,
                         simDF$pAllChildrenUnder6BG+simDF$pAllChildrenUnder6BG_MOE95)
  simPunder6Vec[simPunder6Vec<0] <- 0
  simDF$simBGPop <- simBGPopVec
  simDF$simPunder6 <- simPunder6Vec
  simDF$simBlockPop = simDF$blockPopulation/simDF$totalPopulationBG*
    simDF$simBGPop
  simDF$simulatedTruth = rbinom(nrow(simDF),1,simDF$calibPreds)
  simDF$expChildren <- simDF$simulatedTruth*simDF$simBlockPop*simDF$simPunder6
  simDF$leadConcentration <- concentrationSampVec[,i]
  simDF$baseLineBLL <- rTruncNormCorrected(nrow(simDF),SD=SD,m=meansVec)
  simDF$baseLineBLL[simDF$baseLineBLL<0] <- 0
  simDF$simBLL <- simDF$baseLineBLL/(bllIncreaseConstant^simDF$leadConcentration)
  simDF$iqLoss <- mapply(calcIQLoss2,sim=simDF$simBLL,
                         bl=simDF$baseLineBLL,c1=coef1,c2=coef2)
  iqMat[,i] <- simDF$iqLoss
  print(i)
  }
  return(iqMat)
}

simFunc <- function(simDF,norm=T,meansVec=meansVec,adjustBLL=T,
                    calib=T) {
  simMat <- matrix(nrow=10000,ncol=20)
  for(i in 1:10000)  {
    coef1 <- runif(1,.38,.86)
    coef2 <- runif(1,.12,.26)
    coef3 <- runif(1,.07,.15)
    
    if(adjustBLL) {
      bllIncreaseConstant <- runif(1,exp(.09),exp(.33))
    } else {
      bllIncreaseConstant <- runif(1,exp(.18),exp(.42))
    }
    simBGPopVec <- round(runif(nrow(simDF),simDF$totalPopulationBG-simDF$totalPopulation_MOE95,
                               simDF$totalPopulationBG+simDF$totalPopulation_MOE95))
    simPunder6Vec <- runif(nrow(simDF),simDF$pAllChildrenUnder6BG-simDF$pAllChildrenUnder6BG_MOE95,
                           simDF$pAllChildrenUnder6BG+simDF$pAllChildrenUnder6BG_MOE95)
    simPunder6Vec[simPunder6Vec<0] <- 0
    
    simDF$simBGPop <- simBGPopVec
    simDF$simPunder6 <- simPunder6Vec
    simDF$simBlockPop = simDF$blockPopulation/simDF$totalPopulationBG*
      simDF$simBGPop
    if(calib) {
      simDF$simulatedTruth = rbinom(nrow(simDF),1,simDF$calibPreds)
    } else{
      simDF$simulatedTruth = rbinom(nrow(simDF),1,simDF$preds)
    }
    
    simDF$expChildren <- simDF$simulatedTruth*simDF$simBlockPop*simDF$simPunder6
    simDF$leadConcentration <- concentrationSampVec[,i]
    if(norm) {
      simDF$baseLineBLL <- rTruncNormCorrected(nrow(simDF),SD=SD,m=meansVec)
    } else {
      simDF$baseLineBLL <- runif(nrow(simDF),0,5*(1+simDF$LDPP_2021/100))
    }
    simDF$baseLineBLL[simDF$baseLineBLL<0] <- 0
    simDF$simBLL <- simDF$baseLineBLL/(bllIncreaseConstant^simDF$leadConcentration)
    simDF$iqLoss <- mapply(calcIQLoss2,sim=simDF$simBLL,
                           bl=simDF$baseLineBLL,c1=coef1,c2=coef2)
    simDF$totalIQLoss <- simDF$iqLoss*simDF$expChildren
    simDF$wIQLoss <- simDF$totalIQLoss*simDF$propWhiteBlockPop
    simDF$bIQLoss <- simDF$totalIQLoss*simDF$propBlackBlockPop
    simDF$aIQLoss <- simDF$totalIQLoss*simDF$propAsianBlockPop
    simDF$hIQLoss <- simDF$totalIQLoss*simDF$propHispanicBlockPop
    wExpChildPop <- sum(simDF$propWhiteBlockPop*simDF$expChildren)
    bExpChildPop <- sum(simDF$propBlackBlockPop*simDF$expChildren)
    aExpChildPop <- sum(simDF$propAsianBlockPop*simDF$expChildren)
    hExpChildPop <- sum(simDF$propHispanicBlockPop*simDF$expChildren)
    meanWIQLoss <- sum(simDF$wIQLoss)/wExpChildPop
    meanBIQLoss <- sum(simDF$bIQLoss)/bExpChildPop
    meanAIQLoss <- sum(simDF$aIQLoss)/aExpChildPop
    meanHIQLoss <- sum(simDF$hIQLoss)/hExpChildPop
    iqLoss95 <- as.numeric(quantile(simDF$iqLoss,probs=.95))
    simChildPop <- sum(simDF$simBlockPop*simDF$simPunder6)
    wChildPop <- sum(simDF$propWhiteBlockPop*simDF$simBlockPop*simDF$simPunder6)
    bChildPop <- sum(simDF$propBlackBlockPop*simDF$simBlockPop*simDF$simPunder6)
    aChildPop <- sum(simDF$propAsianBlockPop*simDF$simBlockPop*simDF$simPunder6)
    hChildPop <- sum(simDF$propHispanicBlockPop*simDF$simBlockPop*simDF$simPunder6)
    meanWIQLossTotalPop <- sum(simDF$wIQLoss)/wChildPop
    meanBIQLossTotalPop <- sum(simDF$bIQLoss)/bChildPop
    meanAIQLossTotalPop <- sum(simDF$aIQLoss)/aChildPop
    meanHIQLossTotalPop <- sum(simDF$hIQLoss)/hChildPop
    returnVec <- c(sum(simDF$expChildren),sum(simDF$totalIQLoss),
                   meanWIQLoss,meanBIQLoss,
                   meanAIQLoss,meanHIQLoss,
                   wExpChildPop,bExpChildPop,
                   aExpChildPop,hExpChildPop,
                   simChildPop,wChildPop,
                   bChildPop,aChildPop,
                   hChildPop,meanWIQLossTotalPop,
                   meanBIQLossTotalPop,meanAIQLossTotalPop,
                   meanHIQLossTotalPop,iqLoss95)
    simMat[i,] <- returnVec
    print(i)
  }
  simMatDF <- as.data.frame(simMat)
  colnames(simMatDF) <- c("AffectedChildren_T","TotalIQLoss_T",
                          "MeanIQLoss_W","MeanIQLoss_B",
                          "MeanIQLoss_A","MeanIQLoss_H",
                          "AffectedChildren_W","AffectedChildren_B",
                          "AffectedChildren_A","AffectedChildren_H",
                          "ChildPopulation_T","ChildPopulation_W",
                          "ChildPopulation_B","ChildPopulation_A",
                          "ChildPopulation_H","MeanIQLossTotalPop_W",
                          "MeanIQLossTotalPop_B","MeanIQLossTotalPop_A",
                          "MeanIQLossTotalPop_H","iqLoss95_T")
  return(simMatDF)
}


generateSimTable <- function(vec) {
  tableDF <- as.data.frame(t(vec))
  tableDF$TotalIQLoss_A <- tableDF$MeanIQLoss_A*tableDF$AffectedChildren_A
  tableDF$TotalIQLoss_B <- tableDF$MeanIQLoss_B*tableDF$AffectedChildren_B
  tableDF$TotalIQLoss_H <- tableDF$MeanIQLoss_H*tableDF$AffectedChildren_H
  tableDF$TotalIQLoss_W <- tableDF$MeanIQLoss_W*tableDF$AffectedChildren_W
  tableDF$MeanIQLoss_T <- tableDF$TotalIQLoss_T/tableDF$AffectedChildren_T
  tableDF <- tableDF %>%
    pivot_longer(cols = everything()) %>%
    separate_wider_delim(name,delim="_",names=c("var","racial_group")) %>% 
    pivot_wider(names_from = racial_group, values_from = value)
  tableDF[5,2] <- tableDF[2,2]/tableDF[4,2] #calculate mean for total population
  tableDF[2:6] <- signif(tableDF[2:6],3) #round significant figures
  tableDF2 <- tableDF[,c(1,2,5,4,6,3)] #reorder columns in alphabetical order
  return(tableDF2)
}

generateSimAggTable <- function(sDF) {
  sDF$iqLoss95_T <- NULL
  medians <- apply(sDF,2,median)
  lower05 <- apply(sDF,2,quantile,probs=0.05)
  upper95 <- apply(sDF,2,quantile,probs=0.95)
  
  simTableDF <- generateSimTable(medians)
  simTableDF_lower05 <- generateSimTable(lower05)
  simTableDF_upper95 <- generateSimTable(upper95)
  
  combined_df <- data.frame(matrix(ncol = ncol(simTableDF), nrow = nrow(simTableDF)))
  
  for (i in 1:nrow(combined_df)) {
    for (j in 1:ncol(combined_df)) {
      combined_df[i, j] <- paste0(simTableDF[i, j], " (", simTableDF_lower05[i, j], "-", simTableDF_upper95[i, j], ")")
    }
  }
  combined_df$X1 <- simTableDF$var
  colnames(combined_df) <- colnames(simTableDF)
  simAggTable <- as.data.frame(t(combined_df)) %>% rownames_to_column()
  simAggTable2 <- simAggTable[, c(1, 5, 2, 3,4,6)] #fix column orders
  return(simAggTable2)
}

fitSplit <- function(dataSplit,testData,gridNum=5) {
  #returns original preds and calibrated preds for a given data split
  #lightgbm specification
  tune_spec_calib2 <- boost_tree(
    min_n = tune(),
    trees = 1000,
    tree_depth = tune()) %>% 
    set_mode("classification") %>% 
    set_engine("lightgbm")
    tune_wf_calib2 <- workflow() %>%
      add_formula(overOne_2 ~ .-blockNum) %>%  
      add_model(tune_spec_calib2)
  trees_folds_dataSplit <- group_vfold_cv(dataSplit,group=blockNum,v=3)
  doParallel::registerDoParallel()
  set.seed(127)
  tune_res_dataSplit <- tune_grid(
    tune_wf_calib2,
    resamples = trees_folds_dataSplit,
    grid = gridNum,
    metrics = metric_set(roc_auc, brier_class),
    control = control_grid(save_pred=T)
  )
  best_tree_dataSplit <- tune_res_dataSplit %>%
    select_best("roc_auc")
  final_wf_dataSplit <- 
    tune_wf_calib2 %>% 
    finalize_workflow(best_tree_dataSplit)
  bestFitPreds <- collect_predictions(tune_res_dataSplit,
                                      parameters=best_tree_dataSplit)
  fit_split <- final_wf_dataSplit %>%
    fit(dataSplit)
  cell_cal <- cal_estimate_beta(bestFitPreds,truth=overOne_2)
  test_pred <- augment(fit_split, new_data = testData)
  cal_pred <-
    test_pred %>%
    cal_apply(cell_cal)
  cal_pred$rawPreds <- test_pred$.pred_FALSE
  cal_pred <- cal_pred %>% rename(calibPreds = .pred_FALSE)
  return(cal_pred)
}


calibPlot <- function(predDF) {
  #takes a predDF and produces a 2x2 plot
  #each predDF contains preds and calibpreds, for each split
  cls_met <- metric_set(roc_auc, brier_class)
  
  predDF <- predDF %>% select(-.pred_TRUE) %>% 
    rename(.pred_FALSE = rawPreds,
           .pred_TRUE = calibPreds)

  predDF$outcome <- predDF$overOne_2
  tab1 <- predDF %>% cls_met(outcome, .pred_FALSE)
  tab2 <- predDF %>% cls_met(outcome, .pred_TRUE)
  predPlot <- predDF %>% 
    cal_plot_windowed(truth = outcome,
                      estimate = .pred_FALSE, 
                      step_size = 0.025,
                      include_ribbon=F) + 
    annotate("text", label = paste0("AUC = ",round(tab1[1,3],3)),
             x = .75, y = .25, size = 4, colour = "black") +
    annotate("text", label = paste0("Brier = ",round(tab1[2,3],3)),
             x = .75, y = .175, size = 4, colour = "black") +
    theme_classic() + ggtitle("Uncalibrated predictions")
  calibPlot <- predDF %>% 
    cal_plot_windowed(truth = outcome,
                      estimate = .pred_TRUE, 
                      step_size = 0.025,
                      include_ribbon=F) + 
    annotate("text", label = paste0("AUC = ",round(tab2[1,3],3)),
             x = .75, y = .25, size = 4, colour = "black") +
    annotate("text", label = paste0("Brier = ",round(tab2[2,3],3)),
             x = .75, y = .175, size = 4, colour = "black") +
    theme_classic() + ggtitle("Calibrated predictions")
  #return(list(predPlot,calibPlot))
  prow <- plot_grid(predPlot,calibPlot,
                     labels = c("A", "B"),label_size=10,vjust=1.75,hjust=-2)
  return(prow)
}

getDrawPredictions <- function(imputeDF,draw=1) {
  if(draw==1) {
    imputeDF <- imputeDF %>% select(-`5 Minute`,-overOne_2) %>% 
      mutate(overOne_2 = factor(ifelse(`1st Draw` > 0,TRUE,FALSE))) %>% 
      select(-`1st Draw`)
  }
  if(draw==5) {
    imputeDF <- imputeDF %>% select(-`1st Draw`,-overOne_2) %>% 
      mutate(overOne_2 = factor(ifelse(`5 Minute` > 0,TRUE,FALSE))) %>% 
      select(-`5 Minute`)
  }
  mlDF2 <- imputeDF %>% filter(tested) 
  mlDF2 <- mlDF2 %>% 
    select(-tested,-`2-3 Minute`,
           -HHsInternetPropBG,-pCompletePlumbingFacilitiesBG,
           -HHsHasComputerPropBG,-pOccupiedHousesBG,-sequential,
           -pRenterOccupiedHousesBG,-`Date Sampled`) %>%  #remove redundant features
    mutate(censusTract = factor(censusTract),
           CA = factor(CA),
           blockNum = factor(blockNum),
           blockGroup = factor(blockGroup),
    )
  split_df2 <- group_initial_split(mlDF2,group=blockNum) #keep blocks in same split
  trainDF2 <- training(split_df2)
  testDF2 <- testing(split_df2)
  tree_rec2 <- recipe(overOne_2 ~ ., data = trainDF2) %>% 
    update_role(blockNum, new_role="ID")
  
  #lightgbm specification
  tune_spec2 <- boost_tree(
    min_n = tune(),
    trees = 1000,
    tree_depth = tune()) %>% 
    set_mode("classification") %>% 
    set_engine("lightgbm")
  tune_wf2 <- workflow() %>%
    add_recipe(tree_rec2) %>% 
    add_model(tune_spec2)
  
  trees_folds3 <- group_vfold_cv(trainDF2,group=blockNum,v=3)
  
  
  tune_res_final2 <- tune_grid(
    tune_wf2,
    resamples = trees_folds3,
    grid = 15
  )
  tune_res_final2 %>%
    tune::show_best(metric = "roc_auc",n = 5) %>% print()
  best_tree2 <- tune_res_final2 %>%
    select_best("roc_auc")
  final_wf2 <- 
    tune_wf2 %>% 
    finalize_workflow(best_tree2)
  final_fit2 <- 
    final_wf2 %>%
    last_fit(split_df2)
  final_fit2 %>% 
    collect_metrics() %>% print()
  
  #make calibrated risk predictions
  predSplits <- group_initial_split(mlDF2,group=blockNum,prop=1/2)
  split1 <- training(predSplits)
  split2 <- testing(predSplits)
  
  splitDF1 <- fitSplit(dataSplit = split1, testData=split2,gridNum=10)
  splitDF2 <- fitSplit(dataSplit = split2, testData=split1,gridNum=10)
  
  riskDF <- rbind(splitDF1,splitDF2)
  riskDFname <- paste0("data/processed/riskDF_pt1_draw",draw,".csv")
  write_csv(riskDF,riskDFname)
  
  imputeDF <- imputeDF %>%
    select(-`2-3 Minute`,
           -HHsInternetPropBG,-pCompletePlumbingFacilitiesBG,
           -HHsHasComputerPropBG,-pOccupiedHousesBG,-sequential,
           -pRenterOccupiedHousesBG,-`Date Sampled`) %>%  #remove redundant features
    mutate(censusTract = factor(censusTract),
           CA = factor(CA),
           blockNum = factor(blockNum),
           blockGroup = factor(blockGroup),
    )
  propensity_train <- imputeDF %>% filter(tested==T) %>% 
    select(-tested)
  propensity_test <- imputeDF %>% filter(tested==F) %>% 
    select(-tested)
  withoutTestsPreds <- fitSplit(dataSplit = propensity_train,
                                testData=propensity_test,gridNum=10)
  withoutTests <- withoutTestsPreds %>% 
    rename(preds = rawPreds) %>% 
    select(blockNum,preds,calibPreds)
  withTests <- riskDF %>% 
    rename(preds = rawPreds) %>% 
    select(blockNum,preds,calibPreds)
  riskDF2 <- rbind(withTests,withoutTests)
  riskDFnameFinal <- paste0("data/processed/riskDF_draw",draw,".csv")
  write_csv(riskDF2,riskDFnameFinal)
  return(riskDF2)
}



