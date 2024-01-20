library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(scales)
library(stringr)
leadExposure <- read_csv("data/processed/leadExposure_final.csv")
nc = st_read("data/Comm_20Areas__1_/CommAreas.shp")
leadExposure$CA <- factor(leadExposure$CA)

leadExposure2 <- leadExposure %>% 
  group_by(COMMUNIT_1) %>% 
  mutate(over1 = (as.numeric(`2-3 Minute`) >= 1),
         sumOver1 = sum(over1),
         n = n(),
         rate = sumOver1/n,
         mean=mean(as.numeric(`2-3 Minute`))) %>% 
  ungroup() %>%
  distinct(COMMUNIT_1,.keep_all=T)
  

#group LE by CA first
nc2 <- nc %>% left_join(leadExposure2,by=c("AREA_NUMBE"="CA"))

pal <- c("#e0ecf4",
  "#9ebcda",
  "#8856a7")

riskDF <- read_csv("data/processed/riskDF.csv")
tractsToCA <- read_csv("data/2020 Census Tracts to Chicago Community Area Equivalency File - Sheet1.csv")
riskDF$censusTract <- as.numeric(str_sub(riskDF$blockNum,start=1,end=11))
riskMapDF <- riskDF %>% left_join(tractsToCA,by=c("censusTract"="GEOID20"))

riskMapDF2 <- riskMapDF %>% 
  group_by(COMMUNIT_1) %>% 
  mutate(prediction = preds >= .5,
         calibPreds = 1-calibPreds,
         sumPred = sum(prediction),
         n = n(),
         rate = sumPred/n,
         medPred = median(calibPreds),
         predRate = sum(calibPreds)/n) %>% 
  ungroup() %>%
  distinct(COMMUNIT_1,.keep_all=T) %>% 
  mutate(CA = factor(CA))

nc3 <- nc %>% left_join(riskMapDF2,by=c("AREA_NUMBE"="CA"))
  ggthemes::theme_map()


  
  

library(tidycensus)
apiKey <- readLines("confidential/censusapi.txt",n=1)
census_api_key(apiKey)

acsCBG <- get_acs(geography = "cbg", 
              variables = c(housingUnits = "B25001_001"), 
              state = "IL",
              county = "Cook",
              year = 2021,geometry=T)

#get census tract boundaries map
tractMapDF <- get_acs(geography="tract",
                    variables="B19013_001",
                    state="IL",
                    county = "Cook",
                    year = 2021,
                    geometry=T)
tractMapDF$GEOID <- as.character(tractMapDF$GEOID)

#filter to chicago only
tractMapDF2 <- tractMapDF %>% filter(GEOID %in% as.character(riskMapDF$censusTract)) %>% 
  select(GEOID,geometry)

#get adjusted prevalence map by census tract
riskMapDF3 <- riskMapDF %>%
  mutate(calibPreds = 1-calibPreds) %>% 
  group_by(blockNum) %>% 
  mutate(preds = median(preds),
         calibPreds = median(calibPreds)) %>%
  distinct(blockNum, .keep_all=TRUE) %>% 
  ungroup() %>% 
  group_by(censusTract) %>% 
  mutate(prediction = preds >= .5,
         sumPred = sum(prediction),
         n = n(),
         rate = sumPred/n,
         medPred = median(calibPreds),
         predRate = sum(calibPreds)/n) %>% 
  ungroup() %>%
  distinct(censusTract,.keep_all=T) %>% 
  mutate(censusTract = factor(censusTract))

tractMapDF3 <- tractMapDF2 %>% left_join(riskMapDF3,by=c("GEOID"="censusTract"))
#get raw prevalence map by census tract

leadExposure2 <- leadExposure %>%
  group_by(CensusTract) %>% 
  mutate(over1 = (as.numeric(`2-3 Minute`) >= 1),
         sumOver1 = sum(over1),
         n = n(),
         rate = sumOver1/n,
         mean=mean(as.numeric(`2-3 Minute`)),
         CensusTract=as.character(CensusTract)) %>% 
  ungroup() %>%
  distinct(CensusTract,.keep_all=T)

tractMapDF4 <- tractMapDF2 %>% left_join(leadExposure2,by=c("GEOID"="CensusTract"))


rawLeadPrevalencePlot <- ggplot() + geom_sf(data=tractMapDF4, aes(fill=rate)) +
  scale_fill_distiller(type="seq",palette=9,direction=1,
                        name="Value (%)",
                        labels=label_percent(accuracy = 1L),
                        limits=c(0,1)) + 
  ggthemes::theme_map() + ggtitle("Raw prevalence") +
  theme(plot.title = element_text(size = 8, hjust = 0.5))

estimatedRiskPlot <- ggplot() + geom_sf(data=tractMapDF3, aes(fill=medPred)) +
  scale_fill_distiller(type="seq",palette=9,direction=1,
                        name="Value (%)",
                        labels=label_percent(accuracy = 1L),
                        limits=c(0,1)) + 
  ggthemes::theme_map() + ggtitle("Estimated risk") +
  theme(plot.title = element_text(size = 8, hjust= 0.5))

# numPredictedLead <- riskMapDF %>% filter(calibPreds<=0.5) %>% nrow()
# print(paste0(round(numPredictedLead/nrow(riskMapDF)*100,2),"% of blocks are predicted to have lead service lines."))
# numRawLead <- leadExposure %>% filter(`2-3 Minute` >= 1) %>% nrow()
# print(paste0(round(numRawLead/nrow(leadExposure)*100,2),"% of raw observations have lead service lines."))

propensitiesDF <- read_csv("data/processed/imputeDF.csv")
missingDF <- propensitiesDF %>% select(blockNum,censusTract,tested,pctW) %>% 
  mutate(censusTract = as.character(censusTract)) %>% 
  group_by(censusTract) %>% 
  mutate(n = n(),
         rate = sum(tested)/n,
         ) %>% 
  ungroup() %>% 
  distinct(censusTract,.keep_all=T)
tractMapDF5 <- tractMapDF2 %>% left_join(missingDF,by=c("GEOID"="censusTract"))


testingRatesPlot <- ggplot() + geom_sf(data=tractMapDF5, aes(fill=rate)) +
  scale_fill_distiller(type="seq",palette=9,direction=1,
                        name="Testing rates",
                        labels=label_percent(accuracy = 1L),
                        limits=c(0,1)) + 
  ggthemes::theme_map() + ggtitle("Testing rates") +
  theme(plot.title = element_text(size = 8, hjust = 0.5))

pctPOCPlot <- ggplot() + geom_sf(data=tractMapDF5, aes(fill=1-pctW)) +
  scale_fill_distiller(type="seq",palette=9,direction=1,
                        name="% POC",
                        
                        ) + 
  ggthemes::theme_map()+ ggtitle("Racially minoritized") +
  theme(plot.title = element_text(size = 8, hjust = 0.5))

caFilterPlot <- readRDS("data/processed/caFilterPlot.rds")

library(cowplot)
# Create an empty plot
empty_plot <- ggplot() + 
  theme_void() + 
  theme(plot.background = element_blank())

prow <- plot_grid(rawLeadPrevalencePlot + theme(legend.position="none"),
          estimatedRiskPlot + theme(legend.position="none"),
          #caFilterPlot + theme(legend.position="none",plot.title = element_text(size = 8, hjust = 0.5)),
          testingRatesPlot + theme(legend.position="none"),
          pctPOCPlot + theme(legend.position="none"),
          labels = c("A", "B", "C","D", "E"),label_size=7,vjust=1.75,hjust=-1)

legend <- get_legend(
  # create some space to the left of the legend
  rawLeadPrevalencePlot + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom") + labs(fill="text")
)
legendRow <- plot_grid(NULL, legend,NULL, nrow = 1)#, rel_widths = c(0.5, 1, 0.5))
mapPlot <- plot_grid(prow, legendRow, rel_heights = c(3, .2),nrow=2)

library(tmaptools)
plot_ratio <- get_asp_ratio(tractMapDF5)
ggsave('figs/p2_perfect2.png', plot=mapPlot, width = plot_ratio*5, height=5)
ggsave('figs/p2_perfect2.pdf', plot=mapPlot, width = plot_ratio*5, height=5)



##shap maps
outcomeShap <- readRDS("data/processed/outcomeShap.rds")
probShap <- special_transform(outcomeShap)
shapDF <- as.data.frame(probShap$S)
trainDF2 <- read_csv("data/processed/trainDF2.csv")
shapDF <- shapDF %>% select(`censusTract`,`CA`,
                            `blockGroup`) %>% 
  rename(tractShap = `censusTract`,
         caShap = `CA`,
         bgShap = `blockGroup`)
trainDF3 <- trainDF2 %>% select(censusTract,CA,blockGroup) %>% 
  cbind(shapDF)

#shap tract
tractShapDF <- trainDF3 %>% group_by(censusTract) %>% 
  summarize(medianTractShap = median(tractShap)) %>%
  mutate(censusTract = as.character(censusTract)) %>% 
  left_join(tractMapDF5,by=c("censusTract"="GEOID"))

tractShapPlot <- ggplot() + geom_sf(data=tractShapDF, 
                                    aes(fill=medianTractShap,
                                        geometry=geometry)) +
  scale_fill_gradient2(low="#3E356BFF",
                       high="#49C1ADFF",
                       name="SHAP Value")+
  ggthemes::theme_map() + ggtitle("Tract SHAP") +
  theme(plot.title = element_text(size = 8, hjust = 0.5))

#shap CA
caShapDF <- trainDF3 %>% group_by(CA) %>% 
  summarize(medianCAShap = median(caShap)) %>%
  mutate(CA = factor(CA)) 

caShapDF <- nc3 %>% 
  left_join(caShapDF,by=c("AREA_NUM_1"="CA"))


caShapMap <- ggplot() + geom_sf(data=caShapDF, aes(fill=medianCAShap)) +
  scale_fill_gradient2(low="#3E356BFF",
                       high="#49C1ADFF",
                       name="SHAP",
                       breaks = c(-0.4,-0.2,0.0,0.2),
                       limits=c(-.5,0.25))+
  ggthemes::theme_map()
saveRDS(caShapMap,file="data/processed/caShapMap.rds")

#shap BG
bgShapDF <- trainDF3 %>% group_by(blockGroup) %>% 
  summarize(medianBGShap = median(bgShap)) %>%
  mutate(blockGroup = as.character(blockGroup)) %>% 
  left_join(acsCBG,by=c("blockGroup"="GEOID"))

ggplot() + geom_sf(data=bgShapDF, aes(fill=medianBGShap,
                                      geometry=geometry)) +
  scale_fill_gradient2(low="#3E356BFF",
                       high="#49C1ADFF",
                       name="SHAP Value")+
  ggthemes::theme_map()
