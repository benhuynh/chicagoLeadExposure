
tractsToCA <- read_csv("data/2020 Census Tracts to Chicago Community Area Equivalency File - Sheet1.csv")
tractsToCA$COMMUNIT_1 <- toupper(tractsToCA$COMMUNIT_1)
tractsToCA <- tractsToCA %>% 
  select(-GEOID20) %>% distinct()
waterFilterDF <- read_excel("data/Benjamin Huynh water source request from 11-8-2023.xlsx",sheet = "community area", skip = 3)

caNum <- left_join(waterFilterDF,tractsToCA,
                   by=c("COMMUNITY AREA"="COMMUNIT_1"))
caNum$CA[76] <- 76

colnames(caNum) <-
  c("communityArea",
    "frequency_BW",
    "weightedFrequency_BW",
    "percent_BW",
    "lower_BW",
    "upper_BW",
    "RSE_BW",
    "frequency_Filtered",
    "weightedFrequency_Filtered",
    "percent_Filtered",
    "lower_Filtered",
    "upper_Filtered",
    "RSE_Filtered",
    "frequency_Unfiltered",
    "weightedFrequency_Unfiltered",
    "percent_Unfiltered",
    "lower_Unfiltered",
    "upper_Unfiltered",
    "RSE_Unfiltered",
    "CA"
    )
#match with CA numbers to make sure correct
#
write_csv(caNum,"data/processed/hcsResults.csv")

nc = st_read("data/Comm_20Areas__1_/CommAreas.shp")
caNum$CA <- factor(caNum$CA)

caNum2 <- caNum %>% left_join(nc,by=c("CA"="AREA_NUM_1"))

caFilterPlot <- ggplot() + 
  geom_sf(data=caNum, aes(fill=percent_Unfiltered,
                          geometry=geometry)) +
  scale_fill_distiller(type="seq",palette=9,direction=1,
                       name="% Unfiltered tap water usage",
                       
  ) + 
  ggthemes::theme_map() + ggtitle("Unfiltered tap water usage")
  
  
