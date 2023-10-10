#Purpose: Loads lead data and geocodes addresses
library(readxl)
library(stringr)
library(dplyr)
library(censusxy)
library(rusps)
library(XML)
library(readr)
Results <- read_excel("data/Results.xlsx", 
                      skip = 2) #warnings are just empty notes at end of dataset
Results_sequential <- read_excel("data/Results.xlsx",
                                 sheet = "Sequential", skip = 2)
Results <- Results %>% 
  filter(!is.na(`Date Sampled`)) %>% 
  mutate(Address = toupper(Address))#removing 137 NA results
print(paste0(Results %>% filter(is.na(`2-3 Minute`)) %>% nrow(),
             " observations need to be matched with data from sequential spreadsheet."))

Results_sequential2 <- Results_sequential %>% 
  select(`Date Sampled`,Address,`1st Draw`,`3 Minute`,`5 Minute`) %>% 
  filter(!is.na(`Date Sampled`)) %>%  #removing 7 NA results
  mutate(Address = toupper(Address))

missingResults <- Results %>% filter(is.na(`2-3 Minute`))

Results2 <- Results %>% 
  filter(is.na(`2-3 Minute`)) %>%
  distinct() %>% 
  left_join(Results_sequential2,by=c("Date Sampled" = "Date Sampled",
                                     "Address" = "Address")) %>% 
  filter(!is.na(`3 Minute`)) %>% 
  mutate(`2-3 Minute` = `3 Minute`,
         `1st Draw.x` = `1st Draw.y`,
         `5 Minute.x` = `5 Minute.y`,
         sequential=T) %>% 
  select(-`3 Minute`,-`1st Draw.y`,-`5 Minute.y`) %>% 
  rename(`1st Draw` = `1st Draw.x`,
         `5 Minute` = `5 Minute.x`)
print(paste0(nrow(Results_sequential2)-nrow(Results2),
             " observations removed, can't match with sequential data."))

Results$sequential = F
Results <- Results %>% filter(!is.na(`2-3 Minute`)) %>% 
  rbind(Results2)

Results$Address <- str_replace(Results$Address, "XX", "30")
Results$Address <- str_remove_all(Results$Address, "\\*")
sampleDF <- data.frame(Results$Address,"Chicago","IL","")
colnames(sampleDF) <- c("Address","City","State","Zip")

df_sf <- cxy_geocode(sampleDF, street = "Address", 
                     city = "City", 
                     state = "State", 
                     zip = "Zip", 
                     class = "dataframe",
                     return="geographies",
                     vintage="Current_Current",
                     parallel=12)

df_sf2 <- df_sf %>% cbind(Results[-2])
write_csv(df_sf2,"data/processed/df1_GC.csv")

naDF <- subset(df_sf2,is.na(cxy_tract_id))
dim(naDF) #346 missing observations
naDF$Address2 <- sub("(\\d+)30(\\s+\\w+)", "\\149\\2", naDF$Address) #some are missing because address isn't residential, try other
naDF <- naDF %>% select(-5:-10)
naDF_GC <- cxy_geocode(naDF, street = "Address2", 
                     city = "City", 
                     state = "State", 
                     zip = "Zip", 
                     class = "dataframe",
                     return="geographies",
                     vintage="Current_Current",
                     parallel=12)
write_csv(naDF_GC,"data/processed/naDF_GC.csv")
naDF2 <- naDF_GC %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-16)
dim(naDF2) #200 missing
naDF2$Address2 <- sub("(\\d+)30(\\s+\\w+)", "\\107\\2", naDF2$Address) #some are missing because address isn't residential, try other
naDF_GC2 <- cxy_geocode(naDF2, street = "Address2", 
                       city = "City", 
                       state = "State", 
                       zip = "Zip", 
                       class = "dataframe",
                       return="geographies",
                       vintage="Current_Current",
                       parallel=12)
write_csv(naDF_GC2,"data/processed/naDF_GC2.csv")

naDF3 <- naDF_GC2 %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-16)
dim(naDF3) #190 missing
naDF3$Address2 <- sub("(\\d+)30(\\s+\\w+)", "\\100\\2", naDF3$Address) #some are missing because address isn't residential, try other
naDF_GC3 <- cxy_geocode(naDF3, street = "Address2", 
                        city = "City", 
                        state = "State", 
                        zip = "Zip", 
                        class = "dataframe",
                        return="geographies",
                        vintage="Current_Current",
                        parallel=12)
write_csv(naDF_GC3,"data/processed/naDF_GC3.csv")

naDF4 <- naDF_GC3 %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-16)
dim(naDF4) #186 missing
naDF4$Address2 <- sub("(\\d+)30(\\s+\\w+)", "\\150\\2", naDF4$Address) #some are missing because address isn't residential, try other
naDF_GC4 <- cxy_geocode(naDF4, street = "Address2", 
                        city = "City", 
                        state = "State", 
                        zip = "Zip", 
                        class = "dataframe",
                        return="geographies",
                        vintage="Current_Current",
                        parallel=12)
write_csv(naDF_GC4,"data/processed/naDF_GC4.csv")

naDF5 <- naDF_GC4 %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-16)
dim(naDF5) #180 missing

#fixing typos in the data entry
naDF5$Address <- str_replace(naDF5$Address, "100030 S OAKLEY AVE", "10030 S OAKLEY AVE")
naDF5$Address <- str_replace(naDF5$Address, "22330 N LINCOLN AVE", "2230 N LINCOLN AVE")
naDF5$Address <- str_replace(naDF5$Address, "28330 N SAWYER AVE", "2830 N SAWYER AVE")
naDF5$Address <- str_replace(naDF5$Address, "6730 N ASHLAND AVE 6723 N ASHLAND AVE", "6723 N ASHLAND AVE")
naDF5$Address <- str_replace(naDF5$Address, "10430 S M ST", "10430 S Avenue M")
naDF5$Address <- str_replace(naDF5$Address, "10930 S G DR", "10930 S Avenue G")
naDF5$Address <- str_replace(naDF5$Address, "13330 S L AVE", "13330 S Avenue L")
naDF5$Address <- str_replace(naDF5$Address, "10630 S AVENUE E ST", "10630 S AVENUE E")
naDF5$Address2 <- naDF5$Address
naDF5$Address2 <- sub("^X", "7", naDF5$Address) #single digit addresses
naDF5 <- naDF5 %>% filter(!is.na(Address))
naDF_GC5 <- cxy_geocode(naDF5, street = "Address2", 
                        city = "City", 
                        state = "State", 
                        zip = "Zip", 
                        class = "dataframe",
                        return="geographies",
                        vintage="Current_Current")
write_csv(naDF_GC5,"data/processed/naDF_GC5.csv")

naDF6 <- naDF_GC5 %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-16) %>% filter(!is.na(Address))
dim(naDF6) #155 missing

naDF6$number <- as.numeric(gsub("\\D", "", naDF6$Address))
naDF6$street <- gsub("\\d+", "", naDF6$Address)
naDF6$streetName <- gsub("^(\\d+ )?([NEWS]) (.*)$", "\\3", naDF6$Address)

naDF6$is_letter <- (str_length(naDF6$streetName)==1)
naDF6 <- naDF6 %>% 
  mutate(Address2 = ifelse(is_letter==1,
                           paste0(number," S avenue ",
                                  streetName),
                           Address2))
naDF_GC6 <- cxy_geocode(naDF6, street = "Address2", 
                        city = "City", 
                        state = "State", 
                        zip = "Zip", 
                        class = "dataframe",
                        return="geographies",
                        vintage="Current_Current")
write_csv(naDF_GC6,"data/processed/naDF_GC6.csv")
naDF7 <- naDF_GC6 %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-20) %>% filter(!is.na(Address))
dim(naDF7) #155 missing
naDF7$Address <- gsub("CREIGER", "CREGIER", naDF7$Address)
naDF7$Address <- gsub("101STST", "101ST ST", naDF7$Address)
naDF7$Address <- gsub("AVE GARDEN", "AVE", naDF7$Address)
naDF7$Address <- gsub("AVE UNIT 2", "AVE", naDF7$Address)
naDF7$Address <- gsub("AVE 3W", "AVE", naDF7$Address)
naDF7$Address <- gsub("AVE UNIT 2", "AVE", naDF7$Address)
naDF7$Address <- gsub("AVE UNIT 2", "AVE", naDF7$Address)
naDF7$Address <- gsub("N LINCOLN PARK", "N LINCOLN PARK W", naDF7$Address)
naDF7$Address <- gsub("10430 S RACINE AVE", "10401 S RACINE AVE", naDF7$Address)
naDF7$Address <- gsub("10730 S AVENUE", "10730 S AVENUE M", naDF7$Address)
naDF7$Address <- gsub("13430 W ESTES AVE", "1340 W ESTES AVE", naDF7$Address) #13430 W ESTES AVE doesn't exist, assumed incorrect # of digits
naDF7$Address <- gsub("1730 W WALTER BURLEY GRIFFIN PL", "1730 W 104TH PL", naDF7$Address) #griffin pl is listed as 104th pl
naDF7$Address <- gsub("1830 S FEDERAL ST", "1902 S FEDERAL ST", naDF7$Address) #closest residential building that census recognizes
naDF7$Address <- gsub("STAVEST", "STAVE ST", naDF7$Address)
naDF7$Address <- gsub("2430 W HOWARD ST", "2505 W HOWARD ST", naDF7$Address) #closest residential building that census recognizes
naDF7$Address <- gsub("ST D407", "ST", naDF7$Address)
naDF7$Address <- gsub("71STST", "71ST ST", naDF7$Address)
naDF7$Address <- gsub("W PACIFIC", "N PACIFIC", naDF7$Address)
naDF7$Address <- gsub("81ST", "81ST ST", naDF7$Address)
naDF7$Address <- gsub("MERRIAC", "MERRIMAC", naDF7$Address)
naDF7$Address <- gsub("SPAULING", "Spaulding", naDF7$Address)
naDF7$Address <- gsub("N CENTRAL PARK", "N CENTRAL PARK AVE", naDF7$Address)
naDF7$Address <- gsub("MELROSE 8B ST", "MELROSE ST", naDF7$Address)
naDF7$Address <- gsub("ONTARIO #1706 ST", "ONTARIO ST", naDF7$Address)
naDF7$Address <- gsub("LAULAM AVE", "LUDLAM AVE", naDF7$Address)
naDF7$Address <- gsub("N WEST CIRCLE", "N West Circle Ave", naDF7$Address)
naDF7$Address <- gsub("5830 N BROADWAY", "5832 N BROADWAY", naDF7$Address)
naDF7$Address <- gsub("5830 S WENTWORTH AVE", "5827 S WENTWORTH AVE", naDF7$Address)
naDF7$Address <- gsub("530 E 91STPL", "530 E 91ST PL", naDF7$Address)
naDF7$Address <- gsub("6130 W ARLINGTON PL", "613 W ARLINGTON PL", naDF7$Address)
naDF7$Address <- gsub("6130 W ARLINGTON PL", "613 W ARLINGTON PL", naDF7$Address)
naDF7$Address <- gsub("RD 2G", "RD", naDF7$Address)
naDF7$Address <- gsub("PAULINA ST ST", "PAULINA ST", naDF7$Address)
naDF7$Address <- gsub("6430 N. SAYRE AVE. HOUSE", "6430 N SAYRE AVE", naDF7$Address)
naDF7$Address <- gsub("PAULINA ST ST", "PAULINA ST", naDF7$Address)
naDF7$Address <- gsub("6830 N CENTRAL AVE", "6908 N CENTRAL AVE", naDF7$Address)
naDF7$Address <- gsub("CLARK ST 2501", "CLARK ST", naDF7$Address)
naDF7$Address <- gsub("8330 S STATE ST", "8329 S STATE ST", naDF7$Address)
naDF7$Address <- gsub("830 E 101ST ST", "852 E 101ST ST", naDF7$Address)
naDF7$Address <- gsub("830 E 101ST ST", "852 E 101ST ST", naDF7$Address)
naDF7$Address <- gsub("DR MARTIN LUTHER KING JR DR", "MARTIN LUTHER KING JR DR", naDF7$Address)
naDF7$Zip <- as.character(naDF7$Zip)
naDF7$Zip[naDF7$Address=="1330 W CRYSTAL ST"] <- "60622"
naDF7$Zip[naDF7$Address=="4730 W BELLE PLAINE AVE"] <- "60641"
naDF7$Zip[naDF7$Address=="1330 W EVERGREEN AVE"] <- "60642"
naDF7$Zip[naDF7$Address=="1030 W 129TH PL"] <- "60643"
naDF7$Zip[naDF7$Address=="530 W QUINCY ST"] <- "60661"
naDF7$Zip[naDF7$Address=="130 N GARLAND CT"] <- "60602"
naDF7$Zip[naDF7$Address=="4130 N KEDVALE AVE"] <- "60641"
naDF7$Zip[naDF7$Address=="3930 W ROSEMONT AVE"] <- "60659"

naDF7$Address2 <- naDF7$Address
naDF_GC7 <- cxy_geocode(naDF7, street = "Address2", 
                        city = "City", 
                        state = "State", 
                        zip = "Zip", 
                        class = "dataframe",
                        return="geographies",
                        vintage="Current_Current")
write_csv(naDF_GC7,"data/processed/naDF_GC7.csv")
naDF8 <- naDF_GC7 %>% 
  filter(is.na(cxy_tract_id)) %>%
  select(-11:-16) %>% filter(!is.na(Address))
dim(naDF8) #130 missing total

naDF_GC6 <- naDF_GC6 %>% select(-11:-14) #remove regex columns before merging

fullDF <- rbind(naDF_GC,naDF_GC2,naDF_GC3,naDF_GC4,
                naDF_GC5,naDF_GC6,naDF_GC7) %>% 
  filter(!is.na(cxy_tract_id)) %>% select(-Address2)

fullDF2 <- rbind(df_sf2,fullDF) %>% 
  filter(!is.na(cxy_tract_id)) %>% 
  select(-Zip)
  
write_csv(fullDF2,"data/processed/leadExposure_complete.csv")
