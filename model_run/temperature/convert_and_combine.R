zip_to_ZCTA <- read.csv('~/Desktop/HARVARD/Spring2022/IndStudy/model_run/temperature/data/Zip_to_ZCTA_crosswalk_2015_JSI.csv')
array <- readRDS('~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/zipcode_flood_semifinal_array_week_2000_2016.rds')

#merge conversion chart with lag array so that we have the ZCTA for each zip 
library(tidyverse)
converted_array <- left_join(as.data.frame(array), as.data.frame(zip_to_ZCTA[,c("ZIP","ZCTA")]), by = c("zipcode" = "ZIP"))

#missing zipcodes
zipcode_denom_missing <- readRDS('~/Desktop/HARVARD/Spring2022/IndStudy/medicare_processing/data_update/zipcodes_in_exposure_not_in_denom.rds')

library(stringr)
converted_array$zipcode <- str_pad(converted_array$zipcode, 5, pad = "0")

#remove zipcodes that we get rid of in medicare processing temporarily 
test <- converted_array[!(converted_array$zipcode %in% c(zipcode_denom_missing$zip,"35898", "36112","72314","87750",
                                                      "57542", "57647", "57778" ,"62845" ,"58320" ,"77507", "39529", "38912" ,"72516" ,"87009", "87117", "93262" ,"95836", "29808", 
                                                      "32212" ,"32815", "23459", "23461" ,"23521", "23709", "27859" ,"28310", "92135" ,"92155", "92862" ,"93042", "26674" ,"59354",
                                                      "48710" ,"55455" ,"29632" ,"58319" ,"72105" ,"80913" ,"60037", "00005" ,"56740", "76949", "31314" ,"36113" ,"04549", "06433", "19112" ,
                                                      "58705", "36615", "15275" ,"92152", "36515", "41351" ,"72199" ,"72329")),]
test$zipcode <- as.numeric(test$zipcode)

#check if all zipcodes match ZCTA (they don't)  
test$tested <- ifelse(test$zipcode == test$ZCTA, 0, 1)

#check which zipcodes do not have corresponding ZCTA's (list of 88) 
no_ZCTA <- test[is.na(test$ZCTA),]
no_ZCTA_zips <- sort(unique(no_ZCTA$zipcode))

#pull in RMP ZCTA files that I downloaded from Github: rmp15/PRISM-grids-into-FIPS-ZIP-censustract-USA 
dat.temp = data.frame()
dir.input.weather = "/Users/sarikaaggarwal/Desktop/HARVARD/Spring2022/IndStudy/model_run/temperature/data/RMP_ZCTA_files/"

state_names <- c(state.name, "D.C.")
state_names <- gsub(" ", "_", state_names)

#for each state, read in each yearly file and combine into one data frame
#then combine all states into one data frame and save for further use 
#note: we do not have data for alaska (important) and hawaii (not important)
library(plyr)
for (i in state_names){
  dir.current <- setwd(file.path(dir.input.weather, i))
  print(i)
  temp <- list.files(pattern="*.rds")
  myfiles <- lapply(temp, readRDS)
  df <- plyr::ldply(myfiles)
  #assign(paste0(i, "_df"), df)
  dat.temp <- rbind(dat.temp, df) 
}

saveRDS(dat.temp, "RMP_allZCTA_temp.rds")

