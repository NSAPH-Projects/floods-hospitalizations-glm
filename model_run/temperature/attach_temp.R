#load in temperature data 
print('attaching temperature data')
dat.temp <- readRDS('/Users/sarikaaggarwal/Desktop/HARVARD/Spring2022/IndStudy/model_run/temperature/data/RMP_allZCTA_temp.rds')

#for zipcodes that we do not have corresponding ZCTA's or temperature data (example: Alaska), 
#keep rows in merged array; however, they wil have NA's (just to figure out aggregation)

zip_to_ZCTA <- read.csv('~/Desktop/HARVARD/Spring2022/IndStudy/model_run/temperature/data/Zip_to_ZCTA_crosswalk_2015_JSI.csv')
array <- readRDS('~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/zipcode_flood_semifinal_array_week_2000_2016.rds')

#merge conversion chart with lag array so that we have the ZCTA for each zip 
library(tidyverse)
converted_array <- left_join(as.data.frame(array), as.data.frame(zip_to_ZCTA[,c("ZIP","STATE", "ZCTA")]), by = c("zipcode" = "ZIP"))

dat.temp$zcta <- as.numeric(as.character(dat.temp$zcta))
dat.temp$day <- as.numeric(dat.temp$day)
dat.temp$month <- as.numeric(dat.temp$month)

#merge temperature data with array 
library(data.table)
zip_to_ZCTA_temp <- data.table::merge.data.table(as.data.table(converted_array), 
                                                 as.data.table(dat.temp[,c("zcta","tmean", "day", "month", "year")]), 
                                                 by.x = c("ZCTA", "day", "month", "year"), 
                                                 by.y = c("zcta", "day", "month", "year"), all.x = TRUE, all.y = FALSE)

zip_to_ZCTA_temp_df <- as.data.frame(zip_to_ZCTA_temp)

zip_to_ZCTA_temp_df <- arrange(zip_to_ZCTA_temp_df, floodzip_id, control_indicator, year, month, day )
zip_to_ZCTA_temp_df <- zip_to_ZCTA_temp_df[,c(5,6,1,18,4,3,2,7:17,19)]

aggregated_temp <- zip_to_ZCTA_temp_df %>% group_by(floodzip_id, control_indicator) %>%
  summarise(exp_temp = sum(tmean * event_exposed)/sum(event_exposed),
            exp_lag_wk1_temp = sum(tmean * event_lagwk1)/7,
            exp_lag_wk2_temp = sum(tmean * event_lagwk2)/7,
            exp_lag_wk3_temp = sum(tmean * event_lagwk3)/7,
            exp_lag_wk4_temp = sum(tmean * event_lagwk4)/7,
            control_temp = sum(tmean * control_exposed)/sum(control_exposed),
            control_lag_wk1_temp = sum(tmean * control_lagwk1)/7,
            control_lag_wk2_temp = sum(tmean * control_lagwk2)/7,
            control_lag_wk3_temp = sum(tmean * control_lagwk3)/7,
            control_lag_wk4_temp = sum(tmean * control_lagwk4)/7)

dat.aggregated.temp.update <- matrix(data = NA, nrow = 5*nrow(aggregated_temp), ncol = 1)
k <- 1
for (i in 1:nrow(aggregated_temp)){
  if (aggregated_temp$control_indicator[i] == 0){
    temp_agg <- aggregated_temp[i,c(3:7)]
  }
  else if (aggregated_temp$control_indicator[i] == 1){
    temp_agg <- aggregated_temp[i,c(8:12)]
  }
  else if (aggregated_temp$control_indicator[i] == 2){
    temp_agg <- aggregated_temp[i,c(8:12)]
  }
  else {
    break 
  }
  temp_agg <- t(temp_agg)
  dat.aggregated.temp.update[k:(k + 4),] <- temp_agg
  k <- k + 5 
}

dat.aggregated.temp.update <- as.data.frame(dat.aggregated.temp.update)
dat.aggregated.temp.update$floodzip_id <- rep(aggregated_temp$floodzip_id, each = 5)
dat.aggregated.temp.update$control_indicator <- rep(aggregated_temp$control_indicator, each = 5)
colnames(dat.aggregated.temp.update) <- c("temp", "floodzip_id", "control_indicator")
dat.aggregated.temp.update <- dat.aggregated.temp.update[,c(2,3,1)]

#we could then remove zipcodes in zipcode_denom_missing and other list (and Alaska if we continue with this dataset)
#then merge in "model run file" with dat.sample 


