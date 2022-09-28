# this code will take the flood data
# then process it for a model input
# this is for merging days into weeks 


#rm(list=ls())

# break down the arguments from Rscript
#args <- commandArgs(trailingOnly=TRUE)
#start_year <- as.numeric(args[1])
#end_year = as.numeric(args[2])

start_year <- as.numeric("2000")
end_year = as.numeric("2014")

# load necessary packages
library('tidyverse') ; library('lubridate')

setwd('~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/')
dir.output<- '~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/'
county_flood_data <- readRDS('county_flood_master_2000_2014.rds')

county_flood_data <- county_flood_data[,c(1,6:8,2:5,10:11,9,12:14)]
# county_flood_data$total_duration <- ifelse(county_flood_data$total_duration > 14, 
#                                            14, county_flood_data$total_duration)

#county_flood_data$county_fips <- as.numeric(county_flood_data$county_fips)

#remove floods caused by dams and snowmelt 
county_flood_data_no_dams_snowmelt <- county_flood_data[!(county_flood_data$main_cause == "Dam") 
                                                        & !(county_flood_data$main_cause == "Snowmelt, Ice, Rain"),]




# to obtain the number of unique counties (2934 counties total, 2902 counties subset)
all_counties = unique(county_flood_data_no_dams_snowmelt$county_fips)

#to determine number of states included, use USA_table (should be all except Hawaii)

list_df <- list()
names_of_df <- c()
names_col_df1 <- c("county_fips", "date", "year", "month", "day")
names_col_df2 <- c("event_exposed1", "event_exposed2", "event_exposed3", "event_exposed4", 
                   "event_exposed5", "event_exposed6", "event_exposed7", "event_exposed8", 
                   "event_exposed9", "event_exposed10", "event_exposed11", "event_exposed12", 
                   "event_exposed13", "event_exposed14","event_exposed15", "event_exposed16", 
                   "event_exposed17", "event_exposed18","event_exposed19", "event_exposed20", 
                   "event_exposed21", "event_exposed22", "event_exposed23", "event_exposed24", 
                   "event_exposed25", "event_exposed26","event_exposed27", "event_exposed28",
                   "event_exposed29", "event_exposed30", "event_exposed31")

names_col_df3 <- c("event_lag1", "event_lag2", "event_lag3", "event_lag4",
                             "event_lag5", "event_lag6", "event_lag7", "event_lag8",
                             "event_lag9", "event_lag10", "event_lag11", "event_lag12",
                             "event_lag13", "event_lag14")
for (i in 1:nrow(county_flood_data_no_dams_snowmelt)){
  df1 <- data.frame(matrix(NA, nrow = county_flood_data_no_dams_snowmelt$total_duration[i] + 14, ncol = 5))
  colnames(df1) <- names_col_df1
  list_df[[i]] <- df1
  name <- paste0(county_flood_data_no_dams_snowmelt$id[i],'_flood_df_',county_flood_data_no_dams_snowmelt$county_fips[i])
  names_of_df <- append(names_of_df, name)
}

names(list_df) <- names_of_df

for (i in 1:length(list_df)){
  list_df[[i]]$county_fips <- rep(county_flood_data_no_dams_snowmelt$county_fips[i], county_flood_data_no_dams_snowmelt$total_duration[i] + 14)
  dates <- c()
  for (j in 0:(nrow(list_df[[i]]) - 1)){
    temp_date <- as.Date(county_flood_data_no_dams_snowmelt$start[i], format="%Y-%m-%d") + j
    dates <- append(dates, temp_date)
  }
  list_df[[i]]$date <- dates 
  list_df[[i]]$year <- lubridate::year(dates)
  list_df[[i]]$month <- lubridate::month(dates)
  list_df[[i]]$day <- lubridate::day(dates)
  #if we cut exposure off at 14 days, we would have ncol = 14 in df2 
  df2 <- data.frame(matrix(data = NA, nrow = county_flood_data_no_dams_snowmelt$total_duration[i] + 14, ncol = 31))
  colnames(df2) <- names_col_df2
  for (k in 1:county_flood_data_no_dams_snowmelt$total_duration[i]){
    df2[k,k] <- 1
  }
  df3 <- data.frame(matrix(data = NA, nrow = county_flood_data_no_dams_snowmelt$total_duration[i] + 14, ncol = 14))
  colnames(df3) <- names_col_df3
  list_df[[i]] <- cbind(list_df[[i]], df2, df3)
  for (l in c(1:14)){
    list_df[[i]][county_flood_data_no_dams_snowmelt$total_duration[i] + l, ncol(df1) + ncol(df2) + l] <- 1
  }
  list_df[[i]]$date <- NULL 
}

county_flood_edit_array_week <- plyr::ldply(list_df, data.frame)
names(county_flood_edit_array_week)[names(county_flood_edit_array_week) == ".id"] <- "floodcounty_id"
county_flood_edit_array_week[is.na(county_flood_edit_array_week)] <- 0

#add indicators for exposed weeks and lag weeks 
#consider rewriting using tidyverse::case_when
county_flood_edit_array_week$event_exposed <- ifelse(county_flood_edit_array_week$event_exposed1 == 1 | 
                        county_flood_edit_array_week$event_exposed2 == 1 | county_flood_edit_array_week$event_exposed3 == 1 | 
                        county_flood_edit_array_week$event_exposed4 == 1 | county_flood_edit_array_week$event_exposed5 == 1 | 
                        county_flood_edit_array_week$event_exposed6 == 1 | county_flood_edit_array_week$event_exposed7 == 1 | 
                        county_flood_edit_array_week$event_exposed8 == 1 | county_flood_edit_array_week$event_exposed9 == 1 | 
                        county_flood_edit_array_week$event_exposed10 == 1 | county_flood_edit_array_week$event_exposed11 == 1 | 
                        county_flood_edit_array_week$event_exposed12 == 1 | county_flood_edit_array_week$event_exposed13 == 1 | 
                        county_flood_edit_array_week$event_exposed14 == 1 | county_flood_edit_array_week$event_exposed15 == 1 | 
                        county_flood_edit_array_week$event_exposed16 == 1 | county_flood_edit_array_week$event_exposed17 == 1 | 
                        county_flood_edit_array_week$event_exposed18 == 1 | county_flood_edit_array_week$event_exposed19 == 1 | 
                        county_flood_edit_array_week$event_exposed20 == 1 | county_flood_edit_array_week$event_exposed21 == 1 | 
                        county_flood_edit_array_week$event_exposed22 == 1 | county_flood_edit_array_week$event_exposed23 == 1 | 
                        county_flood_edit_array_week$event_exposed24 == 1 | county_flood_edit_array_week$event_exposed25 == 1 | 
                        county_flood_edit_array_week$event_exposed26 == 1 | county_flood_edit_array_week$event_exposed27 == 1 | 
                        county_flood_edit_array_week$event_exposed28 == 1 | county_flood_edit_array_week$event_exposed29 == 1 | 
                        county_flood_edit_array_week$event_exposed30 == 1 | county_flood_edit_array_week$event_exposed31 == 1, 1, 0)
    
county_flood_edit_array_week$event_lagwk1 <- ifelse(county_flood_edit_array_week$event_lag1 == 1 | county_flood_edit_array_week$event_lag2 == 1 |
                        county_flood_edit_array_week$event_lag3 == 1 | county_flood_edit_array_week$event_lag4 == 1 |
                        county_flood_edit_array_week$event_lag5 == 1 | county_flood_edit_array_week$event_lag6 == 1 |
                        county_flood_edit_array_week$event_lag7 == 1, 1, 0)
county_flood_edit_array_week$event_lagwk2 <- ifelse(county_flood_edit_array_week$event_lag8 == 1 | county_flood_edit_array_week$event_lag9 == 1 |
                        county_flood_edit_array_week$event_lag10 == 1 | county_flood_edit_array_week$event_lag11 == 1 |
                        county_flood_edit_array_week$event_lag12 == 1 | county_flood_edit_array_week$event_lag13 == 1 |
                        county_flood_edit_array_week$event_lag14 == 1, 1, 0)

#merge with medicare data here by fips, year, month, day 
set.seed(2022)
county_flood_edit_array_week$sample_outcome <- sample(seq(1:250000), nrow(county_flood_edit_array_week), replace = TRUE)


county_flood_edit_array_week_aggregated_outcome <- county_flood_edit_array_week %>% group_by(floodcounty_id) %>% 
                                                summarise(exposed = sum(sample_outcome * event_exposed),
                                                          lag_wk1 = sum(sample_outcome * event_lagwk1),
                                                          lag_wk2 = sum(sample_outcome * event_lagwk2))

#merge the aggregated outcomes by flood-county id and remove all daily exposure/lag indicators 
county_flood_final_array_week <- merge(county_flood_edit_array_week, county_flood_edit_array_week_aggregated_outcome, 
                                       by = "floodcounty_id")

#we can decide whether to keep in flood-county id (currently removing)
county_flood_final_array_week <- county_flood_final_array_week[-c(1, 6:50)]

# save as rds for analysis
#saveRDS(county_flood_final_array_week, paste0(dir.output,'county_flood_lag_array_week_',start_year,'_',end_year,'.rds'))
