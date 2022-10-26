# this code will take the flood data
# then process it for a model input
# this is for merging days into weeks 
# with zipcodes 


#rm(list=ls())

# break down the arguments from Rscript
#args <- commandArgs(trailingOnly=TRUE)
#start_year <- as.numeric(args[1])
#end_year = as.numeric(args[2])

start_year <- as.numeric("2000")
end_year = as.numeric("2018")

# load necessary packages
library('tidyverse') ; library('lubridate')

setwd('~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/')
dir.output<- '~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/'
zipcode_flood_data <- readRDS('zipcode_flood_master_2000_2018.rds')

zipcode_flood_data <- zipcode_flood_data[,c(1,6:8,2:5,10:11,9,12:14)]
# zipcode_flood_data$total_duration <- ifelse(zipcode_flood_data$total_duration > 14, 
#                                            14, zipcode_flood_data$total_duration)

# zipcode_flood_data$zipcode <- as.numeric(zipcode_flood_data$zipcode)

#remove floods caused by dams and snowmelt 
zipcode_flood_data_no_dams_snowmelt <- zipcode_flood_data[!(zipcode_flood_data$main_cause == "Dam") 
                                                        & !(zipcode_flood_data$main_cause == "Snowmelt, Ice, Rain"),]

saveRDS(zipcode_flood_data_no_dams_snowmelt, "zipcode_flood_master_no_dams_snowmelt_2000_2018.rds")


# to obtain the number of unique zips ( sips total,  zips subset)
all_zips = as.numeric(unique(zipcode_flood_data_no_dams_snowmelt$zipcode))

#to determine number of states included, use USA_table (should be all except Hawaii)

list_df <- list()
names_of_df <- c()
names_col_df1 <- c("zipcode", "date", "year", "month", "day")
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
                    "event_lag13", "event_lag14", "event_lag15", "event_lag16",
                   "event_lag17", "event_lag18", "event_lag19", "event_lag20",
                   "event_lag21", "event_lag22", "event_lag23", "event_lag24",
                   "event_lag25", "event_lag26", "event_lag27", "event_lag28")

#change in code from 31 to max_duration 
max_duration <- max(zipcode_flood_data_no_dams_snowmelt$total_duration)

#change in code from 28 to be 7*num_lagweeks
num_lagweeks <- 4
for (i in 1:nrow(zipcode_flood_data_no_dams_snowmelt)){
  df1 <- data.frame(matrix(NA, nrow = zipcode_flood_data_no_dams_snowmelt$total_duration[i] + 7*num_lagweeks, ncol = 5))
  colnames(df1) <- names_col_df1
  list_df[[i]] <- df1
  name <- paste0(zipcode_flood_data_no_dams_snowmelt$id[i],'_flood_df_',zipcode_flood_data_no_dams_snowmelt$zipcode[i])
  names_of_df <- append(names_of_df, name)
}

names(list_df) <- names_of_df

for (i in 1:length(list_df)){
  list_df[[i]]$zipcode <- rep(zipcode_flood_data_no_dams_snowmelt$zipcode[i], zipcode_flood_data_no_dams_snowmelt$total_duration[i] + 7*num_lagweeks)
  dates <- c()
  for (j in 0:(nrow(list_df[[i]]) - 1)){
    temp_date <- as.Date(zipcode_flood_data_no_dams_snowmelt$start[i], format="%Y-%m-%d") + j
    dates <- append(dates, temp_date)
  }
  list_df[[i]]$date <- dates 
  list_df[[i]]$year <- lubridate::year(dates)
  list_df[[i]]$month <- lubridate::month(dates)
  list_df[[i]]$day <- lubridate::day(dates)
  #if we cut exposure off at 14 days, we would have ncol = 14 in df2 
  df2 <- data.frame(matrix(data = NA, nrow = zipcode_flood_data_no_dams_snowmelt$total_duration[i] + 7*num_lagweeks, ncol = max_duration))
  colnames(df2) <- names_col_df2
  for (k in 1:zipcode_flood_data_no_dams_snowmelt$total_duration[i]){
    df2[k,k] <- 1
  }
  df3 <- data.frame(matrix(data = NA, nrow = zipcode_flood_data_no_dams_snowmelt$total_duration[i] + 7*num_lagweeks, ncol = 7*num_lagweeks))
  colnames(df3) <- names_col_df3
  list_df[[i]] <- cbind(list_df[[i]], df2, df3)
  for (l in c(1:(7*num_lagweeks))){
    list_df[[i]][zipcode_flood_data_no_dams_snowmelt$total_duration[i] + l, ncol(df1) + ncol(df2) + l] <- 1
  }
  #list_df[[i]]$date <- NULL 
}

zipcode_flood_edit_array_week <- plyr::ldply(list_df, data.frame)
names(zipcode_flood_edit_array_week)[names(zipcode_flood_edit_array_week) == ".id"] <- "floodzip_id"
zipcode_flood_edit_array_week[is.na(zipcode_flood_edit_array_week)] <- 0

#add indicators for exposed weeks and lag weeks 
#consider rewriting using tidyverse::case_when
zipcode_flood_edit_array_week$event_exposed <- ifelse(zipcode_flood_edit_array_week$event_exposed1 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed2 == 1 | zipcode_flood_edit_array_week$event_exposed3 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed4 == 1 | zipcode_flood_edit_array_week$event_exposed5 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed6 == 1 | zipcode_flood_edit_array_week$event_exposed7 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed8 == 1 | zipcode_flood_edit_array_week$event_exposed9 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed10 == 1 | zipcode_flood_edit_array_week$event_exposed11 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed12 == 1 | zipcode_flood_edit_array_week$event_exposed13 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed14 == 1 | zipcode_flood_edit_array_week$event_exposed15 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed16 == 1 | zipcode_flood_edit_array_week$event_exposed17 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed18 == 1 | zipcode_flood_edit_array_week$event_exposed19 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed20 == 1 | zipcode_flood_edit_array_week$event_exposed21 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed22 == 1 | zipcode_flood_edit_array_week$event_exposed23 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed24 == 1 | zipcode_flood_edit_array_week$event_exposed25 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed26 == 1 | zipcode_flood_edit_array_week$event_exposed27 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed28 == 1 | zipcode_flood_edit_array_week$event_exposed29 == 1 | 
                        zipcode_flood_edit_array_week$event_exposed30 == 1 | zipcode_flood_edit_array_week$event_exposed31 == 1, 1, 0)
    
zipcode_flood_edit_array_week$event_lagwk1 <- ifelse(zipcode_flood_edit_array_week$event_lag1 == 1 | zipcode_flood_edit_array_week$event_lag2 == 1 |
                        zipcode_flood_edit_array_week$event_lag3 == 1 | zipcode_flood_edit_array_week$event_lag4 == 1 |
                        zipcode_flood_edit_array_week$event_lag5 == 1 | zipcode_flood_edit_array_week$event_lag6 == 1 |
                        zipcode_flood_edit_array_week$event_lag7 == 1, 1, 0)
zipcode_flood_edit_array_week$event_lagwk2 <- ifelse(zipcode_flood_edit_array_week$event_lag8 == 1 | zipcode_flood_edit_array_week$event_lag9 == 1 |
                        zipcode_flood_edit_array_week$event_lag10 == 1 | zipcode_flood_edit_array_week$event_lag11 == 1 |
                        zipcode_flood_edit_array_week$event_lag12 == 1 | zipcode_flood_edit_array_week$event_lag13 == 1 |
                        zipcode_flood_edit_array_week$event_lag14 == 1, 1, 0)
zipcode_flood_edit_array_week$event_lagwk3 <- ifelse(zipcode_flood_edit_array_week$event_lag15 == 1 | zipcode_flood_edit_array_week$event_lag16 == 1 |
                        zipcode_flood_edit_array_week$event_lag17 == 1 | zipcode_flood_edit_array_week$event_lag18 == 1 |
                        zipcode_flood_edit_array_week$event_lag19 == 1 | zipcode_flood_edit_array_week$event_lag20 == 1 |
                        zipcode_flood_edit_array_week$event_lag21 == 1, 1, 0)
zipcode_flood_edit_array_week$event_lagwk4 <- ifelse(zipcode_flood_edit_array_week$event_lag22 == 1 | zipcode_flood_edit_array_week$event_lag23 == 1 |
                        zipcode_flood_edit_array_week$event_lag24 == 1 | zipcode_flood_edit_array_week$event_lag25 == 1 |
                        zipcode_flood_edit_array_week$event_lag26 == 1 | zipcode_flood_edit_array_week$event_lag27 == 1 |
                        zipcode_flood_edit_array_week$event_lag28 == 1, 1, 0)

#can remove date and flood-zipcode id columns later 
#remove exposure + lag day indicators 
zipcode_flood_exp_lag_array_week  <- zipcode_flood_edit_array_week[-c(7:65)]
zipcode_flood_exp_lag_array_week$zipcode <- as.numeric(zipcode_flood_exp_lag_array_week$zipcode)

floodzip_id_list <- unique(zipcode_flood_exp_lag_array_week$floodzip_id)

#min date is first date of exposure and max date is last day of 4th lag week 
zipcode_flood_summarized_date_array <- zipcode_flood_exp_lag_array_week %>%
                                 group_by(floodzip_id) %>% 
                                 summarise('min_date' = min(date),
                                           'max_date' = max(date))

zipcode_flood_summarized_date_array$zipcode <- as.numeric(substr(zipcode_flood_summarized_date_array$floodzip_id, 15, 20))


#for each year, change the year to be a different year in dataset then compare each control to original set of floods within that specific zipcode
#if overlaps = 0, if no overlap = 1 --> pick closest to the year of flood and then add all dates within bound to array
#for these floods, we did not have to consider any other years besides previous and after (might be different when we expand years)

control_dates_df <- c()
i <- 1
  for (j in all_zips){
    same_zip_floods <- zipcode_flood_summarized_date_array %>% 
                          filter(zipcode == j)
    for (k in 1:nrow(same_zip_floods)){
      min_date_temp <- control1_min_date <- control2_min_date <- same_zip_floods$min_date[k]
      max_date_temp <- control1_max_date <- control2_max_date <- same_zip_floods$max_date[k]
      id_temp <- same_zip_floods$floodzip_id[k]
      same_zip_floods_wo_rowk <- same_zip_floods %>% 
                             filter(!row_number() %in% k)
      year(control1_min_date) <- year(min_date_temp) - i
      year(control1_max_date) <- year(max_date_temp) - i
      year(control2_min_date) <- year(min_date_temp) + i
      year(control2_max_date) <- year(max_date_temp) + i 
      #if overlap exists, mark as 0 (if not, mark as 1)
      overlaps1 <- ifelse(control1_min_date <= same_zip_floods_wo_rowk$max_date & same_zip_floods_wo_rowk$min_date <= control1_max_date, 0, 1)
      overlaps2 <- ifelse(control2_min_date <= same_zip_floods_wo_rowk$max_date & same_zip_floods_wo_rowk$min_date <= control2_max_date, 0, 1)
      #if any value in overlap vector is 0, mark as 0 (i.e. cannot be counted as a control)
      any_overlaps_control1 <- ifelse(any(overlaps1 == 0), 0, 1)
      any_overlaps_control2 <- ifelse(any(overlaps2 == 0), 0, 1)
      if (any_overlaps_control1 == 1){
        control1_dates <- c(control1_min_date, control1_max_date)
      }
      if (any_overlaps_control2 == 1){
        control2_dates <- c(control2_min_date, control2_max_date)
      }
      # if (any_overlaps_control1 == 0 | any_overlaps_control1 == 0){
      #   i <- i + 1
      # }
      final_row <- c(id_temp, control1_dates, control2_dates)
      control_dates_df <- rbind(control_dates_df, final_row)
    }
  } 

control_dates_df <- as.data.frame(control_dates_df)
control_dates_df$V2 <- as.Date(as.numeric(control_dates_df$V2), origin = "1970-01-01")
control_dates_df$V3 <- as.Date(as.numeric(control_dates_df$V3), origin = "1970-01-01")
control_dates_df$V4 <- as.Date(as.numeric(control_dates_df$V4), origin = "1970-01-01")
control_dates_df$V5 <- as.Date(as.numeric(control_dates_df$V5), origin = "1970-01-01")
colnames(control_dates_df) <- c("floodzip_id", "control1_start", "control1_end", "control2_start", "control2_end")
control_dates_df <- control_dates_df[order(control_dates_df$floodzip_id),]
rownames(control_dates_df) <- c(1:nrow(control_dates_df))

zipcode_flood_control_array_week <- data.frame()

#this is slow 
for (i in 1:nrow(control_dates_df)){
  dates1 <- seq(as.Date(control_dates_df$control1_start[i]), as.Date(control_dates_df$control1_end[i]), by = "days")
  dates2 <- seq(as.Date(control_dates_df$control2_start[i]), as.Date(control_dates_df$control2_end[i]), by = "days")
  control_dates_expanded <- c(dates1, dates2)
  floodzip_id <- rep(control_dates_df$floodzip_id[i], length(control_dates_expanded))
  zipcode_flood_control_array_week <- rbind(zipcode_flood_control_array_week, cbind(floodzip_id, control_dates_expanded))
}

zipcode_flood_control_array_week$control_dates_expanded <- as.Date(as.numeric(zipcode_flood_control_array_week$control_dates_expanded), origin = "1970-01-01")

zipcode_flood_control_array_week$event_exposed <- 0
zipcode_flood_control_array_week$event_lagwk1 <- 0
zipcode_flood_control_array_week$event_lagwk2 <- 0
zipcode_flood_control_array_week$event_lagwk3 <- 0
zipcode_flood_control_array_week$event_lagwk4 <- 0

zipcode_flood_control_array_week$zipcode <- as.numeric(substr(zipcode_flood_control_array_week$floodzip_id, 15, 20))
zipcode_flood_control_array_week$year <- lubridate::year(zipcode_flood_control_array_week$control_dates_expanded)
zipcode_flood_control_array_week$month <- lubridate::month(zipcode_flood_control_array_week$control_dates_expanded)
zipcode_flood_control_array_week$day <- lubridate::day(zipcode_flood_control_array_week$control_dates_expanded)

#simplify into one step 
zipcode_flood_control_array_week <- zipcode_flood_control_array_week[,c(1,11,2,8:10,3:7)]

zipcode_flood_control_array_week <- rename(zipcode_flood_control_array_week, date = control_dates_expanded)

zipcode_flood_semifinal_array_week <- rbind(zipcode_flood_control_array_week, zipcode_flood_exp_lag_array_week)
  
#control == 1 --> control, control == 0 --> flood (exposure/lag)
zipcode_flood_semifinal_array_week$control <- ifelse(zipcode_flood_semifinal_array_week$event_exposed == 1 |
                                          zipcode_flood_semifinal_array_week$event_lagwk1 == 1 |
                                          zipcode_flood_semifinal_array_week$event_lagwk2 == 1 |
                                          zipcode_flood_semifinal_array_week$event_lagwk3 == 1 |
                                          zipcode_flood_semifinal_array_week$event_lagwk4 == 1, 0, 1)

zipcode_flood_semifinal_array_week <- zipcode_flood_semifinal_array_week[order(zipcode_flood_semifinal_array_week$floodzip_id, zipcode_flood_semifinal_array_week$date),]
rownames(zipcode_flood_semifinal_array_week) <- c(1:nrow(zipcode_flood_semifinal_array_week))

zipcode_flood_semifinal_array_week <- zipcode_flood_semifinal_array_week[,c(1,4,3,5,6,2,7:11)]

#merge with medicare data here by zipcode, year, month, day 

# need to modify aggregation to be done for each flood-zipcode combo by exposure + lag, control 1, control 2
# zipcode_flood_array_week_aggregated_outcome <- zipcode_flood_semifinal_array_week %>% group_by(floodzip_id) %>% 
#                                                 summarise(exposed = sum(outcome * event_exposed),
#                                                           lag_wk1 = sum(outcome * event_lagwk1),
#                                                           lag_wk2 = sum(outcome * event_lagwk2),
#                                                           lag_wk3 = sum(outcome * event_lagwk3),
#                                                           lag_wk4 = sum(outcome * event_lagwk4))

# #merge the aggregated outcomes by flood-zip id
# zipcode_flood_final_array_week <- merge(zipcode_flood_semifinal_array_week, zipcode_flood_array_week_aggregated_outcome, 
#                                        by = "floodzip_id")

#zipcode_flood_final_array_week$floodzip_id <- NULL
#zipcode_flood_final_array_week$date <- NULL

# save as rds for analysis
#saveRDS(zipcode_flood_final_array_week, paste0(dir.output,'zipcode_flood_lag_array_week_',start_year,'_',end_year,'.rds'))
