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
end_year = as.numeric("2016")

# load necessary packages
library('tidyverse') ; library('lubridate')

setwd('~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/')
dir.output<- '~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/'
#zipcode_flood_data <- readRDS('zipcode_flood_master_2000_2018.rds')
#zipcode_flood_data <- zipcode_flood_data %>% filter(year <= 2016)

# zipcode_flood_data$total_duration <- ifelse(zipcode_flood_data$total_duration > 14, 
#                                            14, zipcode_flood_data$total_duration)

# zipcode_flood_data$zipcode <- as.numeric(zipcode_flood_data$zipcode)

#remove floods caused by dams and snowmelt 
# zipcode_flood_data_no_dams_snowmelt <- zipcode_flood_data[!(zipcode_flood_data$main_cause == "Dam") 
#                                                         & !(zipcode_flood_data$main_cause == "Snowmelt, Ice, Rain"),]
#zipcode_flood_data_no_dams_snowmelt <-zipcode_flood_data_no_dams_snowmelt[,c(1:3,8:13,4:7)]
#saveRDS(zipcode_flood_data_no_dams_snowmelt, "zipcode_flood_master_no_dams_snowmelt_2000_2016.rds")
zipcode_flood_data_no_dams_snowmelt <- readRDS('zipcode_flood_master_no_dams_snowmelt_2000_2016.rds')

# to obtain the number of unique zips (19104 zips in subset, 20561 in total)
all_zips = as.numeric(unique(zipcode_flood_data_no_dams_snowmelt$zipcode))

#to determine number of states included, use USA_table (should be all except Hawaii)

list_df <- list()
names_of_df <- c()
names_col_df1 <- c("zipcode", "date", "year", "month", "day")
max_duration <- max(zipcode_flood_data_no_dams_snowmelt$total_duration)
names_col_df2 <- paste0('event_exposed',1:max_duration)
num_lagweeks <- 4
names_col_df3 <- paste0('event_lag',1:(7*num_lagweeks))

#change in code from 28 to be 7*num_lagweeks

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
zipcode_flood_edit_array_week$event_exposed <- ifelse(rowSums(zipcode_flood_edit_array_week[,c(7:48)]) >= 1, 1, 0)
    
zipcode_flood_edit_array_week$event_lagwk1 <- ifelse(rowSums(zipcode_flood_edit_array_week[,c(49:55)]) >= 1, 1, 0)
zipcode_flood_edit_array_week$event_lagwk2 <- ifelse(rowSums(zipcode_flood_edit_array_week[,c(56:62)]) >= 1, 1, 0)
zipcode_flood_edit_array_week$event_lagwk3 <- ifelse(rowSums(zipcode_flood_edit_array_week[,c(63:69)]) >= 1, 1, 0)
zipcode_flood_edit_array_week$event_lagwk4 <- ifelse(rowSums(zipcode_flood_edit_array_week[,c(70:76)]) >= 1, 1, 0)

#can remove date and flood-zipcode id columns later 
#remove exposure + lag day indicators 
zipcode_flood_exp_lag_array_week  <- zipcode_flood_edit_array_week[-c(7:76)]
zipcode_flood_exp_lag_array_week$zipcode <- as.numeric(zipcode_flood_exp_lag_array_week$zipcode)

#control == 0 --> flood (exposure/lag)
zipcode_flood_exp_lag_array_week$control_indicator <- 0

saveRDS(zipcode_flood_exp_lag_array_week, "zipcode_flood_exp_lag_array_week_2000_2016.rds")

floodzip_id_list <- unique(zipcode_flood_exp_lag_array_week$floodzip_id)

#min date is first date of exposure and max date is last day of 4th lag week 
zipcode_flood_summarized_date_array <- zipcode_flood_exp_lag_array_week %>%
                                 group_by(floodzip_id) %>% 
                                 summarise('min_date' = min(date),
                                           'max_date' = max(date))

zipcode_flood_summarized_date_array$zipcode <- as.numeric(substr(zipcode_flood_summarized_date_array$floodzip_id, 15, 20))
saveRDS(zipcode_flood_summarized_date_array, "zipcode_flood_summarized_date_array_2000_2016.rds")

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
      if (year(control2_min_date) >= end_year){
        year(control2_min_date) <- year(min_date_temp) - (i + 1)
        year(control2_max_date) <- year(max_date_temp) - (i + 1)
      }
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
  #control1 == 1 --> control, control2 == 2 --> control
  control_indicator <- c(rep(1, length(control_dates_expanded)/2), rep(2, length(control_dates_expanded)/2))
  zipcode_flood_control_array_week <- rbind(zipcode_flood_control_array_week, cbind(floodzip_id, control_dates_expanded, control_indicator))
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
zipcode_flood_control_array_week <- zipcode_flood_control_array_week[,c(1,9,2,10:12,4:8,3)]
zipcode_flood_control_array_week <- rename(zipcode_flood_control_array_week, date = control_dates_expanded)

saveRDS(zipcode_flood_control_array_week, "zipcode_flood_control_array_week_2000_2016.rds")


zipcode_flood_semifinal_array_week <- rbind(zipcode_flood_control_array_week, zipcode_flood_exp_lag_array_week)

zipcode_flood_semifinal_array_week <- zipcode_flood_semifinal_array_week[order(zipcode_flood_semifinal_array_week$floodzip_id, zipcode_flood_semifinal_array_week$date),]
rownames(zipcode_flood_semifinal_array_week) <- c(1:nrow(zipcode_flood_semifinal_array_week))
saveRDS(zipcode_flood_semifinal_array_week, "zipcode_flood_semifinal_array_week_2000_2016.rds")

#merge with medicare data here by zipcode, year, month, day 

# zipcode_flood_array_week_aggregated_outcome <- zipcode_flood_semifinal_array_week %>% group_by(floodzip_id, control_indicator) %>% 
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
