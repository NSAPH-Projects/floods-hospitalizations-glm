# this code will take the flood data
# then process it for a model input

#SA edited from RMP - 7/12/2022

#Not running on the cluster, so we don't need the lines below 

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
#county_flood_data$county_fips <- as.numeric(county_flood_data$county_fips)

# to obtain the number of unique counties (2934 counties)
all_counties = unique(county_flood_data$county_fips)

#to determine number of states included, use USA_table (should be all except Hawaii)

# add lag to the date -- takes the start date, adds lag value (0-7) and then pulls the year, month, day of the new date 
#fills in event_lag column with 1's and 0's based on lag
add_date_info = function(dat, lag_chosen){
  dat$start = as.Date(dat$start, format="%Y-%m-%d") + lag_chosen
  dat$year     = lubridate::year(dat$start)
  dat$month    = lubridate::month(dat$start)
  dat$day      = lubridate::day(dat$start)
  
  col_name     = paste0('event_lag',lag_chosen)
  dat[col_name]= 1
  
  dat = dat[
    with(dat, order(year,month,day)),
  ]
  
  dat = dat[,c('county_fips','year','month','day',col_name)]
  
  return(dat)
}

# lags to have included
lags=c(0:7)

# calculate lag values for the unconstrained lag model
#creates a data frame for each lag that is included (0-7) based on function above 
#form: county, year, month, day, eventlag0 = 1; county, year, month, day + 1, eventlag1 = 1
for(lag in lags){
  assign(paste0('county_flood_edit_',lag), add_date_info(county_flood_data, lag))
  print(head(get(paste0('county_flood_edit_',lag))))
  
}

#create a grid of all county/date combinations 
dates = seq(as.Date("2000-01-01"), as.Date("2014-12-31"), by="days")
all_counties <- as.numeric(all_counties)
complete_grid = expand.grid(dates=dates,county_fips=all_counties)
complete_grid$year = year(dates) 
complete_grid$month = month(dates)
complete_grid$day = day(dates)
complete_grid$dates = NULL
county_flood_edit_array = complete_grid

#merge the grid of county/date combinations with the dataframes that have the lag columns 
# add multiple lags to array for all counties in dataset
for(lag in lags){
  print(paste0('Matching lag ',lag))
  county_flood_edit_array = merge(county_flood_edit_array,get(paste0('county_flood_edit_',lag)),by=c('county_fips','year','month','day'), all.x=TRUE)
}

county_flood_edit_array[is.na(county_flood_edit_array)] <- 0

# save as rds for analysis
saveRDS(county_flood_edit_array, paste0(dir.output,'county_flood_lag_array_',start_year,'_',end_year,'.rds'))
