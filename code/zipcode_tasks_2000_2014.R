library(raster)
#library(terra)
library(sf)
library(tidyverse)
#library(lwgeom)

#__________________________________________________________________________________________________________

# library(googledrive)
# drive_auth(email = 'saggarwal@g.harvard.edu', cache = T)
# 
# library(rgee)
# ee_check()
# ee_Initialize('saggarwal', drive = TRUE)
# 
# gfd <- ee$ImageCollection('GLOBAL_FLOOD_DB/MODIS_EVENTS/V1')
# 
# #On GFD, they count US floods as long as portion of US was involved --> 98 flood maps 
# USAfilter <- ee$Filter$stringContains("cc", 'USA')
# 
# gfd_USA <- gfd$select("flooded","duration","jrc_perm_water")$
#   filter(USAfilter)$
#   filterDate("2000-01-01","2014-12-31")
# 
# #gfd_USA$getInfo()
# 
# #According to GFD, we should have 83 (or 90 if we go through 2016) flood maps
# #Use ee_print(gfd_USA) to print MetaData about the ImageCollection, first Image, and first Band
# 
# #Need 'rgee' and 'googledrive' setup complete
# #Save DFO ID's for later use 
# USA_DFO <- unlist(gfd_USA$aggregate_array('id')$getInfo())
# saveRDS(USA_DFO, "USA_DFO_2000_2014.rds")
# 
# #__________________________________________________________________________________________________________
# 
# #Function Goal: Creates a table that includes properties of each flood 
# #collection is a GEE image collection 
# properties_table <- function(collection){
#   id <- unlist(collection$aggregate_array('id')$getInfo())
#   countries <- unlist(collection$aggregate_array('countries')$getInfo())
#   start <- as.Date(unlist(collection$aggregate_array('began')$getInfo()))
#   end <- as.Date(unlist(collection$aggregate_array('ended')$getInfo()))
#   days_flooded <- difftime(end, start, units = "days")
#   main_cause <- unlist(collection$aggregate_array('dfo_main_cause')$getInfo())
#   severity <- unlist(collection$aggregate_array('dfo_severity')$getInfo())
#   displaced <- unlist(collection$aggregate_array('dfo_displaced')$getInfo())
#   dead <- unlist(collection$aggregate_array('dfo_dead')$getInfo()) 
#   table <- data.frame(id, start, end, countries, days_flooded, main_cause, severity, displaced, dead)
#   return(table)
# }
# 
# USA_table <- properties_table(gfd_USA)
# 
# #Standardize main cause variable for this specific collection using gfd as reference 
# #This is bad practice because it relies on an external resource 
# for (x in c(2491, 3815, 3689, 3625)){
#   USA_table$main_cause[USA_table$id == x] <- "Dam"
# }
# for (x in c(2719, 3861, 2566, 3977, 2356, 3671, 2753, 2735, 3336, 
#             3366, 2959, 2063, 3567, 3370)){
#   USA_table$main_cause[USA_table$id == x] <- "Tropical Storm, Surge"
# }
# for (x in c(3799, 3285, 2007, 2841, 2829, 2462, 3300, 3092, 3268, 2182, 4051)){
#   USA_table$main_cause[USA_table$id == x] <- "Snowmelt, Ice, Rain"
# }
# y <- setdiff(USA_table$id, c(2491, 3815, 3689, 3625,2719, 3861, 
#                              2566, 3977, 2356, 3671, 2753, 2735, 3336, 
#                              3366, 2959, 2063, 3567, 3370,3799, 3285, 2007, 2841, 
#                              2829, 2462, 3300, 3092, 3268, 2182, 4051))
# for (x in y){
#   USA_table$main_cause[USA_table$id == x] <- "Heavy rain"
# }
# 
# saveRDS(USA_table, "USA_table_2000_2014.rds")

#__________________________________________________________________________________________________________

#For ease of use on the cluster 
USA_DFO <- readRDS("USA_DFO_2000_2014.rds")
USA_table <- readRDS("USA_table_2000_2014.rds")

#Function Goal: Using a list, import all rasters into R
#path is a vector of size one and type character string 
import_rasters <- function(path, id){
  setwd(path)
  flood_list <- list.files(path = path, pattern = '.tif$', all.files = TRUE, full.names = TRUE)
  flood_rasters <- lapply(flood_list, stack)
  flood_rasters <- setNames(flood_rasters, id)
  return(flood_rasters)
}

#Changed on cluster to '~/flood'
path <- '/Users/sarikaaggarwal/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA'



flood_rasters <- import_rasters(path, USA_DFO)


#__________________________________________________________________________________________________________


#Function Goal: Using a list, import all zipcode shape files into R
#path is a vector of size one and type character string 
import_zipcodeSHP <- function(path, years){
  zipcode_shp_files <- dir(path, full.names = T, recursive = T)
  
  zipcode_shp_files <- zipcode_shp_files[grepl('shp$', zipcode_shp_files)]
  zipcode_shp_files <- zipcode_shp_files[grepl('polygon', zipcode_shp_files)]
  
  zipcode_polygons <- lapply(zipcode_shp_files, st_read)
  zipcode_polygons <- setNames(zipcode_polygons, years)
  
  return(zipcode_polygons)
}

years <- c("2000", "2004", "2005", "2006", "2007", "2009", "2010", "2012", "2013", "2014", "2015", "2016", 2017)
#Changed on cluster to '/zipcode_shape_files'
zip_path <- '/Users/sarikaaggarwal/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/zipcode_shape_files'

zipcode_polygons <- import_zipcodeSHP(zip_path, years)


#__________________________________________________________________________________________________________

# Function Goal: This function masks permanent water to isolate the floodwater
# This is done for both 'flooded' (binary) and 'duration' (days flooded, integer values) 
# The output is a list of 2 lists (floodwater, duration for each flood) 

#rasters is a list of RasterStack objects, id is a vector of flood ID's (to name each object in the list)
flood_masking <- function(id, rasters){
  iso_floodwater <- list()
  iso_floodwater_dur <- list()
  for (i in 1:length(id)){
    ID <- as.character(id[i])
    floodwater <- mask((rasters[[ID]])[[1]], (rasters[[ID]])[[3]])
    floodwater_dur <- mask((rasters[[ID]])[[2]], (rasters[[ID]])[[3]])
    iso_floodwater[[ID]] <- floodwater
    iso_floodwater_dur[[ID]] <- floodwater_dur
  }
  floodwater_info <- list('iso_floodwater' = iso_floodwater,
                          'iso_floodwater_dur' = iso_floodwater_dur)
  
  return(floodwater_info)
}

floods <- flood_masking(USA_DFO, flood_rasters)

#__________________________________________________________________________________________________________

# Function Goal: Computes the percent flooded and mean/median/max duration among flooded pixels for each zipcode involved in a flood 
# Output: Data frame with aggregate measures, zipcode, state abbrev

#Aggregate measures
aggregate_measures <- function(id, properties_table, floodwater_info, iso_floodwater, iso_floodwater_dur, polygons){
  aggregate_info <- list()
  statistics <- function(x, na.rm) {c('mean' = mean(x[x >= 1], na.rm = na.rm), 
                                      'median' = median(x[x >= 1], na.rm = na.rm), 
                                      'max' = max(x[x >= 1], na.rm = na.rm))}
  zipcodes_years <- as.numeric(substring(properties_table$start,1,4))
  zipcodes_years <- case_when(
    zipcodes_years %in% c("2001","2002","2003") ~ "2000",
    zipcodes_years %in% c("2007","2008") ~ "2007",
    zipcodes_years %in% c("2010","2011") ~ "2010",
    TRUE ~ as.character(zipcodes_years)
  )
  df <- data.frame(cbind(properties_table$id,zipcodes_years))
  colnames(df) <- c("id", "zipcodes_years")
  for (i in 1:length(id)){
    ID <- as.character(id[i])
    floodwater <- iso_floodwater[[ID]]
    floodwater_dur <- iso_floodwater_dur[[ID]]
    floodwater_zipcodes <- polygons[[df$zipcodes_years[df$id == ID]]]
    mean_flooded <- raster::extract(floodwater, floodwater_zipcodes, fun = mean, na.rm = TRUE, df = TRUE)
    mean_flooded[,2] <- mean_flooded[,2] * 100
    #Duration measures are in days 
    duration_measures <- raster::extract(floodwater_dur, floodwater_zipcodes, fun = statistics, df = TRUE)
    colnames(duration_measures) <- c("ID", "Mean", "Median", "Max")
    combined_measures <- left_join(mean_flooded, duration_measures, by = 'ID')
    combined_measures <- data.frame(combined_measures[,2:5], floodwater_zipcodes[c("ZIP","STATE")])
    combined_measures <- combined_measures[,c(5:6,1:4)]
    colnames(combined_measures) <- c("zipcode","state_abbrev", "pct_flooded", "avg_duration", "median_duration", "total_duration")
    combined_measures <- combined_measures %>% drop_na()
    aggregate_info[[ID]] <- combined_measures   
  }
  return(aggregate_info)
}


zipcode_flood_measures <- floods %>% aggregate_measures(USA_DFO, USA_table,., .$iso_floodwater, .$iso_floodwater_dur, zipcode_polygons)

saveRDS(zipcode_flood_measures, file = "zipcode_flood_aggmeasures_2000_2014.rds")
zipcode_flood_measures <- readRDS("zipcode_flood_aggmeasures_2000_2014.rds")


#__________________________________________________________________________________________________________

#Task: Add the flood ID, start date, main cause, and severity to aggregate measures ("master data set")

library(plyr)
flood_data <- ldply(zipcode_flood_measures, data.frame)
names(flood_data)[names(flood_data) == ".id"] <- "id"

start_vec <- c()
cause_vec <-c()
severity_vec <- c()
for (i in 1:length(zipcode_flood_measures)){
  rows <- nrow(zipcode_flood_measures[[i]])
  start_date <- rep(USA_table$start[i], rows)
  cause <- rep(USA_table$main_cause[i], rows)
  severity <- rep(USA_table$severity[i], rows)
  start_vec <- append(start_vec, start_date)
  cause_vec <- append(cause_vec, cause)
  severity_vec <- append(severity_vec, severity)
}

flood_data$start <- start_vec
flood_data$main_cause <- cause_vec
flood_data$severity <- severity_vec

#Add year, month, day columns to dataset 

library(lubridate)

flood_data$year <- year(flood_data$start)
flood_data$month <- month(flood_data$start)
flood_data$day <- day(flood_data$start)

saveRDS(flood_data, file = 'zipcode_flood_master_2000_2014.rds')

#__________________________________________________________________________________________________________

#Function Goal: Add the states involved in each flood to properties table
 
 states_affected <- function(id, properties_table){
   states_list <- list()
   for (i in 1:length(id)){
     ID <- as.character(id[i])
     states_list[[ID]] <- unique(zipcode_flood_measures[[ID]]$state_abbrev)
   }
   properties_table <- tibble(properties_table, "states_affected" = states_list)
 }

USA_table <- states_affected(USA_DFO, USA_table)
saveRDS(USA_table, "USA_table.rds")

#__________________________________________________________________________________________________________


# Time Series Datasets of Exposure (modeled after 'hurricaneexposure')
# For every flood, zip code combination, we want a row that gives key information
# Both functions rely on the use of a simple properties table that was developed using 'rgee' 
# Years: 2000-2014 
# 
# 
#This function takes in start and end years (numeric) and returns a dataframe with the DFO flood ID, start/end date, cause, severity and states impacted

events_floods <- function(properties_table, start_year, end_year){
  #colnames(properties_table) <- c("id", "start", "end", "countries", "days_flooded", "main_cause", "severity", "displaced", "dead")
  properties_table$event_years <- as.numeric(substring(properties_table$start,1,4))
  events <- properties_table[properties_table$event_years >= start_year & properties_table$event_years <= end_year,]
  events <- events[c(1:3,6:7,10)]
  return(events)
}

#This function takes in zipcode (vector), start and end years (numeric) and returns a dataframe with the county FIPS, DFO flood ID, start/end date, and aggregate measures

zipcode_floods <- function(properties_table, aggregate_info, zipcodes, start_year, end_year){
  zipcode_events <- data.frame()
  properties_table$event_years <- as.numeric(substring(properties_table$start,1,4))
  event_id <- as.character(properties_table[properties_table$event_years >= start_year & properties_table$event_years <= end_year,]$id)
  event_info <- properties_table[properties_table$event_years >= start_year & properties_table$event_years <= end_year,]
  event_info <- event_info[,c(1:3)]
  event_info$id <- as.character(event_info$id)

  events <- subset(aggregate_info, names(aggregate_info) == event_id)
  events <- lapply(events, function(x) subset(x, x$zipcode %in% zipcodes))
  events <- lapply(events, function(x) x[c("zipcode","pct_flooded", "avg_duration", "median_duration", "total_duration")])

  df_events2 <- data.frame()
  for (i in 1:length(events)){
    df_events <- as.data.frame(events[[i]])
    df_events$id <- as.character(rep(event_id[i], times = nrow(df_events)))
    df_events2 <- rbind(df_events2, df_events)
  }
  zipcode_events <- left_join(df_events2, event_info, by = "id")
  zipcode_events <- zipcode_events[c(1,6:8,2:5)]
  return(zipcode_events)
}

zipcode_floods(USA_table, zipcode_flood_measures, c("77584"), start_year = 2000, end_year = 2014)
