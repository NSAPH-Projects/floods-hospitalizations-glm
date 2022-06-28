library(raster)
library(terra)
library(tmap)
library(tmaptools)
library(sf)
library(USAboundaries)
#install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")
library(USAboundariesData)
library(tidyverse)

#__________________________________________________________________________________________________________

#Separate script for 'rgee' setup 
gfd <- ee$ImageCollection('GLOBAL_FLOOD_DB/MODIS_EVENTS/V1')

#On GFD, they count US floods as long as portion of US was involved --> 98 flood maps 
USAfilter <- ee$Filter$stringContains("cc", 'USA')

gfd_USA <- gfd$select("flooded","duration","jrc_perm_water")$
  filter(USAfilter)$
  filterDate("2000-01-01","2014-12-31")

#gfd_USA$getInfo()

#According to GFD, we should have 83 (or 90 if we go through 2016) flood maps
#Use ee_print(gfd_USA) to print MetaData about the ImageCollection, first Image, and first Band

#Need 'rgee' and 'googledrive' setup complete
#Save DFO ID's for later use 
USA_DFO <- unlist(gfd_USA$aggregate_array('id')$getInfo())

#__________________________________________________________________________________________________________

#Function Goal: Creates a table that includes properties of each flood 
#collection is a GEE image collection 
properties_table <- function(collection){
  id <- unlist(collection$aggregate_array('id')$getInfo())
  countries <- unlist(collection$aggregate_array('countries')$getInfo())
  start <- as.Date(unlist(collection$aggregate_array('began')$getInfo()))
  end <- as.Date(unlist(collection$aggregate_array('ended')$getInfo()))
  days_flooded <- difftime(end, start, units = "days")
  main_cause <- unlist(collection$aggregate_array('dfo_main_cause')$getInfo())
  severity <- unlist(collection$aggregate_array('dfo_severity')$getInfo())
  displaced <- unlist(collection$aggregate_array('dfo_displaced')$getInfo())
  dead <- unlist(collection$aggregate_array('dfo_dead')$getInfo()) 
  table <- data.frame(id, start, end, countries, days_flooded, main_cause, severity, displaced, dead)
  return(table)
}

USA_table <- properties_table(gfd_USA)

#Standardize main cause variable for this specific collection using gfd as reference 
#This is bad practice because it relies on an external resource 
for (x in c(2491, 3815, 3689, 3625)){
  USA_table$main_cause[USA_table$id == x] <- "Dam"
}
for (x in c(2719, 3861, 2566, 3977, 2356, 3671, 2753, 2735, 3336, 
            3366, 2959, 2063, 3567, 3370)){
  USA_table$main_cause[USA_table$id == x] <- "Tropical Storm, Surge"
}
for (x in c(3799, 3285, 2007, 2841, 2829, 2462, 3300, 3092, 3268, 2182, 4051)){
  USA_table$main_cause[USA_table$id == x] <- "Snowmelt, Ice, Rain"
}
y <- setdiff(USA_table$id, c(2491, 3815, 3689, 3625,2719, 3861, 
                             2566, 3977, 2356, 3671, 2753, 2735, 3336, 
                             3366, 2959, 2063, 3567, 3370,3799, 3285, 2007, 2841, 
                             2829, 2462, 3300, 3092, 3268, 2182, 4051))
for (x in y){
  USA_table$main_cause[USA_table$id == x] <- "Heavy rain"
}

#Summary statistics of certain properties (in table format)
#Heavily right skewed so used five-number summary 
fivenum(as.numeric(USA_table$days_flooded)) #Median number of days of flood 
sapply(USA_table[c('displaced','dead')], fivenum) #Min/Median/Max of people displaced and deaths 
table(USA_table$severity) #How many of each type of severity according to DFO 
table(USA_table$countries) #Less than half are restricted to the US 
table(USA_table$main_cause) #How many of each type of cause 

#__________________________________________________________________________________________________________

#Function Goal: Convert each GEE image in GEE image collection to a raster 
#collection is a GEE image collection 
imagecollection_to_raster <- function(collection){
  #Vector of DFO Flood ID's 
  id <- unlist(collection$aggregate_array('id')$getInfo())
  for (i in 1:length(id)){
    ID <- id[i]
    file_name <- as.character(ID)
    #Convert image collection to individual images 
    img <- ee$Image(gfd_USA$filterMetadata('id', 'equals', ID)$first())
    #Convert all bands to the same type ('flooded' + 'jrc_perm_water' are UInt8 or Byte; duration is UInt16)
    img_converted <- img$toByte()
    #Convert each image to raster for easier use 
    img_raster <- ee_as_raster(image = img_converted, via = "drive", dsn = file_name)
  }
}
#imagecollection_to_raster(gfd_USA)

#Note: Exporting the image collection to Google Drive takes multiple hours (10+), but they are located in 'rgee_backup' folder on Google Drive unless different container specified 
#Note: The .tif files also download locally to where this .RMD file is located  

#__________________________________________________________________________________________________________

#Function Goal: Using a list, import all rasters into R
#path is a vector of size one and type character string 
import_rasters <- function(path, id){
  setwd(path)
  flood_list <- list.files(path = path, pattern = '.tif$', all.files = TRUE, full.names = TRUE)
  flood_rasters <- lapply(flood_list, stack)
  flood_rasters <- setNames(flood_rasters, id)
  return(flood_rasters)
}

path <- '/Users/sarikaaggarwal/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA'
#import_rasters(path, USA_DFO)

#__________________________________________________________________________________________________________

# Function Goal: This function masks permanent water to isolate the floodwater
# This is done for both 'flooded' (binary) and 'duration' (days flooded, integeer values) 
# The output is a list of 3 lists (floodwater, duration, and county polygons for each flood) 

#rasters is a list of RasterStack objects, id is a vector of flood ID's (to name each object in the list)
flood_masking <- function(id, rasters){
  iso_floodwater <- list()
  iso_floodwater_dur <- list()
  counties_list <- list()
  county_polygons <- us_counties() 
  for (i in 1:length(id)){
    ID <- as.character(id[i])
    floodwater <- mask((rasters[[ID]])[[1]], (rasters[[ID]])[[3]])
    floodwater_dur <- mask((rasters[[ID]])[[2]], (rasters[[ID]])[[3]])
    floodwater_counties <- st_crop(county_polygons, extent(floodwater))
    counties_list[[ID]] <- floodwater_counties 
    iso_floodwater[[ID]] <- floodwater
    iso_floodwater_dur[[ID]] <- floodwater_dur
  }
  floodwater_info <- list('iso_floodwater' = iso_floodwater,
                          'iso_floodwater_dur' = iso_floodwater_dur,
                          'counties' = counties_list)
  
  return(floodwater_info)
}

floods <- import_rasters(path, USA_DFO) %>% flood_masking(USA_DFO,.)

#__________________________________________________________________________________________________________

#Function Goal: Plot the isolated floodwater (plots of flood duration are not currently included) 
#floodwater is a list of RasterLayer objects, id is a vector of flood ID's (to name each object in the list)
plot_floods <- function(id,floodwater_info, iso_floodwater, counties){
  all_plots <- list()
  for (i in 1:length(id)){
    ID <- as.character(id[i])
    floodwater <- iso_floodwater[[ID]]
    floodwater_counties <- counties[[ID]]
    #floodwater (red) + county boundaries
    plot <- tm_shape(floodwater) +
      tm_raster(style= "cat", palette = c("white","red3"), title = "") +
      tm_shape(floodwater_counties) +
      tm_borders(col="black", lwd = 0.25) + 
      tm_layout(legend.outside=TRUE)
    all_plots[[ID]] <- plot 
  }
  return(all_plots)
}

# import_rasters(path, USA_DFO) %>% 
#   flood_masking(USA_DFO,.) %>% 
#   plot_floods(USA_DFO,.$iso_floodwater, .$counties)

#__________________________________________________________________________________________________________

# Function Goal: Computes the percent flooded and mean/median/max duration among flooded pixels for each county involved in a flood 
# Output: Data frame with aggregate measures, 5-number county FIPS, county name, state abbrev

#Aggregate measures
aggregate_measures <- function(id, floodwater_info, iso_floodwater, iso_floodwater_dur, counties){
  aggregate_info <- list()
  statistics <- function(x, na.rm) {c('mean' = mean(x[x >= 1], na.rm = na.rm), 
                                      'median' = median(x[x >= 1], na.rm = na.rm), 
                                      'max' = max(x[x >= 1], na.rm = na.rm))}
  for (i in 1:length(id)){
    ID <- as.character(id[i])
    floodwater <- iso_floodwater[[ID]]
    floodwater_dur <- iso_floodwater_dur[[ID]]
    floodwater_counties <- counties[[ID]]
    mean_flooded <- terra::extract(floodwater, floodwater_counties, fun = mean, na.rm = TRUE, df = TRUE)
    mean_flooded[,2] <- mean_flooded[,2] * 100
    #Duration measures are in days 
    duration_measures <- terra::extract(floodwater_dur, floodwater_counties, fun = statistics, df = TRUE)
    colnames(duration_measures) <- c("ID", "Mean", "Median", "Max")
    combined_measures <- left_join(mean_flooded, duration_measures, by = 'ID')
    combined_measures <- data.frame(combined_measures[,2:5], floodwater_counties[c("countyfp","statefp","namelsad","stusps")])
    combined_measures$county_fips <- paste(combined_measures$statefp, combined_measures$countyfp, sep = "")
    combined_measures <- combined_measures[,c(1:4,10,7:8)]
    colnames(combined_measures) <- c("pct_flooded", "avg_duration", "median_duration", "total_duration","county_fips","county_name","state_abbrev")
    combined_measures <- combined_measures %>% drop_na()
    aggregate_info[[ID]] <- combined_measures   
  }
  return(aggregate_info)
}

# import_rasters(path, USA_DFO) %>% 
#   flood_masking(., USA_DFO) %>% 
#   aggregate_measures(.$iso_floodwater, .$iso_floodwater_dur, .$counties, USA_DFO)

county_flood_measures <- floods %>% aggregate_measures(USA_DFO, ., .$iso_floodwater, .$iso_floodwater_dur, .$counties)
saveRDS(county_flood_measures, file = "county_flood_aggmeasures.rds")

county_flood_measures <- readRDS("county_flood_aggmeasures.rds")

#__________________________________________________________________________________________________________

#Task: Add the flood ID, start date, main cause, and severity to aggregate measures ("master data set")

library(plyr)
flood_data <- ldply(county_flood_measures, data.frame)
names(flood_data)[names(flood_data) == ".id"] <- "id"

start_vec <- c()
cause_vec <-c()
severity_vec <- c()
for (i in 1:length(county_flood_measures)){
  rows <- nrow(county_flood_measures[[i]])
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

saveRDS(flood_data, file = 'county_flood_master_2000_2014.rds')


#__________________________________________________________________________________________________________

#Task: Add the states involved in each flood to properties table

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


#Time Series Datasets of Exposure (modeled after 'hurricaneexposure')
#For every flood, county combination, we want a row that gives key information
#Both functions rely on the use of a simple properties table that was developed using 'rgee' 
#Years: 2000-2014 


#This function takes in start and end years (numeric) and returns a dataframe with the DFO flood ID, start/end date, cause, severity and states impacted
events_floods <- function(properties_table, start_year, end_year){
  #colnames(properties_table) <- c("id", "start", "end", "countries", "days_flooded", "main_cause", "severity", "displaced", "dead")
  properties_table$event_years <- as.numeric(substring(properties_table$start,1,4))
  events <- properties_table[properties_table$event_years >= start_year & properties_table$event_years <= end_year,]
  events <- events[c(1:3,6:7,10)]
  return(events)
}



#This function takes in 5-number county FIPS (vector), start and end years (numeric) and returns a dataframe with the county FIPS, DFO flood ID, start/end date, and aggregate measures 
county_floods <- function(properties_table, aggregate_info, counties, start_year, end_year){
  county_events <- data.frame()
  properties_table$event_years <- as.numeric(substring(properties_table$start,1,4))
  event_id <- as.character(properties_table[properties_table$event_years >= start_year & properties_table$event_years <= end_year,]$id)
  event_info <- properties_table[properties_table$event_years >= start_year & properties_table$event_years <= end_year,]
  event_info <- event_info[,c(1:3)] 
  event_info$id <- as.character(event_info$id)
                  
  events <- subset(aggregate_info, names(aggregate_info) == event_id) 
  events <- lapply(events, function(x) subset(x, x$county_fips %in% counties))
  events <- lapply(events, function(x) x[c("county_fips","pct_flooded", "avg_duration", "median_duration", "total_duration")])
  
  df_events2 <- data.frame()
  for (i in 1:length(events)){
    df_events <- as.data.frame(events[[i]])
    df_events$id <- as.character(rep(event_id[i], times = nrow(df_events)))
    df_events2 <- rbind(df_events2, df_events)
  }
  county_events <- left_join(df_events2, event_info, by = "id")
  county_events <- county_events[c(1,6:8,2:5)]
  return(county_events)
}




