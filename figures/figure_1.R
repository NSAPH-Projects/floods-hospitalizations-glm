#FIGURE 1: plot of number of flood events in a ZIP Code 2000-2016 (does remove ZIP Codes or floods that we exclude) 
library(dplyr)
library(stringr)

library(raster)
library(sf)
library(tmap)
library(usmap)

library(RColorBrewer)

output.folder <- #output directory 
  
load('zipcode_flood_subset_2000_2016_APR.Rdata') #load data 
flood_data_test$floodzip_id <- paste0(flood_data_test$flood_id, "_flood_df_", flood_data_test$zip)

floodzips_remove <- #path to file with flood-zipcode ID's to remove
load("missing_zipcodes.Rdata") #load missing zipcodes
load("AK_zips.Rdata") #load Alaska zipcodes

flood_data_test<- flood_data_test[!(flood_data_test$floodzip_id %in% c(floodzips_remove)),] 
flood_data_test <- flood_data_test[!(flood_data_test$zip %in% c(missing_zipcodes, AK_zips)),]

number_flooded <- as.data.frame(table(flood_data_test$zip))

import_zipcodeSHP <- function(path, years){
  zipcode_shp_files <- dir(path, full.names = T, recursive = T)
  
  zipcode_shp_files <- zipcode_shp_files[grepl('shp$', zipcode_shp_files)]
  zipcode_shp_files <- zipcode_shp_files[grepl('polygon', zipcode_shp_files)]
  
  zipcode_polygons <- lapply(zipcode_shp_files, st_read)
  zipcode_polygons <- setNames(zipcode_polygons, years)
  
  return(zipcode_polygons)
}

year <- c("") #pick a year to use for the shapefile 

zip_path <- #path to zipcode shapefiles 

zipcode_polygons <- import_zipcodeSHP(zip_path, years)

zip <- zipcode_polygons[[""]] #fill in with year chosen above 
merged <- left_join(zip, numb_flooded, by = c("ZIP" = "Var1"))
merged_2010[is.na(merged)] <- 0

#check how many zipcodes are missing from the shapefile for the particular year 
numb_flooded$missingness <- ifelse(numb_flooded$Var1 %in% c(zip$ZIP), 1, 0)
sum(numb_flooded$missingness == 0) 

#exclude zipcodes in Alaska, Hawaii, and Puerto Rico
contig_us <- merged[!(merged$STATE %in% c("AK", "HI", "PR")),]

#change color palette 
colorpalette <- c('cornsilk',"lightblue", "blue","darkblue","black")
sf_use_s2(FALSE)

usa <- tm_shape(contig_us) + 
  tm_polygons(col = "Freq", pal = colorpalette, style = "cont", border.col = "black", lwd = 0.1, title = "Number of unique flood exposures by ZIP Code", legend.is.portrait = FALSE) + 
  tmap_options(check.and.fix = TRUE) + 
  tm_layout(frame = FALSE, legend.outside=TRUE, legend.outside.position = "bottom")
usa
