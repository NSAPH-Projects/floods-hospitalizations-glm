# this code will take all processed years and load them
# aggregate cases + person-time 
# add person-time to admissions file 
# compute rate
# subset to a single category code --> save file 

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part5.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1])

# expand grid of stuff temporary
years = c(2000:2016)

# PREPARE ADMISSIONS FILES

# load processed admissions file
dir.input.local = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/in_progress/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.admissions)=1:nrow(dat.admissions)

#print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

# PREPARE DENOM FILES

# load processed denom file
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/in_progress_denom/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)

#do we need a mechanism for "Died within year" !!! 
# dat.denom$full_name = as.character(dat.denom$full_name)
# dat.denom$full_name[is.na(dat.denom$full_name)==TRUE] <- 'Died within year'

library(stringr)
dat.admissions$zipcode_R <- str_pad(dat.admissions$zipcode_R, 5, pad = "0")
dat.admissions$zipcode <- stringi::stri_reverse(dat.admissions$zipcode_R)

dat.admissions$zipcode_R <- NULL

#just in case
dat.admissions$zipcode = as.numeric(dat.admissions$zipcode)

library(dplyr)

zipcode_flood_semifinal_array_week <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/zipcode_flood_semifinal_array_week_2000_2016.rds')

<<<<<<< HEAD
# 3. MERGE CASES AND DENOM FILES
=======
#dat.denom.exposure = merge(zipcode_flood_semifinal_array_week,dat.denom, by.x =c('year','zipcode'), by.y = c('year', 'zip'), all.x = TRUE)
dat.denom.exposure <- left_join(zipcode_flood_semifinal_array_week,dat.denom, by = c("year" = "year", "zipcode" = "zip"))
dat.denom.exposure$population <- ifelse(is.na(dat.denom.exposure$population)==TRUE,0,dat.denom.exposure$population)
print("lag array merged with medicare denominator")
>>>>>>> fce4cbbe2a85a31bed634b1c75bc0b4e131a9540

# dat.denom$zip = as.numeric(dat.denom$zip)
# dat.denom = dat.denom[,c('year','zip','population')]
# 
# 
# # #dat.denom.exposure = merge(zipcode_flood_semifinal_array_week,dat.denom, by.x =c('year','zipcode'), by.y = c('year', 'zip'), all.x = TRUE)
#  dat.denom.exposure <- left_join(zipcode_flood_semifinal_array_week,dat.denom, by = c("year" = "year", "zipcode" = "zip"))
#  dat.denom.exposure$population <- ifelse(is.na(dat.denom.exposure$population)==TRUE,0,dat.denom.exposure$population)
#  print("lag array merged with medicare denominator")
# # 
# zipcode_flood_week_aggregated_population <- dat.denom.exposure %>% group_by(floodzip_id, control_indicator) %>%
#   summarise(exp_pt = sum(population * event_exposed),
#             exp_lag_wk1_pt = sum(population * event_lagwk1),
#             exp_lag_wk2_pt = sum(population * event_lagwk2),
#             exp_lag_wk3_pt = sum(population * event_lagwk3),
#             exp_lag_wk4_pt = sum(population * event_lagwk4),
#             control_pt = sum(population * control_exposed),
#             control_lag_wk1_pt = sum(population * control_lagwk1),
#             control_lag_wk2_pt = sum(population * control_lagwk2),
#             control_lag_wk3_pt = sum(population * control_lagwk3),
#             control_lag_wk4_pt = sum(population * control_lagwk4))
# 
# # 
# # #merge the aggregated outcomes by flood-zip id and control_indicator
# # dat.denom.exposure.aggregate <- merge(dat.denom.exposure, zipcode_flood_week_aggregated_population,
# #                                       by = c("floodzip_id", "control_indicator"))
# dat.denom.exposure.aggregate <- inner_join(dat.denom.exposure, zipcode_flood_week_aggregated_population, by = c("floodzip_id", "control_indicator"))
#  
#  print("aggregate population merged with lag array + medicare denom")
# # 
# #keep exp version for control_indicator == 0, control version for control_indicator == 1, 2
# dat.denom.exposure.update <- dat.denom.exposure.aggregate %>% mutate(pt_final = case_when(event_exposed == 1 ~ exp_pt,
#                                                                                           event_lagwk1 == 1 ~ exp_lag_wk1_pt,
#                                                                                           event_lagwk2 == 1 ~ exp_lag_wk2_pt,
#                                                                                           event_lagwk3 == 1 ~ exp_lag_wk3_pt,
#                                                                                           event_lagwk4 == 1 ~ exp_lag_wk4_pt,
#                                                                                           control_exposed == 1 ~ control_pt,
#                                                                                           control_lagwk1 == 1 ~ control_lag_wk1_pt,
#                                                                                           control_lagwk2 == 1 ~ control_lag_wk2_pt,
#                                                                                           control_lagwk3 == 1 ~ control_lag_wk3_pt,
#                                                                                           control_lagwk4 == 1 ~ control_lag_wk4_pt))
# 
# print("kept person-time respective to indicator variables")
# 
# dat.denom.exposure.update$exp_pt <- NULL
# dat.denom.exposure.update$exp_lag_wk1_pt <- NULL
# dat.denom.exposure.update$exp_lag_wk2_pt <- NULL
# dat.denom.exposure.update$exp_lag_wk3_pt <- NULL
# dat.denom.exposure.update$exp_lag_wk4_pt <- NULL
# dat.denom.exposure.update$control_pt <- NULL
# dat.denom.exposure.update$control_lag_wk1_pt <- NULL
# dat.denom.exposure.update$control_lag_wk2_pt <- NULL
# dat.denom.exposure.update$control_lag_wk3_pt <- NULL
# dat.denom.exposure.update$control_lag_wk4_pt <- NULL
# # 
# dat.denom.exposure.update <- dat.denom.exposure.update[,c('floodzip_id', 'zipcode', 'year', 'month', 'day', 'event_exposed', 'event_lagwk1', 'event_lagwk2', 'event_lagwk3', 'event_lagwk4', 'control_indicator', 'pt_final')]
# saveRDS(dat.denom.exposure.update, "/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/dat.denom.exposure.update.rds")

<<<<<<< HEAD
# head(dat.denom.exposure.update)
dat.denom.exposure.update <- readRDS("/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/dat.denom.exposure.update.rds")
=======

#merge the aggregated outcomes by flood-zip id and control_indicator
# dat.denom.exposure.aggregate <- merge(dat.denom.exposure, zipcode_flood_week_aggregated_population, 
#                                       by = c("floodzip_id", "control_indicator"))
dat.denom.exposure.aggregate <- inner_join(dat.denom.exposure, zipcode_flood_week_aggregated_population, by = c("floodzip_id", "control_indicator"))

print("aggregate population merged with lag array + medicare denom")

#keep exp version for control_indicator == 0, control version for control_indicator == 1, 2
dat.denom.exposure.update <- dat.denom.exposure.aggregate %>% mutate(pt_final = case_when(event_exposed == 1 ~ exp_pt,
                                                                                          event_lagwk1 == 1 ~ exp_lag_wk1_pt,
                                                                                          event_lagwk2 == 1 ~ exp_lag_wk2_pt,
                                                                                          event_lagwk3 == 1 ~ exp_lag_wk3_pt,
                                                                                          event_lagwk4 == 1 ~ exp_lag_wk4_pt,
                                                                                          control_exposed == 1 ~ control_pt,
                                                                                          control_lagwk1 == 1 ~ control_lag_wk1_pt,
                                                                                          control_lagwk2 == 1 ~ control_lag_wk2_pt,
                                                                                          control_lagwk3 == 1 ~ control_lag_wk3_pt,
                                                                                          control_lagwk4 == 1 ~ control_lag_wk4_pt))

print("kept person-time respective to indicator variables")

dat.denom.exposure.update$exp_pt <- NULL
dat.denom.exposure.update$exp_lag_wk1_pt <- NULL
dat.denom.exposure.update$exp_lag_wk2_pt <- NULL
dat.denom.exposure.update$exp_lag_wk3_pt <- NULL
dat.denom.exposure.update$exp_lag_wk4_pt <- NULL
dat.denom.exposure.update$control_pt <- NULL
dat.denom.exposure.update$control_lag_wk1_pt <- NULL
dat.denom.exposure.update$control_lag_wk2_pt <- NULL
dat.denom.exposure.update$control_lag_wk3_pt <- NULL
dat.denom.exposure.update$control_lag_wk4_pt <- NULL

dat.denom.exposure.update <- dat.denom.exposure.update[,c('floodzip_id', 'pt_final')]
saveRDS(dat.denom.exposure.update, "/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/dat.denom.exposure.update.rds")
>>>>>>> fce4cbbe2a85a31bed634b1c75bc0b4e131a9540

# isolate a particular cause of hospitalisations
ccs_category_descs = sort(unique(dat.admissions$category_code))
# ccs names
code.lookup = readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/CCS_DX.rds')
ccs.categories <- subset(code.lookup, select = -c(1,4,5))
ccs.categories <- unique.data.frame(ccs.categories)
#full_diag_name is the same as category_desc 
names(ccs.categories) = c('category_code','full_diag_name')
ccs.categories$full_diag_name = as.character(ccs.categories$full_diag_name)

# output directory
dir.output = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

category = ccs_category_descs[seedVal]

# CYCLE THROUGH EACH CCS
  # print full name
  full_diag_name = as.character(subset(ccs.categories,category_code==category)[2])
  full_diag_name = trimws(full_diag_name)
  full_diag_name = gsub('/',' ',full_diag_name)
  print(paste0(category,': ',full_diag_name))
  
  # filter out single ccs category
  dat.single = subset(dat.admissions,category_code==category)
  #dat.complete = merge(zipcode_flood_semifinal_array_week, dat.single, by = c("zipcode", "year", "month", "day"), all.x = TRUE)
  dat.complete <- left_join(zipcode_flood_semifinal_array_week,dat.single, by = c("zipcode", "year", "month", "day"))
  dat.complete$cases <- ifelse(is.na(dat.complete$cases)==TRUE,0,dat.complete$cases)
  
  print("lag array merged with medicare admissions")
  
  zipcode_flood_week_aggregated_cases <- dat.complete %>% group_by(floodzip_id, control_indicator) %>%
    summarise(exp_cases = sum(cases * event_exposed),
              exp_lag_wk1_cases = sum(cases * event_lagwk1),
              exp_lag_wk2_cases = sum(cases * event_lagwk2),
              exp_lag_wk3_cases = sum(cases * event_lagwk3),
              exp_lag_wk4_cases = sum(cases * event_lagwk4),
              control_cases = sum(cases * control_exposed),
              control_lag_wk1_cases = sum(cases * control_lagwk1),
              control_lag_wk2_cases = sum(cases * control_lagwk2),
              control_lag_wk3_cases = sum(cases * control_lagwk3),
              control_lag_wk4_cases = sum(cases * control_lagwk4))
  
  #merge the aggregated outcomes by flood-zip id and control_indicator
  # dat.admissions.exposure.aggregate <- merge(dat.complete, zipcode_flood_week_aggregated_cases, 
  #                                            by = c("floodzip_id", "control_indicator"))
  dat.admissions.exposure.aggregate <- inner_join(dat.complete, zipcode_flood_week_aggregated_cases, 
                                                  by = c("floodzip_id", "control_indicator"))
  
  print("aggregate cases merged with lag array + medicare admissions")
  
  #keep exp version for control_indicator == 0, control version for control_indicator == 1, 2
  dat.admissions.exposure.update <- dat.admissions.exposure.aggregate %>% mutate(cases_final = case_when(event_exposed == 1 ~ exp_cases,
                                                                                                         event_lagwk1 == 1 ~ exp_lag_wk1_cases,
                                                                                                         event_lagwk2 == 1 ~ exp_lag_wk2_cases,
                                                                                                         event_lagwk3 == 1 ~ exp_lag_wk3_cases,
                                                                                                         event_lagwk4 == 1 ~ exp_lag_wk4_cases,
                                                                                                         control_exposed == 1 ~ control_cases,
                                                                                                         control_lagwk1 == 1 ~ control_lag_wk1_cases,
                                                                                                         control_lagwk2 == 1 ~ control_lag_wk2_cases,
                                                                                                         control_lagwk3 == 1 ~ control_lag_wk3_cases,
                                                                                                         control_lagwk4 == 1 ~ control_lag_wk4_cases))
  
  print("kept cases respective to indicator variables")
  nrow(dat.admissions.exposure.update)
  
  dat.admissions.exposure.update$exp_cases <- NULL
  dat.admissions.exposure.update$exp_lag_wk1_cases <- NULL
  dat.admissions.exposure.update$exp_lag_wk2_cases <- NULL
  dat.admissions.exposure.update$exp_lag_wk3_cases <- NULL
  dat.admissions.exposure.update$exp_lag_wk4_cases <- NULL
  dat.admissions.exposure.update$control_cases <- NULL
  dat.admissions.exposure.update$control_lag_wk1_cases <- NULL
  dat.admissions.exposure.update$control_lag_wk2_cases <- NULL
  dat.admissions.exposure.update$control_lag_wk3_cases <- NULL
  dat.admissions.exposure.update$control_lag_wk4_cases <- NULL
  
  # dat.merged <- merge(dat.admissions.exposure.update[,c(floodzip_id, zipcode, year, month, day, event_exposed, event_lagwk1, event_lagwk2, event_lagwk3, event_lagwk4, control_indicator, cases_final)], 
  #                     dat.denom.exposure.update, 
  #                     by = c("floodzip_id"))
<<<<<<< HEAD
  #dat.merged <- inner_join(dat.admissions.exposure.update, dat.denom.exposure.update, by = c("floodzip_id"))
  dat.admissions.exposure.update <- dat.admissions.exposure.update[,c('floodzip_id', 'zipcode', 'year', 'month', 'day', 'event_exposed', 'event_lagwk1', 'event_lagwk2', 'event_lagwk3', 'event_lagwk4', 'control_indicator', 'cases_final')]
  
  library(data.table)
  dat.denom.exposure.update_dt <- as.data.table(dat.denom.exposure.update)
  dat.admissions.exposure.update_dt <- as.data.table(dat.admissions.exposure.update)
  dat.merged_dt <- data.table::merge.data.table(dat.admissions.exposure.update_dt, dat.denom.exposure.update_dt, by = c("floodzip_id",'zipcode', 'year', 'month', 'day', 'event_exposed', 'event_lagwk1', 'event_lagwk2', 'event_lagwk3', 'event_lagwk4', 'control_indicator'))
=======
  dat.merged <- inner_join(dat.admissions.exposure.update[,c(floodzip_id, zipcode, year, month, day, event_exposed, event_lagwk1, event_lagwk2, event_lagwk3, event_lagwk4, control_indicator, cases_final)], 
                                                dat.denom.exposure.update, 
                                                by = c("floodzip_id"))
>>>>>>> fce4cbbe2a85a31bed634b1c75bc0b4e131a9540
  
  print("admissions merged with denom")
  
  dat.merged <- as.data.frame(dat.merged_dt)
  nrow(dat.merged)
  
  dat.merged$rate <- ifelse(dat.merged$pt_final == 0, NA, dat.merged$cases_final/dat.merged$pt_final)
  
  print("computed rate")
  #returns NA because some rates are NA because pt_final = 0 
  #any(dat.merged$rate > 1) 
  saveRDS(dat.merged, paste0(dir.output,'medicare_',gsub(" ", "_", full_diag_name),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds'))
  
  dat.merged.complete <- na.omit(dat.merged)  
  saveRDS(dat.merged.complete, paste0(dir.output,'medicare_no_NA_',gsub(" ", "_", full_diag_name),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds'))
  
  