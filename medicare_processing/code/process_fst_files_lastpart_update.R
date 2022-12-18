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

# 3. MERGE CASES AND DENOM FILES

# dat.denom$zip = as.numeric(dat.denom$zip)
# dat.denom = dat.denom[,c('year','zip','population')]
# 
# dat.denom.exposure <- left_join(zipcode_flood_semifinal_array_week,dat.denom, by = c("year" = "year", "zipcode" = "zip"))
# dat.denom.exposure$population <- ifelse(is.na(dat.denom.exposure$population)==TRUE,0,dat.denom.exposure$population)
# print("lag array merged with medicare denominator")
# 
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
# dat.denom.exposure.update <- matrix(data = NA, nrow = 5*nrow(zipcode_flood_week_aggregated_population), ncol = 1)
# j <- 1
# for (i in 1:nrow(zipcode_flood_week_aggregated_population)){
#   if (zipcode_flood_week_aggregated_population$control_indicator[i] == 0){
#     pt <- zipcode_flood_week_aggregated_population[i,c(3:7)]
#   }
#   else if (zipcode_flood_week_aggregated_population$control_indicator[i] == 1){
#     pt <- zipcode_flood_week_aggregated_population[i,c(8:12)]
#   }
#   else if (zipcode_flood_week_aggregated_population$control_indicator[i] == 2){
#     pt <- zipcode_flood_week_aggregated_population[i,c(8:12)]
#   }
#   else {
#     break 
#   }
#   pt <- t(pt)
#   dat.denom.exposure.update[j:(j + 4),] <- pt
#   j <- j + 5 
# }
# 
# dat.denom.exposure.update <- as.data.frame(dat.denom.exposure.update)
# dat.denom.exposure.update$floodzip_id <- rep(zipcode_flood_week_aggregated_population$floodzip_id, each = 5)
# dat.denom.exposure.update$control_indicator <- rep(zipcode_flood_week_aggregated_population$control_indicator, each = 5)
# colnames(dat.denom.exposure.update) <- c("pt", "floodzip_id", "control_indicator")
# dat.denom.exposure.update <- dat.denom.exposure.update[,c(2,3,1)]
# 
# print("computed person-time for each time period and flood-zipcode combo")
# head(dat.denom.exposure.update)

# saveRDS(dat.denom.exposure.update, "/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/dat.denom.exposure.update.rds")

dat.denom.exposure.update <- readRDS("/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/dat.denom.exposure.update.rds")

# isolate a particular cause of hospitalisations
ccs_category_descs = sort(unique(dat.admissions$category_code))
# ccs names
code.lookup = readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/CCS_DX.rds')
ccs.categories <- subset(code.lookup, select = -c(1,4,5))
ccs.categories <- unique.data.frame(ccs.categories)
#full_diag_name is the same as category_desc 
names(ccs.categories) = c('category_code','full_diag_name')
ccs.categories$full_diag_name = as.character(ccs.categories$full_diag_name)

#missing zipcodes
zipcode_denom_missing <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/zipcodes_in_exposure_not_in_denom.rds')


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
  dat.single.exposure <- left_join(zipcode_flood_semifinal_array_week,dat.single, by = c("zipcode", "year", "month", "day"))
  dat.single.exposure$cases <- ifelse(is.na(dat.single.exposure$cases)==TRUE,0,dat.single.exposure$cases)
  
  print("lag array merged with medicare admissions")
  
  zipcode_flood_week_aggregated_cases <- dat.single.exposure %>% group_by(floodzip_id, control_indicator) %>%
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
  
  dat.single.exposure.update <- matrix(data = NA, nrow = 5*nrow(zipcode_flood_week_aggregated_cases), ncol = 1)
  k <- 1
  for (i in 1:nrow(zipcode_flood_week_aggregated_cases)){
    if (zipcode_flood_week_aggregated_cases$control_indicator[i] == 0){
      cases_agg <- zipcode_flood_week_aggregated_cases[i,c(3:7)]
    }
    else if (zipcode_flood_week_aggregated_cases$control_indicator[i] == 1){
      cases_agg <- zipcode_flood_week_aggregated_cases[i,c(8:12)]
    }
    else if (zipcode_flood_week_aggregated_cases$control_indicator[i] == 2){
      cases_agg <- zipcode_flood_week_aggregated_cases[i,c(8:12)]
    }
    else {
      break 
    }
    cases_agg <- t(cases_agg)
    dat.single.exposure.update[k:(k + 4),] <- cases_agg
    k <- k + 5 
  }
  
  dat.single.exposure.update <- as.data.frame(dat.single.exposure.update)
  dat.single.exposure.update$floodzip_id <- rep(zipcode_flood_week_aggregated_cases$floodzip_id, each = 5)
  dat.single.exposure.update$control_indicator <- rep(zipcode_flood_week_aggregated_cases$control_indicator, each = 5)
  colnames(dat.single.exposure.update) <- c("cases", "floodzip_id", "control_indicator")
  dat.single.exposure.update <- dat.single.exposure.update[,c(2,3,1)]
  
  print("computed cases for each time period and flood-zipcode combo")
  head(dat.single.exposure.update)

  dat.merged <- dat.single.exposure.update
  dat.merged$pt <- dat.denom.exposure.update$pt 
  
  print("df with floodzip_id, control_indicator, cases_agg, pt")
  
  dat.merged$zipcode <- str_sub(dat.merged$floodzip_id, - 5, - 1)
  
  year_df <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/years_of_floodzips_by_time_period.rds')
  dat.merged$year <- year_df$year
  
  print(dat.merged[1:100,])

  indicator_array_ind <- rbind(diag(5), matrix(data = 0, nrow = 5, ncol = 5,), matrix(data = 0, nrow = 5, ncol = 5)) 
  indicator_array <- do.call(rbind, replicate(nrow(dat.merged)/15, indicator_array_ind, simplify = FALSE))
  indicator_array <- as.data.frame(indicator_array)
  colnames(indicator_array) <- c("exposed", "lag_wk1", "lag_wk2", "lag_wk3", "lag_wk4")
  dat.complete <- cbind(dat.merged, indicator_array)
  #computed rate function doesn't work but is not necessary, will ignore for now
  # dat.complete$rate <- ifelse(dat.complete$pt == 0, NA, dat.complete$cases_agg/dat.complete$pt)
  
  #print("computed rate")
  
  dat.complete <- dat.complete[,c(1,5,6,2,7,8,9,10,11,3,4)]
  nrow(dat.complete)
  
  dat.complete.noNA <- dat.complete[!(dat.complete$zipcode %in% c(zipcode_denom_missing$zip,"35898", "36112","72314","87750",
  "57542", "57647", "57778" ,"62845" ,"58320" ,"77507", "39529", "38912" ,"72516" ,"87009", "87117", "93262" ,"95836", "29808", 
  "32212" ,"32815", "23459", "23461" ,"23521", "23709", "27859" ,"28310", "92135" ,"92155", "92862" ,"93042", "26674" ,"59354",
  "48710" ,"55455" ,"29632" ,"58319" ,"72105" ,"80913" ,"60037", "00005" ,"56740", "76949", "31314" ,"36113" ,"04549", "06433", "19112" ,
  "58705", "36615", "15275" ,"92152", "36515", "41351" ,"72199" ,"72329")),]
  
  nrow(dat.complete.noNA)
  print(dat.complete.noNA[1:100,])

  saveRDS(dat.complete.noNA, paste0(dir.output,'medicare_',gsub(" ", "_", full_diag_name),'_rates_',years[1],'_',years[length(years)],'.rds'))
  