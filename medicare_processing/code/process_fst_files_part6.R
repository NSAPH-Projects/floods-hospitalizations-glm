# this code will take processed merged cases and denom files
# make a complete grid for each file

#edited from RMP on 6/26 - SA 

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part6.submit

# arguments from Rscript
#args <- commandArgs(trailingOnly=TRUE)

# expand grid of stuff temporary
years = c(2000:2014)

# load processed admissions file
dir.input.local = paste0('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_rates_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.admissions)=1:nrow(dat.admissions)

# load processed wind file
#setwd('/Users/sarikaaggarwal/Desktop/rmparks_coastal_storms_Jan_2020-master/data/coastal_storm_data/')
#wind <- readRDS('wind_data_1999_2014.rds')

# load processed flood file
#on local, all flood info is under GFD_USA
dir.input = paste0('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/data/flood_info/')
counties = readRDS(paste0(dir.input,'county_flood_master_',years[1],'_',years[length(years)],'.rds'))

library(plyr)

# isolate a particular cause of hospitalisations
ccs_codes = sort(unique(dat.admissions$ccs_category))

# ccs names
ccs.names = read.csv('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_diag_name')
ccs.names$full_diag_name = as.character(ccs.names$full_diag_name)

# output directory
dir.output = paste0('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# fully expand dates, fips then match to loaded admissions data
dates = seq(as.Date("2000-01-01"), as.Date("2014-12-31"), by="days")

fipscounty = sort(unique(dat.admissions$fipscounty))
complete.grid = expand.grid(date=dates,fipscounty=fipscounty)

#recoded using lubridate 
complete.grid$year = year(date)
complete.grid$month = month(date)
complete.grid$day = day(date)
complete.grid$date = NULL

# PREPARE DENOM FILES

# load processed denom file
dir.input = paste0('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/in_progress_denom/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)
dat.denom$fips = paste0('0',dat.denom$fips)
dat.denom$fips = substr(dat.denom$fips, nchar(dat.denom$fips)-5+1, nchar(dat.denom$fips))
dat.denom$state.fips = substr(dat.denom$fips, 1,2)

#attach state recognized fips codes 
fips.lookup <- read.csv('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/state_fips_lookup.csv')
#remove Hawaii (Alaska has floods)
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(15)),]
#remove third column with two letter state abbreviations 
fips.lookup = fips.lookup[,c(1:2)]
dat.denom$state.fips = as.numeric(dat.denom$state.fips)
dat.denom = merge(dat.denom,fips.lookup,by.x='state.fips',by.y='fips',all.x=TRUE)
dat.denom$full_name = as.character(dat.denom$full_name)
dat.denom$full_name[is.na(dat.denom$full_name)==TRUE] <- 'Died within year'


# CYCLE THROUGH EACH CCS
for(ccs_code in ccs_codes){

    # print full name
    full_diag_name = as.character(subset(ccs.names,css_category==ccs_code)[2])
    full_diag_name = trimws(full_diag_name)
    full_diag_name = gsub('/',' ',full_diag_name)
    print(paste0(ccs_code,': ',full_diag_name))

    # filter out single ccs category
    dat.admissions.single = subset(dat.admissions,ccs_category==ccs_code)
    dat.admissions.single$month = as.numeric(dat.admissions.single$month)
    dat.admissions.single$day = as.numeric(dat.admissions.single$day)

    # merge deaths counts with complete grid to ensure there are rows with zero deaths
    dat.complete = merge(complete.grid,dat.admissions.single,by=c('fipscounty','year','month','day'),all.x='TRUE') # WHY IS THIS LOSING CASES???

    # assign missing cases to have value 0
    dat.complete$cases = ifelse(is.na(dat.complete$cases)==TRUE,0,dat.complete$cases)

    # match admissions and storm data by fips and date
    dat.complete$month = as.numeric(dat.complete$month)
    dat.complete$year = as.numeric(dat.complete$year)
    dat.complete$day = as.numeric(dat.complete$day)

    # MERGE CASES AND DENOM FILES
    dat.denom$fips = as.numeric(dat.denom$fips)
    dat.denom = dat.denom[,c('year','fips','population')]
    dat.merged = merge(dat.complete[,c('year','fipscounty','month','day','cases')], dat.denom[,c('year','fips','population')], by.x=c('year','fipscounty'),by.y=c('year','fips'),all.x=TRUE)

    # only take years from beginning of actual intended dataset
    dat.merged = subset(dat.merged,year%in%years)

    # exclude those weird georgia counties (this is done again because of the re-merge with the denominator files) 
    gfips_to_exclude = c(13007, 13037, 13061, 13087, 13099, 13131, 13201, 13239, 13243, 13253, 13273)
    dat.merged = subset(dat.merged,!(year%in%c(1999:2001)&fipscounty%in%c(gfips_to_exclude)))
    
    #I think RMP does not remove Virginia again because it is not present in the dat.admissions file and 
    #so it won't merge with the denominator file (follow this logic for 02230 FIPS)
    
    #hypothetically would do something similar for Alaska if we knew the exact years it is/isn't present
    
    # calculate rates
    dat.merged$rates = with(dat.merged,cases/population)

    # check missing values
    dat.merged.na = dat.merged[rowSums(is.na(dat.merged)) > 0,]

    print(paste0('Total number of cases = ',sum(dat.merged$cases)))
    print(paste0('Unmatched cases = ',sum(dat.merged.na$cases)))

    saveRDS(dat.merged, paste0(dir.output,'medicare_',gsub(" ", "_", full_name),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds'))
}