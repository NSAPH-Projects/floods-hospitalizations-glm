# this code will take all processed years and load them
# then process them together because some admissions are in the previous year

#SA edited from RMP - 5/26/2022

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part2.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# years of data
years = c(2000:2014)

# codes from SSA_STATE_CD info here https://www.resdac.org/cms-data/variables/medpar-beneficiary-residence-ssa-standard-state-code

# location of files
dir.input = paste0('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/in_progress/')

# loop through and load processed admissions file for next stage of processing
dat.all = data.frame()
for(year in years){
    print(paste0('loading ',year))

    # load current year's medicare data
    dat.current = readRDS(paste0(dir.input,'medicare_admissions_processing_',year,'.rds'))

    # fix date categories (partially recoded from RMP)
    dat.current$date = format(as.Date(dat.current$ADATE, "%d%B%Y"))
    dat.current$year = year(date)
    dat.current$month = month(date)
    dat.current$day = day(date)

    # fix ccs category
    dat.current$ccs_category = as.numeric(dat.current$ccs_category)

    # summarising by date of admission (which isn't necessarily in the file for the year on record)
    # hospitalizations are dated by discharge year which may not always be in the same year of admission 
    print('summarising file...')
    library(plyr)
    dat.current = ddply(dat.current,.(SSA_STATE_CD,SSA_CNTY_CD,ccs_category,day,month,year),summarise,cases=sum(cases))
    dat.all=rbind(dat.all,dat.current)
}

# summarise one final time to get over entire multi-year period
dat_admissions_sum_total = ddply(dat.all,.(SSA_STATE_CD, SSA_CNTY_CD,ccs_category,day,month,year),summarise,cases=sum(cases))

#reorder
dat_admissions_sum_total = dat_admissions_sum_total[order(dat_admissions_sum_total$SSA_STATE_CD,dat_admissions_sum_total$SSA_CNTY_CD,dat_admissions_sum_total$ccs_category,dat_admissions_sum_total$year,dat_admissions_sum_total$month,dat_admissions_sum_total$day),]

# save processed admissions file
dir.output.local = paste0('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/in_progress/')
ifelse(!dir.exists(dir.output.local), dir.create(dir.output.local, recursive=TRUE), FALSE)
saveRDS(dat_admissions_sum_total, paste0(dir.output.local,'medicare_admissions_',years[1],'_',years[length(years)],'.rds'))

