# this code will take a particular cause of death (CCS level 1)
# and run the chosen model for it

#SA edited from RMP 

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1])

# years of analysis
years = c(2000:2016)

library(stringr)
library(dplyr)

# load ccs lookup
code.lookup.merged = read.csv('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/CCS_DX.csv')
code.lookup.merged = subset(code.lookup.merged, !(code_chapter%in%c("Complications of pregnancy; childbirth; and the puerperium", 
                                                                    "Congenital anomalies",
                                                                    "Certain conditions originating in the perinatal period",
                                                                    "Symptoms; signs; and ill-defined conditions and factors influencing health status",
                                                                    "Residual codes; unclassified; all E codes [259. and 260.]")))

# make list of broad causes of hospitalization (level 1 names)
causes_groups = unique(as.character(code.lookup.merged$code_chapter))

# process for finding broad causes of death and matching sub causes
causes_group = causes_groups[seedVal]
code.lookup.merged.subset = subset(code.lookup.merged, code_chapter==causes_group)
causes_to_load = unique(as.character(code.lookup.merged.subset$category_desc))
causes_to_load = trimws(causes_to_load)

# directory to load data from
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')

# CCS level 1 input file
input.file = paste0(dir.input,'medicare_',gsub(" ", "_", causes_group),'_rates_',years[1],'_',years[length(years)],'_APR.rds')

# check to see if a file exists for the analysis
if(file.exists(input.file)){
    print('data already processed... loading...')
    dat = readRDS(paste0(input.file))
}

# process if file doesn't already exists
if(!file.exists(input.file)){

    print('data not processed... processing now...')

    # load hospitalizations data
    dat.all = data.frame()
    for(cod.arg in causes_to_load){
        cod.arg = gsub(" ", "_", cod.arg) ; cod.arg = gsub("/", "_", cod.arg)
        input.file.2 = paste0(dir.input,'medicare_',cod.arg,'_rates_',years[1],'_',years[length(years)],'_APR.rds')
        if(file.exists(input.file.2)){
            print(cod.arg)
            dat = readRDS(input.file.2)
            print(head(dat))
            dat.all=rbind(dat.all,dat)
        }
        if(!file.exists(input.file.2)){
            print(paste0('Cannot find ', cod.arg, ', so skipping over...'))
        }
    }
    
    print(dat.all[1:30,])
    # resummarise by CCS level 1

    dat <- dat.all %>% group_by(floodzip_id, control_indicator) %>% mutate(cases_final = case_when(exposed == 1 ~ sum(exposed * cases),
                                                                                lag_wk1 == 1 ~ sum(lag_wk1 * cases),
                                                                                lag_wk2 == 1 ~ sum(lag_wk2 * cases),
                                                                                lag_wk3 == 1 ~ sum(lag_wk3 * cases),
                                                                                lag_wk4 == 1 ~ sum(lag_wk4 * cases)))
    dat <- as.data.frame(dat)
    dat <- dat %>% select(-c(cases))
    dat <- unique.data.frame(dat)
    print(dat[1:30,])
    
    #change back to correct array (control rows are 0 all the way across)
    dat$exposed <- ifelse(dat$control_indicator == 1 | dat$control_indicator == 2, 0, dat$exposed)
    dat$lag_wk1 <- ifelse(dat$control_indicator == 1 | dat$control_indicator == 2, 0, dat$lag_wk1)
    dat$lag_wk2 <- ifelse(dat$control_indicator == 1 | dat$control_indicator == 2, 0, dat$lag_wk2)
    dat$lag_wk3 <- ifelse(dat$control_indicator == 1 | dat$control_indicator == 2, 0, dat$lag_wk3)
    dat$lag_wk4 <- ifelse(dat$control_indicator == 1 | dat$control_indicator == 2, 0, dat$lag_wk4)
        
    # load for future attempts so do not need to waste lots of time processing data again
    saveRDS(dat,input.file)

    # get rid to save space
    rm(dat.all)
}

floodzips_remove <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/floodzips_remove_JUN.rds')
dat <- dat[!(dat$floodzip_id %in% c(floodzips_remove)),]
print(dat[1:30,])
nrow(dat)

    # merging temperature data with model matrix 
    dat.temp <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/model_run/gridmet/dat.temperature.final_JUN.rds')
    dat$tmmx <- dat.temp$tmmx
    
    # merging humidity data with model matrix 
    dat.hum <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/model_run/gridmet/dat.humidity.final_JUN.rds')
    dat$rmax <- dat.hum$rmax
    
    # merging windspeed data with model matrix 
    dat.wind <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/model_run/gridmet/dat.windspeed.final_JUN.rds')
    dat$vs <- dat.wind$vs

dat.sample <- dat
print(dat.sample[1:30,])

# make year numeric 
dat.sample$year = as.numeric(dat.sample$year)


# create log of population
dat.sample$logpt <- log(dat.sample$pt)

USA_table <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/data/flood_info/USA_table_2000_2018.rds')

dat.sample$ID <- as.numeric(str_sub(dat.sample$floodzip_id, 1, 4))

dat.sample <- inner_join(dat.sample, USA_table[,c("id", "main_cause", "severity")], by = c("ID" = "id"))
dat.sample <- dat.sample %>% mutate(severity = case_when(severity == 1 ~ 1,
                                                         severity == 1.5 ~ 2,
                                                         severity == 2 ~ 2))

library(gnm) ; library(splines) ; library(dlnm)


# temperature of the recorded flooded days (did not adjust for day before as of 1/18/2023)
cb1.temp = crossbasis(dat.sample$tmmx,argvar=list("ns", df=2),lag=0, arglag=list("ns", df=2))

# humidity of the recorded flooded days (did not adjust for day before as of 1/18/2023)
cb1.hum = crossbasis(dat.sample$rmax,argvar=list("ns", df=2),lag=0, arglag=list("ns", df=2))

# windspeed of the recorded flooded days (did not adjust for day before as of 1/18/2023)
cb1.wind = crossbasis(dat.sample$vs,argvar=list("ns", df=2),lag=0, arglag=list("ns", df=2))

# distributed lag unconstrained
system.time
({

    mod_dlm_unconstrained_adjusted = gnm(cases_final ~ exposed + lag_wk1 + lag_wk2 + lag_wk3 + lag_wk4 + cb1.temp + cb1.hum + cb1.wind +
                                ns(year, df=2), data=dat.sample, offset=logpt, eliminate=factor(floodzip_id), family=quasipoisson)

})

# distributed lag unconstrained
system.time({

    mod_dlm_unconstrained_unadjusted = gnm(cases_final ~ exposed + lag_wk1 + lag_wk2 + lag_wk3 + lag_wk4 + 
                                    ns(year, df=2), data=dat.sample, offset=logpt, eliminate=factor(floodzip_id), family=quasipoisson)

})

# make list of broad causes of hospitalization
causes_groups_final=c('Cardiovascular system diseases','Respiratory system diseases','Neoplasms','Injury and poisoning','Mental illness',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary system diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous tissue diseases')

# bonferroni correction CI calculation
alpha = 100 * (0.05/length(causes_groups_final))
corrected.ci = round(100 - alpha,1) / 100

# provide summary for adjusted model
lag_est_mean = as.data.frame(exp(mod_dlm_unconstrained_adjusted$coefficients[1:5]))
lag_est_uncertainty = exp(confint.default(mod_dlm_unconstrained_adjusted)[c(1:5),])
lag_est_uncertainty_bf_corrected = exp(confint.default(mod_dlm_unconstrained_adjusted,level=corrected.ci)[c(1:5),])

dat.results = data.frame(lag=c(0:4),
                        rr=lag_est_mean,rr.ll=lag_est_uncertainty[,1],rr.ul=lag_est_uncertainty[,2],
                        rr.ll.bfc=lag_est_uncertainty_bf_corrected[,1],rr.ul.bfc=lag_est_uncertainty_bf_corrected[,2])

dat.results$cause = causes_group
rownames(dat.results) = seq(nrow(dat.results))
names(dat.results)[2] = 'rr'

# provide summary for unadjusted model
lag_est_mean_unadj = as.data.frame(exp(mod_dlm_unconstrained_unadjusted$coefficients[1:5]))
lag_est_uncertainty_unadj = exp(confint.default(mod_dlm_unconstrained_unadjusted)[c(1:5),])
lag_est_uncertainty_bf_corrected_unadj = exp(confint.default(mod_dlm_unconstrained_unadjusted,level=corrected.ci)[c(1:5),])

dat.results_unadj = data.frame(lag=c(0:4),
                        rr=lag_est_mean_unadj,rr.ll=lag_est_uncertainty_unadj[,1],rr.ul=lag_est_uncertainty_unadj[,2],
                        rr.ll.bfc=lag_est_uncertainty_bf_corrected_unadj[,1],rr.ul.bfc=lag_est_uncertainty_bf_corrected_unadj[,2])

dat.results_unadj$cause = causes_group
rownames(dat.results_unadj) = seq(nrow(dat.results_unadj))
names(dat.results_unadj)[2] = 'rr'

# output directory
dir.output.plots = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/model_run/figures_JUN/level1/unconstrained_dlm/')
dir.output.model.summary = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/model_run/results_JUN/level1/unconstrained_dlm/summary/')
dir.output.model.output = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/model_run/results_JUN/level1/unconstrained_dlm/model_output/')

ifelse(!dir.exists(dir.output.plots), dir.create(dir.output.plots, recursive=TRUE), FALSE)
ifelse(!dir.exists(dir.output.model.summary), dir.create(dir.output.model.summary, recursive=TRUE), FALSE)
ifelse(!dir.exists(dir.output.model.output), dir.create(dir.output.model.output, recursive=TRUE), FALSE)

# save summary of model results in a way which can be compiled later
saveRDS(mod_dlm_unconstrained_adjusted, paste0(dir.output.model.output,'medicare_',gsub(" ", "_", causes_group),'_model_summary_',years[1],'_',years[length(years)],'.rds'))

# save w temperature + humidity + windspeed model summary
write.csv(dat.results, paste0(dir.output.model.summary,'medicare_',gsub(" ", "_", causes_group),'_model_summary_',years[1],'_',years[length(years)],'.csv'))

# save w/o temperature + humidity + windspeed model summary
write.csv(dat.results_unadj, paste0(dir.output.model.summary,'medicare_',causes_group,'_model_summary_unadjusted_',years[1],'_',years[length(years)],'.csv'))

library(ggplot2)

pdf(paste0(dir.output.plots,'medicare_rr_values_model_summary_',gsub(" ", "_", causes_group),'_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',height=0,width=0)
print(
    ggplot(subset(dat.results)) +
    geom_point(aes(x=lag,y=rr)) +
    geom_errorbar(aes(x=lag,ymin=rr.ll.bfc,ymax=rr.ul.bfc)) +
    geom_hline(yintercept=1,linetype=2) +
    ggtitle(paste0(gsub("_", " ", causes_group),' (unconstrained dlm)')) +
    xlab('Lag') + ylab('RR') +
    ylim(c(0.8,1.3)) +
    theme_bw() + theme(text = element_text(size = 8),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)

dev.off()
