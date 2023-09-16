# Figure 9. Percentage change in hospitalization rates with flood exposure by cause of hospitalization,
# proportion of residents that are Black and lag time. Lag time is measured in weeks after floods exposure.
# Dots show the point estimates and error bars represent Bonferroni-corrected 95% confidence intervals.

library(tidyverse)
library(ggplot2)
#years of study
years=c(2000:2016)

output.folder <- '/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/create_figures/'

# load CCS level 1 and 3 names
code.lookup.merged = read.csv('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/CCS_DX.csv')
code.lookup.merged = subset(code.lookup.merged, !(code_chapter%in%c("Complications of pregnancy; childbirth; and the puerperium", 
                                                                    "Congenital anomalies",
                                                                    "Certain conditions originating in the perinatal period",
                                                                    "Symptoms; signs; and ill-defined conditions and factors influencing health status",
                                                                    "Residual codes; unclassified; all E codes [259. and 260.]")))
# make list of broad causes of hospitalization (level 1 names)
ccs_level_1 = unique(as.character(code.lookup.merged$code_chapter))

# load model summaries for CCS level 1 (adjusted), severity 1
dir.output.model.summary.adjusted = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/Sensitivity/results_blk1_JUN/level1/unconstrained_dlm/summary/adjusted/')
dat.results.adjusted = data.frame()
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary.adjusted,'medicare_',gsub(" ", "_", cod),'_model_summary_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
        print(cod)
        dat.results.adjusted.current = read.csv(file.current)
        dat.results.adjusted = rbind(dat.results.adjusted, dat.results.adjusted.current)}
}

dat.results.adjusted$cause[61:65] <- "Mental illness"
dat.results.adjusted$cause[11:15] <- "Endocrine, metabolic, and immunity disorders"

dir.output.model.summary.adjusted = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/Sensitivity/results_blk2_JUN/level1/unconstrained_dlm/summary/adjusted/')
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary.adjusted,'medicare_',gsub(" ", "_", cod),'_model_summary_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
        print(cod)
        dat.results.adjusted.current = read.csv(file.current)
        dat.results.adjusted = rbind(dat.results.adjusted, dat.results.adjusted.current)}
}

dat.results.adjusted$cause[126:130] <- "Mental illness"
dat.results.adjusted$cause[76:80] <- "Endocrine, metabolic, and immunity disorders"

dat.results.adjusted$blk[1:65] <- "Zipcodes with % Black below 50th quantile"
dat.results.adjusted$blk[66:130] <- "Zipcodes with % Black above 50th quantile"

colors.blk <- c("#94C773", "#ba9ce4")

# reorder CCS level 1 causes for plotting
dat.results.adjusted$cause = factor(dat.results.adjusted$cause,
                                    levels=unique(dat.results.adjusted$cause))

# ensure lags go correct order in plot
dat.results.adjusted$lag.factor = factor(dat.results.adjusted$lag, levels=c(4:0))

# save plot output for Figure 5
pdf(paste0(output.folder,'figure_9_JUN.pdf'),paper='a4r',width=0,height=0)
ggplot() +
    geom_errorbar(data=subset(dat.results.adjusted),aes(x=as.factor(lag.factor),ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1,color=as.factor(blk)),width=0.5,size=0.5,alpha=1,position=position_dodge(width=0.7)) +
    geom_point(data=subset(dat.results.adjusted), aes(x=as.factor(lag.factor),y=rr-1,color=as.factor(blk)),size=2.5,shape=16,position=position_dodge(width=0.7)) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Lag (weeks after exposure)') + ylab('Percentage change in hospitalization rates associated with flood exposure') +
    facet_wrap(vars(cause),ncol=2) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.blk) +
    guides(color=guide_legend(title="",nrow=1)) +
    coord_flip() +
    theme_bw() + theme(text = element_text(size = 11),
                       panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
                       plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                       legend.justification='center',legend.box = "horizontal",legend.position=c(0.76, 0.07),
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()