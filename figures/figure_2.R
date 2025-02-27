#FIGURE 2: Percentage change in hospitalization rates across all time periods and relevant level 1 causes
library(tidyverse)
library(ggplot2)
#years of study
years=c(2000:2016)

output.folder <- #output directory

# load CCS level 1 names
code.lookup.merged = read.csv('CCS_DX.csv')
code.lookup.merged = subset(code.lookup.merged, !(code_chapter%in%c("Complications of pregnancy; childbirth; and the puerperium", 
                                                                    "Congenital anomalies",
                                                                    "Certain conditions originating in the perinatal period",
                                                                    "Symptoms; signs; and ill-defined conditions and factors influencing health status",
                                                                    "Residual codes; unclassified; all E codes [259. and 260.]")))
# make list of broad causes of hospitalization (level 1 names)
ccs_level_1 = unique(as.character(code.lookup.merged$code_chapter))

# load model summaries for CCS level 1 (adjusted)
dir.output.model.summary.adjusted = #path to CCS level 1 model summaries (adjusted)
dat.results.adjusted = data.frame()
for(cod in ccs_level_1){
  file.current = #CCS level 1 model summary
  if (file.exists(file.current)){
    print(cod)
    dat.results.adjusted.current = read.csv(file.current)
    dat.results.adjusted = rbind(dat.results.adjusted, dat.results.adjusted.current)}
}

dat.results.adjusted$cause[61:65] <- "Mental illness"
dat.results.adjusted$cause[11:15] <- "Endocrine, metabolic, and immunity disorders"

dat.results.level.1.adjusted <- dat.results.adjusted %>% group_by(cause) %>% summarise(err.mean = (sum(rr-1))/5,
                                                                                       err.mean.ll = (sum(rr.ll.bfc -1))/5,
                                                                                       err.mean.ul = (sum(rr.ul.bfc - 1))/5)

dat.results.adjusted$cause = factor(dat.results.adjusted$cause,
                                    levels=unique(dat.results.adjusted$cause))

dat.results.level.1.adjusted$cause = factor(dat.results.level.1.adjusted$cause,
                                    levels=unique(dat.results.level.1.adjusted$cause))



colors.ccs.level.1 <- c("#9E0142","#D53E4F","#F46D43","#FDAE61", "#FEE08B", "#FFF200",
                        "#E6F598", "#ABDDA4" ,"#66C2A5","#3288BD" ,"#9970AB", "#DE77AE","#74ADD1")

names(colors.ccs.level.1) <- unique(dat.results.adjusted$cause)

# reorder CCS level 1 causes for plotting
dat.results.adjusted$cause = factor(dat.results.adjusted$cause,
                                    levels=unique(dat.results.adjusted$cause))

dat.results.level.1.adjusted$cause <- factor(dat.results.level.1.adjusted$cause,
                                             levels=unique(dat.results.adjusted$cause))


colors.ccs.level.1.means <- c("#FDAE61", "#FFF200", "#ABDDA4", "#66C2A5", "#9970AB", "#FEE08B", 
                              "#E6F598", "#3288BD", "#F46D43","#9E0142","#DE77AE", "#74ADD1", "#D53E4F")
# ensure lags go correct order in plot
dat.results.adjusted$lag.factor = factor(dat.results.adjusted$lag, levels=c(5:0))

dat.results.adjusted <- dat.results.adjusted[,c(3,6,7,8,9)]
dat.results.adjusted$rr <- dat.results.adjusted$rr - 1
dat.results.adjusted$rr.ll.bfc <- dat.results.adjusted$rr.ll.bfc - 1
dat.results.adjusted$rr.ul.bfc <- dat.results.adjusted$rr.ul.bfc - 1

dat.results.level.1.adjusted$lag.factor <- ''
colnames(dat.results.level.1.adjusted) <- c("cause", "rr", "rr.ll.bfc", "rr.ul.bfc", "lag.factor")
dat.results.level.1.adjusted <- dat.results.level.1.adjusted[,c(2,3,4,1,5)]
  
dat.results <- rbind(dat.results.adjusted, dat.results.level.1.adjusted)
dat.results <- dat.results %>% arrange(cause)
dat.results$lag.factor <- factor(dat.results$lag.factor, levels = c("", 0, 1, 2, 3, 4))

# save plot output for Figure 2 (adjusted)
pdf(paste0(output.folder,'figure_2.pdf'),paper='a4r',width=0,height=0)
ggplot() +
  geom_errorbar(data=subset(dat.results.adjusted),aes(x=fct_inorder(lag.factor),ymax=rr.ll.bfc,ymin=rr.ul.bfc),width=.2,size=0.5) +
  geom_point(data=subset(dat.results.adjusted), aes(x=fct_inorder(lag.factor),y=rr),size=3,shape=16) +
  geom_point(data=subset(dat.results.adjusted), aes(x=fct_inrder(lag.factor),y=rr,color=cause),size=2,shape=16) + 
  geom_point(data=subset(dat.results[c(1, 7, 13, 19, 25, 31, 37, 43, 49, 55, 61, 67, 73),]), aes(x = lag.factor, y = rr, color = cause), size = 4, shape = 16) +
  geom_errorbar(data=subset(dat.results[c(1, 7, 13, 19, 25, 31, 37, 43, 49, 55, 61, 67, 73),]), aes(x= lag.factor, ymax = rr.ll.bfc, ymin=rr.ul.bfc, color = cause), width = .2, size = 0.5) +
  geom_hline(yintercept=0,linetype='dotted') +
  xlab('Lag (weeks after exposure)') + ylab('Percentage change in hospitalization rates associated with flood exposure') +
  facet_wrap(vars(cause),ncol=3) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colors.ccs.level.1) +
  guides(color="none") +
  coord_flip() +
  theme_bw() + theme(text = element_text(size = 11),
                     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
                     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()
