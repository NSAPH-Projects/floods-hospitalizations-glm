#FIGURE 2: plot of number of hospitalizations by cause by lag 
library(RColorBrewer)
library(ggplot2)
library(ggthemes)

library(dplyr)
library(stringr)
output.folder <- '/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/create_figures/'
# load ccs lookup
code.lookup.merged = read.csv('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/CCS_DX.csv')
code.lookup.merged = subset(code.lookup.merged, !(code_chapter%in%c("Complications of pregnancy; childbirth; and the puerperium", 
                                                                    "Congenital anomalies",
                                                                    "Certain conditions originating in the perinatal period",
                                                                    "Symptoms; signs; and ill-defined conditions and factors influencing health status",
                                                                    "Residual codes; unclassified; all E codes [259. and 260.]")))

# make list of broad causes of hospitalization (level 1 names)
causes_groups = unique(as.character(code.lookup.merged$code_chapter))

years <- c(2000:2016)

# directory to load data from
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')

floodzips_remove <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/floodzips_remove_JUN.rds')
load("/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/missing_zipcodes_APR.Rdata")
load("/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/AK_zips.Rdata")

rates <- data.frame()
for (i in 1:length(causes_groups)){
  # CCS level 1 input file
  input.file = paste0(dir.input,'medicare_',gsub(" ", "_", causes_groups[i]),'_rates_',years[1],'_',years[length(years)],'_APR.rds')
  dat <- readRDS(paste0(input.file))
  dat <- dat[!(dat$floodzip_id %in% c(floodzips_remove)),] 
  dat <- dat[!(dat$zipcode %in% c(missing_zipcodes, AK_zips)),]
  dat$control_indicator <- ifelse(dat$control_indicator == "2", "1", dat$control_indicator)
  for (j in c(1:5)){
    dat2 <- dat[seq(from = j, to = nrow(dat), by = 5),]
    dat2 <- dat2 %>% group_by(control_indicator) %>% summarise(rate = sum(cases_final)/sum(pt)) %>% select(rate)
    dat2 <- unname(unlist(c(dat2)))
    rates <- rbind(rates, dat2)
    }
  }
 

colnames(rates) <- c("exp", "con")

lags <- rep(c(0:4), 13)
causes <- c(rep(c(causes_groups), each = 5))


rates_vec_exp <- as.vector(rates$exp)
final_df_exp <- data.frame(lags, causes, rates_vec_exp)
print(final_df_exp)
final_df_exp$causes <- gsub("Mental Illness", "Mental illness", final_df_exp$causes)

rates_vec_con <- as.vector(rates$con)
final_df_con <- data.frame(lags, causes, rates_vec_con)
print(final_df_con)
final_df_con$causes <- gsub("Mental Illness", "Mental illness", final_df_con$causes)

causes_groups[13] <- "Mental illness"

# colors.ccs.level.1 <- c("#9E0142","#D53E4F","#F46D43","#FDAE61", "#FEE08B", "#FFF200",
#                         "#E6F598", "#ABDDA4" ,"#66C2A5" ,"#3288BD" ,"#9970AB", "#DE77AE","#74ADD1")
# names(colors.ccs.level.1) <- causes_groups

final_df <- cbind(final_df_exp, final_df_con$rates_vec_con)
colnames(final_df) <- c("lags", "causes", "rates_vec_exp", "rates_vec_con")
final_df2 <- data.frame(rep(final_df$lags, 2), rep(final_df$causes, 2), c(final_df$rates_vec_exp, final_df$rates_vec_con), rep(c("Event", "Control"), each = 65))
colnames(final_df2) <- c("lags", "causes", "rates", "indicator")

library(forcats)

p <- final_df2 %>%  
    mutate(indicator =  fct_relevel(indicator, "Event", "Control"))    %>%
    ggplot(.,aes(x=lags,y=rates)) +
    geom_bar(stat = "identity", position = position_dodge(), aes(fill = indicator)) +
    facet_grid(.~causes) +
    xlab('Lag (weeks after exposure)') + ylab('Hospitalization rate (cases/person-time)') +
    scale_y_continuous(limits = c(0, 0.00025), expand = c(0, 0)) +
    scale_fill_manual(values = c("black", "gray")) + 
    theme_bw() + theme(text = element_text(size = 15),
                       axis.title.y = element_text(margin=margin(b=1000)),
                       panel.grid.major = element_blank(),
                       plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),
                       legend.text=element_text(size=10),legend.title = element_blank(),
                       legend.position = 'bottom',legend.justification='center',
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
