#FIGURE 2: plot of number of hospitalizations by cause by lag 
library(RColorBrewer)
library(ggplot2)
library(ggthemes)

library(dplyr)
library(stringr)

output.folder <- #output directory 
# load ccs lookup
code.lookup.merged = read.csv('CCS_DX.csv')
code.lookup.merged = subset(code.lookup.merged, !(code_chapter%in%c("Complications of pregnancy; childbirth; and the puerperium", 
                                                                    "Congenital anomalies",
                                                                    "Certain conditions originating in the perinatal period",
                                                                    "Symptoms; signs; and ill-defined conditions and factors influencing health status",
                                                                    "Residual codes; unclassified; all E codes [259. and 260.]")))

# make list of broad causes of hospitalization (level 1 names)
causes_groups = unique(as.character(code.lookup.merged$code_chapter))

years <- c(2000:2016)


dir.input = # directory to load data from

floodzips_remove <- #path to file with flood-zipcode ID's to remove
load("missing_zipcodes.Rdata") #load missing zipcodes
load("AK_zips.Rdata") #load Alaska zipcodes

rates <- data.frame()
for (i in 1:length(causes_groups)){
  # CCS level 1 input file
  input.file = # CCS level 1 input file
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

final_df_exp$causes <- gsub("Mental Illness", "Mental illness", final_df_exp$causes)
final_df_exp$causes <- gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", "Endocrine, metabolic, and immunity disorders", final_df_exp$causes)

rates_vec_con <- as.vector(rates$con)
final_df_con <- data.frame(lags, causes, rates_vec_con)

final_df_con$causes <- gsub("Mental Illness", "Mental illness", final_df_con$causes)
final_df_con$causes <- gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", "Endocrine, metabolic, and immunity disorders", final_df_con$causes)

final_df <- cbind(final_df_exp, final_df_con$rates_vec_con)
colnames(final_df) <- c("lags", "causes", "rates_vec_exp", "rates_vec_con")
final_df$rate_ratio <- final_df$rates_vec_exp/final_df$rates_vec_con

cases <- data.frame()
for (i in 1:length(causes_groups)){
  # CCS level 1 input file
  input.file = # CCS level 1 input file
  dat <- readRDS(paste0(input.file))
  dat <- dat[!(dat$floodzip_id %in% c(floodzips_remove)),] 
  dat <- dat[!(dat$zipcode %in% c(missing_zipcodes, AK_zips)),]
  dat$control_indicator <- ifelse(dat$control_indicator == "2", "1", dat$control_indicator)
  for (j in c(1:5)){
    dat3 <- dat[seq(from = j, to = nrow(dat), by = 5),]
    dat3 <- dat2 %>% group_by(control_indicator) %>% summarise(total_cases = sum(cases_final)) 
    cases <- rbind(cases, dat3)
  }
}

cases$lag <- rep(c(0,0,1,1,2,2,3,3,4,4), 13)

cases$causes <- rep(causes_groups, each = 10)
cases$causes <- gsub("Mental Illness", "Mental illness", cases$causes)
cases$causes <- gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", "Endocrine, metabolic, and immunity disorders", cases$causes)

case_counts <- cases %>% group_by(causes) %>% summarise(total = sum(total_cases)) #auto sorts by alphabetical order of cause
case_counts <- case_counts[c(10,13,9,1,6,2,7,3,4,8,5,11,12),]

colors.ccs.level.1 <- c("#9E0142","#D53E4F","#F46D43","#FDAE61", "#FEE08B", "#FFF200",
                        "#E6F598", "#ABDDA4" ,"#66C2A5" ,"#3288BD" ,"#9970AB", "#DE77AE","#74ADD1")

causes_shortened <- c("Infectious #", "Neoplasms #", "Endocrine #", "Blood #", "Nervous #", "Circulatory #", "Respiratory #", "Digestive #", 
                      "Genitourinary #", "Skin #", "Musculoskeletal #", "Injuryy #", "Mental illness #")

library(forcats)
final_df$causes <- fct_inorder(as.factor(causes_shortened))
case_counts$causes <- fct_inorder(as.factor(causes_shortened))

names(colors.ccs.level.1) <- unique(final_df$causes)


global_labeller <- labeller(
  causes = label_wrap_gen(15))

p <- final_df %>%  
  ggplot(.,aes(x=lags,y=rate_ratio - 1)) +
  geom_bar(stat = "identity", aes(fill = causes)) +
  facet_wrap(.~causes, labeller = global_labeller, nrow = 2) +
  xlab('Lag (weeks after exposure)') + ylab('Crude hospitalization rate ratios (exposed/control)') +
  scale_y_continuous(breaks = c(-0.1,-0.05,0.0,0.05), labels = c(0.90,.95,1.0,1.05)) +
  scale_fill_manual(values = colors.ccs.level.1) +
  guides(fill = "none") +
  theme_bw() + theme(text = element_text(size = 11),
                     axis.title.y = element_text(margin=margin(b=1000)),
                     panel.grid.major = element_blank(),
                     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),
                     legend.text=element_text(size=10),legend.title = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
