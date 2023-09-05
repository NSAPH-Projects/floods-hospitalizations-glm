#FIGURE 1, panel A: plot of number of floods in a state over 2000-2016 (does remove zipcodes or floods that we exclude) - 6/23/2024
library(dplyr)
library(stringr)

library(raster)
library(sf)
library(tmap)
library(usmap)


library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(ggpubr)

output.folder <- '/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/create_figures/'

load('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/data/flood_info/zipcode_flood_subset_2000_2016_APR.Rdata') #path !!!
flood_data_test$floodzip_id <- paste0(flood_data_test$flood_id, "_flood_df_", flood_data_test$zip)

floodzips_remove <- readRDS('/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/floodzips_remove_JUN.rds')
load("/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/missing_zipcodes_APR.Rdata")
load("/n/dominici_nsaph_l3/Lab/projects/floods-hospitalizations-glm/medicare_processing/data_update/AK_zips.Rdata")

flood_data_test<- flood_data_test[!(flood_data_test$floodzip_id %in% c(floodzips_remove)),] 
flood_data_test <- flood_data_test[!(flood_data_test$zip %in% c(missing_zipcodes, AK_zips)),]

tab_by_state <- as.data.frame(table(flood_data_test$state_abbrev, flood_data_test$flood_id))
tab_by_state$indicator <- ifelse(tab_by_state$Freq > 0, 1, 0)
final_count <- tab_by_state %>% group_by(Var1) %>% summarise(sum(indicator))

dat <- usmap::us_map(regions = c("states"))
state_mappy <- left_join(dat, final_count, by = c("abbr" = "Var1"))
state_mappy[is.na(state_mappy)] <- 0

#change color palette 
colorpalette <- c('cornsilk',"lightblue", "blue","darkblue","black")
sf_use_s2(FALSE)
names(colorpalette) <- seq(0,40,10)

state_mappy2 <- state_mappy %>% filter(!(abbr %in% c("HI", "AK")))
state_map_flood <- ggplot() + geom_polygon(data = state_mappy2, 
                                           aes(x = x, y = y, group = group, fill = `sum(indicator)`), 
                                           color = "black", size = 0.2) + scale_fill_gradientn(name="Number of floods", colors = colorpalette,
                                                                                               limits = c(0,40), breaks=c(5,10,15,20,25,30,35)) +
  theme(legend.position = "bottom") +
  ggtitle("") + xlim("") + ylim("") +
  xlab("") + ylab("")


#FIGURE 1, panel B: plot of exposed population 

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

for (i in 1:length(causes_groups)){
  # CCS level 1 input file
  input.file = paste0(dir.input,'medicare_',gsub(" ", "_", causes_groups[i]),'_rates_',years[1],'_',years[length(years)],'_APR.rds')
  dat <- readRDS(paste0(input.file))
  dat <- dat[!(dat$floodzip_id %in% c(floodzips_remove)),] 
  dat <- dat[!(dat$zipcode %in% c(missing_zipcodes, AK_zips)),]
  dat2 <- dat %>% filter(control_indicator == 0 & lag_wk1 == 1) %>% mutate(pt = pt/7) %>% group_by(year) %>% summarise(pop_exposed = sum(pt))
}

pop <- ggplot(data=dat2, aes(x=year, y=pop_exposed, group = 1)) +
  geom_line()+
  geom_point()+
  xlab("Year") +
  ylab("Population impacted by flood exposure")

figure1 <- ggarrange(state_map_flood, pop, nrow=1,ncol=2,labels=c("",""),
               common.legend = FALSE)

  
