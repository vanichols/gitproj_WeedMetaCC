##############
##  March 23 2018
## Author: Gina Nichols
## Purpose: Process CC database from google sheets
##
#############

####==SETS WORKING DIRECTORY TO WHEREVER ACTIVE DOCUMENT IS KEPT===####
#
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


## load packages
library(googlesheets)
library(tidyverse)
library(lubridate)

## Read in google sheet

# which google sheets do you have access to?
# may ask you to authenticate in a browser!
gs_ls()

# get the cover crop sheet
cc <- gs_title("Cover crop - weeds meta-analysis")

# list worksheets in that file
gs_ws_ls(cc)

# "download" one of the sheets using gs_read()
db <- gs_read(ss=cc, ws = "main database")

# convert to tibble
df <- as_tibble(db)


##------Fix dates

# cc_pDATE, cc_termDATE, crop_pDATE
df <- df%>%
  # Fix dates
  mutate(cc_plube = mdy(cc_pDATE),
         cc_tlube = mdy(cc_termDATE),
         crop_plube = mdy(crop_pDATE),
         cc_pDOY = yday(cc_plube),
         cc_termDOY = yday(cc_tlube),
         crop_pDOY = yday(crop_plube)) %>%
  mutate_if(is.character, as.factor) %>%
  # Fix 0 values by adding 0.01
  mutate(cced_wd.den_num.m2 = cced_wd.den_num.m2 + 0.001,
         ctl_wd.den_num.m2 = ctl_wd.den_num.m2 + 0.001,
         cced_wd.bm_g.m2 = cced_wd.bm_g.m2 + 0.001,
         ctl_wd.bm_g.m2 = ctl_wd.bm_g.m2 + 0.001) %>%
  # Calculate log response ratio
  mutate(LR_wBM = log(cced_wd.bm_g.m2 / ctl_wd.bm_g.m2),
         LR_wDEN = log(cced_wd.den_num.m2 / ctl_wd.den_num.m2),
         LR_yield = log(cced_yield_kg.ha / ctl_yield_kg.ha))

####-----------Weed Biomass---####

dfBM <- df %>% arrange(LR_wBM) %>%
  mutate(rank_BM = 1:length(df$pub_reference))


png("../_figs/Rank_WeedBMResp.png", width=650, height=450, res=100)

ggplot(dfBM, aes(LR_wBM, rank_BM)) +
  geom_jitter(aes(color = cc_type, size = 2), alpha = 0.5, width = 0.3) + 
  geom_vline(xintercept=0, size = 1.5) + 
  xlab("Log Response, (+) = more weed biomass with cover crop") + 
  guides(size = FALSE) + 
  ggtitle("Effect of Cover Crop on Weed Biomass")

dev.off()


png("../_figs/Violin_WeedBMResp.png", width=650, height=450, res=100)

ggplot(filter(dfBM, cc_type %in% c("grass", "legume")), aes(x = cc_type, y = LR_wBM)) +
  geom_violin(aes(fill = cc_type)) + 
  geom_hline(yintercept = 0, size = 1.5) + 
  ylab("Log Response Ratio") + 
  xlab("Cover Crop Type") + 
  guides(fill=FALSE) + 
  theme_bw() + 
  annotate(geom="text", x=0.7, y=11, label="Cover Crops\nIncrease Weed BM",
           color="gray30", fontsize=13, fontface="italic") +
  annotate(geom="text", x=0.7, y=-4, label="Cover Crops\nDecrease Weed BM",
           color="gray30", fontsize=13, fontface="italic") +
  ggtitle("Effect of Cover Crop on Weed Biomass")


dev.off()

####-----------Weed density---####

dfDEN <- df %>% arrange(LR_wDEN) %>%
  mutate(rank_BM = 1:length(df$pub_reference))


png("../_figs/Rank_WeedDENResp.png", width=650, height=450, res=100)

ggplot(dfDEN, aes(LR_wDEN, rank_BM)) +
  geom_jitter(aes(color = cc_type, size = 2), alpha = 0.5, width = 0.3) + 
  geom_vline(xintercept=0, size = 1.5) + 
  xlab("Log Response, (+) = more weeds with cover crop") + 
  guides(size = FALSE) +
  ggtitle("Effect of Cover Crop on Weed Density")

dev.off()


png("../_figs/Violin_WeedDENResp.png", width=650, height=450, res=100)

ggplot(filter(dfDEN, cc_type %in% c("grass", "legume")), aes(x = cc_type, y = LR_wDEN)) +
  geom_violin(aes(fill = cc_type)) + 
  geom_hline(yintercept = 0, size = 1.5) + 
  ylab("Log Response Ratio") + 
  xlab("Cover Crop Type") + 
  guides(fill=FALSE) + 
  theme_bw() + 
  annotate(geom="text", x=0.7, y=11, label="Cover Crops\nIncrease Weed Density",
           color="gray30", fontsize=13, fontface="italic") +
  annotate(geom="text", x=0.7, y=-4, label="Cover Crops\nDecrease Weed Density",
           color="gray30", fontsize=13, fontface="italic") +
  ggtitle("Effect of Cover Crop on Weed Density")

dev.off()


####-----------Weed density response vs biomass---####

png("../_figs/Reg_WeedDEN_vs_CCbiomass.png", width=650, height=450, res=100)

ggplot(filter(df, cc_bm_kg.ha > 50), aes(cc_bm_kg.ha, LR_wDEN)) + 
  geom_hline(yintercept = 0, size = 1.5) +
  geom_point(aes(color = cc_type), size = 2) + 
  theme_bw() + 
  geom_smooth(color = "blue", se = F, size = 2) +
  geom_smooth(method = lm, color = "red", se = F, size = 2) +
  
  annotate(geom="text", x=4000, y=8, label="Cover Crops\nIncrease Weed Density",
           color="gray30", fontsize=13, fontface="italic") +
  annotate(geom="text", x=4000, y=-3, label="Cover Crops\nDecrease Weed Density",
           color="gray30", fontsize=13, fontface="italic") +
  ggtitle("Effect of Cover Crop Biomass on Weed Density") + 
  xlab("Cover Crop Biomass [kg ha-1]") + 
  ylab("Log of Response Ratio")

dev.off()

####-----------Weed biomass response vs biomass---####

png("../_figs/Reg_WeedBM_vs_CCbiomass.png", width=650, height=450, res=100)

ggplot(filter(df, cc_bm_kg.ha > 50), aes(cc_bm_kg.ha, LR_wBM)) + 
  geom_hline(yintercept = 0, size = 1.5) +
  geom_point(aes(color = cc_type), size = 2) + 
  theme_bw() + 
  geom_smooth(color = "blue", se = F, size = 2) +
  geom_smooth(method = lm, color = "red", se = F, size = 2) +
  
  annotate(geom="text", x=4000, y=10, label="Cover Crops\nIncrease Weed Biomass",
           color="gray30", fontsize=13, fontface="italic") +
  annotate(geom="text", x=4000, y=-5, label="Cover Crops\nDecrease Weed Biomass",
           color="gray30", fontsize=13, fontface="italic") +
  ggtitle("Effect of Cover Crop Biomass on Weed Biomass") + 
  xlab("Cover Crop Biomass [kg ha-1]") + 
  ylab("Log of Response Ratio")

dev.off()


####-----------CC biomass response vs planting day---####

png("../_figs/Reg_CCbiomass_vs_CCplantingDOY.png", width=650, height=450, res=100)

ggplot(df, aes(cc_pDOY, cc_bm_kg.ha)) + 
  geom_vline(xintercept = 244, size = 1.5, color = "red", alpha = 0.5) +
  geom_point(aes(color = cc_type), size = 4) + 
  theme_bw() +
  annotate(geom="text", x=250, y=50, label="September 1",
           color="gray30", fontsize=13, fontface="italic") +
  ggtitle("Effect of Planting Date on Cover Crop Biomass") + 
  ylab("Cover Crop Biomass [kg ha-1]") + 
  xlab("Day of Year")

dev.off()

png("../_figs/Reg_CCbiomass_vs_CCterminationDOY.png", width=650, height=450, res=100)

ggplot(filter(df, cc_type %in% c("grass", "legume")), aes(cc_termDOY, cc_bm_kg.ha)) + 
  geom_vline(xintercept = 121, size = 1.5, color = "blue", alpha = 0.5) +
  geom_point(aes(color = cc_type), size = 4) + 
  theme_bw() +
  geom_smooth(method = lm, se = F, color = "red", size = 2) +
  annotate(geom="text", x=124, y=7500, label="May 1",
           color="gray30", fontsize=13, fontface="italic") +
  ggtitle("Effect of Planting Date on Cover Crop Biomass") + 
  ylab("Cover Crop Biomass [kg ha-1]") + 
  xlab("Day of Year") + 
  facet_grid(cc_type~.)

dev.off()


####-----------win-win lose-lose---####

png("../_figs/WW-LL_Yields.png", width=650, height=450, res=100)

ggplot(df, aes(LR_wBM, LR_yield)) +
  geom_vline(xintercept=0, size = 1.5) + 
  geom_hline(yintercept = 0, size = 1.5) + 
  geom_point(aes(color = cc_type, size = 2), alpha = 0.5, width = 0.3) + 
  xlab("Log Response Weed Biomass, (+) = more weed biomass with cover crop") + 
  ylab("Log Response Yield, (+) = more grain yield with cover crop") + 
  guides(size = FALSE) + 
  ggtitle("Win-Win or Lose-Lose Scenarios") +
  
  annotate(geom="text", x=-3, y=6, label="Win-Win; 14%",
           color="red", fontsize=13, fontface="italic") +
  
  annotate(geom="text", x=8, y=6, label="Lose Weeds-Win Yield; 3%",
           color="red", fontsize=13, fontface="italic") +
  
  annotate(geom="text", x=-3, y=-4, label="Win Weeds-Lose Yield; 21%",
           color="red", fontsize=13, fontface="italic") +
  
  annotate(geom="text", x=8, y=-4, label="Lose-Lose; 62%",
           color="red", fontsize=13, fontface="italic")

dev.off()
