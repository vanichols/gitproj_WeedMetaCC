#############################
##
# Author: Gina
#
# Date Created: Sept 11 2019
#
# Date last modified: Oct 3 (added modifier analyses)
#                     Dec 6 2019 (made separate file for yields LRR)
#                     Jan 2 2020 (look at other mods?)
#
# Purpose: Fit linear models to get estimates of effect sizes
#
# Inputs: td_cc-database-clean-long
##############################


rm(list=ls())
library(lme4)
library(ggplot2)
library(tidyverse)
library(lmerTest)

source("_code/code_00_functions.R")

# # what did I test for weeds?
# themods <- c("cropsys_tillage", 
#              "msmt_season", "msmt_planting", 
#              "weed_group", "ccterm_meth", 
#              "crop_follow")

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv") %>% 
  # select(obs_no, study, reps, wgt, resp, yieldLRR, 
  #        cropsys_tillage, msmt_season, msmt_planting,
  #        weed_group, ccterm_meth, crop_follow) %>% 
  select(-LRR) %>% 
  filter(!is.na(yieldLRR)) %>% 
  rename("LRR" = yieldLRR)

bio <- dat %>% filter(resp == "bio")
den <- dat %>% filter(resp == "den")

all <- read_csv("_tidydata/td_cc-database-clean.csv") %>% 
  filter(!is.na(yieldLRR)) %>% 
  mutate(LRR = yieldLRR,
         resp = "all") 


# fit full models on yield ------------------------------------------------

ybio <- RunModelNoModsFun(mydata = den, resp = "bio")
yden <- RunModelNoModsFun(mydata = bio, resp = "den")
yall <- RunModelNoModsFun(mydata = all, resp = 'all')

yall2 <- summary(lmer(LRR ~ cc_type2 + (1|study), weights = wgt, data = all))
yall2 <- summary(lmer(LRR ~ 1 + (1|study), weights = wgt, data = all))


yres <- bind_rows(ybio, yden) %>%
  mutate_if(is.numeric, round, 3) 

yres %>%  write_csv("_tidydata/sd_overall-yield.csv")


# do select contrasts ------------------------------------------------------------

themods <- c("cropsys_tillage",
             "msmt_season", "msmt_planting",
             "weed_group", "ccterm_meth",
             "crop_follow", "cc_type2")

#--does cctype affect yield? No. bio: p=0.2, den:p=0.3
denmc <- RunModelModsContrastFun(mydata = den, mymod = themods[7], myresp = "den")
biomc <- RunModelModsContrastFun(mydata = bio, mymod = themods[7], myresp = "bio")
allmc <- RunModelModsContrastFun(mydata = all, mymod = themods[7], myresp = "all")

#--does ccbio differ for yields? No. 
denbio <- summary(lmer(LRR ~ cc_bio_Mgha + (1|study), weights = wgt, data = den))
biobio <- summary(lmer(LRR ~ cc_bio_Mgha + (1|study), weights = wgt, data = bio))
allbio <- summary(lmer(LRR ~ cc_bio_Mgha + (1|study), weights = wgt, data = filter(all, cc_bio_Mgha < 8)))

all %>%
  filter(cc_bio_Mgha < 8) %>% 
  ggplot(aes(cc_bio_Mgha, LRR)) +
  geom_point()


# fit model w/modifier ----------------------------------------------------

#--density
denmr <- RunModelModsFun(mydata = den, mymod = themods[1], myresp = "den")
for (i in 2:length(themods)) {
  tmp <- RunModelModsFun(mydata = den, mymod = themods[i], myresp = "den")
  denmr <- bind_rows(denmr, tmp)
}

#--biomass
biomr <- RunModelModsFun(mydata = bio, mymod = themods[1], myresp = "bio")
for (i in 2:length(themods)){
  tmp <- RunModelModsFun(mydata = bio, mymod = themods[i], myresp = "bio")
  biomr <- bind_rows(biomr, tmp)
}

#--combine and write
denmr %>%
  bind_rows(biomr) %>%
  rename(modlvl = desc) %>%
  write_csv("_tidydata/sd_mods-yield.csv")

