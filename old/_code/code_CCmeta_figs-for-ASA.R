#############################
##
# Author: Gina Nichols
#
# Date Created: Oct 22
#
# Date last modified: Oct 22 2018 - by Gina
#
# Purpose: Create figures for ASA presentation
#
# Inputs: td_cc-database-clean
#
# Outputs: 
#
# NOtes: 
#
##############################

library(tidyverse)
library(usmap) # pre-loaded maps package
library(ggridges) # makes nice distribution plots

# Set working directory
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

# Read in data ------------------------------------------------------------
dat <- read_csv("../_tidydata/td_cc-database-clean.csv") %>%
  mutate(tmeth_nice = ifelse(cc_termMETH2 == "W", "Winterkill", NA),
         tmeth_nice = ifelse(cc_termMETH2 == "M", "Mechanical", tmeth_nice),
         tmeth_nice = ifelse(cc_termMETH2 == "H", "Herbicide", tmeth_nice),
         tmeth_nice = ifelse(cc_termMETH2 == "D", "Combo Mech/Herb", tmeth_nice))



# Get map data ------------------------------------------------------------


map_all <- as.tibble(map_data('state'))

map_crnblt <- map_all %>%
  filter(region %in% c("illinois", "iowa", "indiana", 
                       "michigan", "minnesota", "missouri", 
                       "nebraska", "ohio", "wisconsin"))


# Figure 1 - map ----------------------------------------------------------


ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), fill = "white", color = "gray80") +
  geom_point(data = dat, pch = 21, size = 4, color = "black", fill = "red",
             aes(x = long, y = lat)) 

ggsave("../_figs/map-sites.png")


# Figure 2 - distribution of points ----------------------------------------------------------

dat_lab <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  group_by(resp) %>%
  summarise(n = n())

dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  
  ggplot(aes(LRR, resp)) + 
  
  geom_density_ridges(aes(fill = resp), alpha = 1, scale = 1.1) + 
  geom_text(data = dat_lab, aes(-8, resp, label = paste0("n = ", n)), 
            vjust = -1, color = "gray50", fontface = "italic") + 
  geom_text(x = -5, y = 3, label = "Cover Crop Decrease Weeds", 
            fontface = "italic", color = "gray50", size = 5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) + 
  
  labs(x = "Log of Response Ratio", y = NULL) +
  guides(fill = F) + 
  scale_fill_manual(values = c("orange", "purple")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14))

ggsave("../_figs/data-distribution.png")


# Fig 3 - CC species -----------------------------------------------------

dat_lab3 <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  # mix and brassica only have 2/3 points
  filter(cc_type != "mix", cc_type != "brassica") %>%
  group_by(resp, cc_type) %>%
  summarise(n = n())

dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  # mix and brassica only have 2/3 points
  filter(cc_type != "mix", cc_type != "brassica") %>%
  
  # Pipe new data to ggplot2
  
  ggplot(aes(LRR, cc_type)) + 
  geom_density_ridges(aes(fill = resp), alpha = 1, scale = 1.1) + 
  geom_text(data = dat_lab3, aes(-7, cc_type, label = paste0("n = ", n)), 
            vjust = -0.5, hjust = 0, color = "gray50", fontface = "italic") + 
  
  #geom_text(x = -5, y = 3, label = "Cover Crops Decrease Weeds", 
  #          fontface = "italic", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) + 
  
  facet_grid(~resp) + 
  
  labs(x = "Log of Response Ratio", y = NULL) +
  guides(fill = F) + 
  scale_fill_manual(values = c("orange", "purple")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave("../_figs/data-by-cc-type.png")


# Fig 4 - Termination method -----------------------------------------------------

# Get n values for labels
#
dat_lab4 <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  group_by(resp, cc_termMETH2, tmeth_nice) %>%
  summarise(n = n()) %>%
# D = dual, M = mechanical, H = herb, W = winterkill
  filter(!is.na(cc_termMETH2))

# Create figure
#

# Prepare data
dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  filter(!is.na(cc_termMETH2)) %>%
  
# Pipe new data to ggplot  
  ggplot(aes(LRR, tmeth_nice)) + 
  geom_density_ridges(aes(fill = resp), alpha = 1, scale = 1.1) + 
  geom_text(data = dat_lab4, aes(-7, tmeth_nice, label = paste0("n = ", n)), 
            vjust = -0.5, hjust = 0, color = "gray50", fontface = "italic") + 
  
  #geom_text(x = -5, y = 3, label = "Cover Crops Decrease Weeds", 
  #          fontface = "italic", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) + 
  
  facet_grid(~resp) + 
  
  labs(x = "Log of Response Ratio", y = NULL) +
  guides(fill = F) + 
  scale_fill_manual(values = c("orange", "purple")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave("../_figs/data-by-term-meth2.png")

