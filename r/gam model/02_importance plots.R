###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Plot importance scores
# Author:  Claude Spencer
# Date:    May 2024
## 

# Clear objects from your environment
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyverse)
library(patchwork)

# Set your working directory to the project's directory
setwd(here::here())

# Set the study name
name <- '2024_Albany_stereo-BRUVs'
name <- "Parks-Ningaloo-synthesis"

# Read in the data
dat <- read.csv(paste("model out", paste(name, "all.var.imp.csv", sep = "_"), sep = "/")) %>% 
  dplyr::rename(resp.var = X) %>%
  pivot_longer(cols = c(eveyrthing but resp.vars), names_to = "predictor", values_to = "importance") %>%
  # gather(key = predictor, value = importance,2:ncol(.)) %>%
  glimpse()

dat.taxa <- dat %>%
  dplyr::mutate(label = case_when(predictor %in% "" & resp.var %in% "", "X",
                                  .default = NA)) %>%
  glimpse()

# colour ramps-
re <- colorRampPalette(c("blue3", "white","red2"))(200)

# Labels-
legend_title<-"Importance"

imp.full <- ggplot(dat.taxa %>% dplyr::filter(resp.var%in%c("total.abundance", "species.richness")), 
                   aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(-1, 1))+
  scale_y_discrete(labels=c("Species richness","Total abundance"))+
  labs(x = NULL, y = NULL, title = "Whole assemblage") +
  theme_classic()+
  geom_text(aes(label=label)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.x = element_blank(),
        plot.title = element_text(hjust = -0.45, vjust = -15)) # Looks crap here but title comes back in exported version
imp.full

#save output - changed dimensions for larger text in report
save_plot(paste0("figures/fish/", name, "_importance-scores.png"), 
          gg.importance,base_height = 4,base_width = 6.275)