###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise habitat data
# Author:  Claude Spencer
# Date:    May 2024
## 

# Clear objects from your environment
rm(list = ls())

# Load libraries and install the CheckEM package (only need to install once)
library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # Use this to install the CheckEM package if you have not already done so
library(CheckEM)
library(tidyverse)

# Set your working directory to the project's directory
setwd(here::here())

name <- "2024_Albany_stereo-BRUVs"

# Load and format habitat data
habitat <- read.delim("data/raw/habitat/Annotation_1_Dot Point Measurements.txt", 
                      skip = 4, stringsAsFactors = F, colClasses = "character") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, broad) %>%
  dplyr::filter(!broad %in% c("Unknown", NA, "")) %>%
  dplyr::mutate(number = 1) %>%
  group_by(campaignid, opcode, broad) %>%
  summarise(number = sum(number)) %>%
  pivot_wider(names_from = "broad", values_from = "number", values_fill = 0) %>%
  ungroup() %>%
  glimpse()

# Load and format the relief data
relief <- read.delim("data/raw/habitat/Relief_Dot Point Measurements.txt", 
                     skip = 4, stringsAsFactors = F, colClasses = "character") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA, "")) %>%
  dplyr::mutate(level_5 = as.numeric(level_5)) %>%
  group_by(campaignid, opcode) %>%
  summarise(mean_relief = mean(level_5),
            sd_relief = sd(level_5)) %>%
  ungroup() %>%
  glimpse()

# Join the two datasets
tidy_habitat <- habitat %>%
  left_join(relief) %>%
  glimpse()

# Save the final tidy dataset
saveRDS(tidy_habitat, file = paste0("data/staging/", name, "_tidy-habitat.rds"))
