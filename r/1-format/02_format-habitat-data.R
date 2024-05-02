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

name <- "test-data/post-checkem/2023-03_SwC_stereo-BRUVs"

# Load and format habitat data
habitat <- read.csv(paste0("data/raw/", name, "_benthos.csv")) %>%
  # dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::mutate(broad_habitat = case_when(level_2 %in% c("Sponges", "Seagrasses", "Macroalgae", 
                                                         "Sessile invertebrates", "Cnidaria", "Bryozoa") ~ level_2,
                                          level_2 %in% "Substrate" ~ level_3)) %>%
  dplyr::filter(!is.na(level_2)) %>%
  group_by(campaignid, sample, broad_habitat) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  pivot_wider(names_from = broad_habitat, values_from = number) %>%
  clean_names() %>%
  glimpse()

# Load and format the relief data
relief <- read.csv(paste0("data/raw/", name, "_relief.csv")) %>%
  uncount(number) %>%
  dplyr::group_by(campaignid, sample) %>%
  summarise(mean_relief = mean(level_5, na.rm = T),
            sd_relief = sd(level_5, na.rm = T)) %>%
  ungroup() %>%
  glimpse()

# Join the two datasets
tidy_habitat <- habitat %>%
  left_join(relief) %>%
  glimpse()

# Save the final tidy dataset
saveRDS(tidy_habitat, file = paste0("data/staging/", name, "_tidy-habitat.rds"))
