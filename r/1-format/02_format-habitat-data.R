###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise habitat data
# Author:  Claude Spencer
# Date:    May 2024
## 

# Clear objects from your environment
rm(list = ls())

# Set your working directory to the project's directory
setwd(here::here())

# Load libraries and install the CheckEM package (only need to install once)
library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # Use this to install the CheckEM package if you have not already done so
library(CheckEM)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(here)
library(tidyverse)

# Set the study name (e.g. campaignID) - this way your code is reuseable for a different stereo-video campaign
name <- "2023-03_SwC_stereo-BRUVs"

# Load the metadata
metadata <- read_metadata("data/raw/") %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth, successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()

# Save the tidy metadata - remove unnecessary columns
saveRDS(metadata, file = here::here(paste0("r-workflows/data/tidy/", name, "_Metadata.rds")))

# Read in the habitat annotation data
points <- read_TM("data/raw/", sample = "opcode")

habitat <- points %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, starts_with("level"), scientific) %>%
  glimpse()

relief <- points %>%
  dplyr::filter(relief_annotated %in% "yes") %>%
  dplyr::select(campaignid, sample, starts_with("level"), scientific) %>%
  glimpse()

num.points <- 20

wrong.points.habitat <- habitat %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ num.points * 2, successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

wrong.points.relief <- relief %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ num.points * 2, successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat, metadata, by = c("campaignid", "sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat, by = c("campaignid", "sample")) %>%
  glimpse()

tidy.habitat <- habitat %>%
  dplyr::mutate(number = 1) %>%                                     
  left_join(catami) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "number") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

saveRDS(tidy.habitat, file = here::here(paste0("r-workflows/data/staging/", name, "_habitat.rds")))

tidy.relief <- relief %>%
  dplyr::select(campaignid, sample, starts_with("level"), scientific) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%              
  dplyr::mutate(number = 1) %>% 
  left_join(catami) %>%
  group_by(campaignid, sample, across(starts_with("level"))) %>% 
  dplyr::tally(number, name = "number") %>%
  ungroup() %>%                                                     
  glimpse()   

saveRDS(tidy.relief, file = here::here(paste0("r-workflows/data/staging/", name, "_relief.rds")))