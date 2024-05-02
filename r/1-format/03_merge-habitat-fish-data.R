###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise fish data
# Author:  Claude Spencer
# Date:    May 2024
## 

# Clear objects from your environment
rm(list = ls())

# Load libraries
library(sf)
library(tidyverse)
library(nngeo)

# Set your working directory to the project's directory
setwd(here::here())

name <- "2024_Albany_stereo-BRUVs"

habitat <- readRDS(paste0("data/staging/", name, "_tidy-habitat.rds")) %>%
  glimpse()

# Create distance to boat ramp, and join with habitat data
access <- st_read("data/spatial/shapefiles/National Boat Ramps Reduced.shp") %>%
  dplyr::filter(State %in% "WA") %>%
  st_transform(4326)

count <- readRDS(paste0("data/staging/", name, "_tidy-count.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  dplyr::mutate(distance_from_access = unlist(st_nn(., access, returnDist = T, progress = F)[2])) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(habitat) %>%
  glimpse()

length <- readRDS(paste0("data/staging/", name, "_tidy-length.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  dplyr::mutate(distance_from_access = unlist(st_nn(., access, returnDist = T, progress = F)[2])) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(habitat) %>%
  glimpse()

saveRDS(length, )



