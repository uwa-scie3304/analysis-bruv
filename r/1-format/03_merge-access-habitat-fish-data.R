###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise fish data AND add in distance to boat ramp
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

# To add in extra access points-----
  # make a .shp in QGIS 
  # or
# - make a data frame manually
# - make it a simple features
# see example

count <- readRDS(paste0("data/staging/", name, "_tidy-count.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  dplyr::mutate(distance_from_access = unlist(st_nn(., access, returnDist = T, progress = F)[2]),
                opcode = as.character(opcode)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(habitat) %>%
  glimpse()


length <- readRDS(paste0("data/staging/", name, "_tidy-length.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  dplyr::mutate(distance_from_access = unlist(st_nn(., access, returnDist = T, progress = F)[2]),
                opcode = as.character(opcode)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(habitat) %>%
  glimpse()

saveRDS(length, paste0('data/tidy/', 
                       name,'_tidy-length.rds'))

