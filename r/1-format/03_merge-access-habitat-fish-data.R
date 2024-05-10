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

# Set the study name
name <- "2024_Albany_stereo-BRUVs"

# Load the habitat data----
habitat <- readRDS(paste0("data/staging/", name, "_tidy-habitat.rds")) %>%
  glimpse()



# Create distance to boat ramp, and join with habitat data----
access <- st_read("data/spatial/shapefiles/National Boat Ramps Reduced.shp") %>%
  dplyr::filter(State %in% "WA") %>%
  st_transform(4326)

# To add in extra access points-----
  # make a .shp in QGIS or,

# Manually make a simple features dataframe - see below

# access <- data.frame(name = c("accesspoint1"), 
#                      lat = c(-31.9789), 
#                      lon = c(115.8181)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# plot(access)

# Join habitat and count data, and create distance from access point (m)----
count <- readRDS(paste0("data/staging/", name, "_tidy-count.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  dplyr::mutate(distance_from_access = unlist(st_nn(., access, returnDist = T, progress = F)[2]),
                opcode = as.character(opcode)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(habitat) %>%
  glimpse()

# Join habitat and length data, and create distance from access point (m)
length <- readRDS(paste0("data/staging/", name, "_tidy-length.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  dplyr::mutate(distance_from_access = unlist(st_nn(., access, returnDist = T, progress = F)[2]),
                opcode = as.character(opcode)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(habitat) %>%
  glimpse()

# Save the length data----
# - count data can be saved in the same way if needed
saveRDS(length, paste0('data/tidy/', 
                       name,'_tidy-length.rds'))

