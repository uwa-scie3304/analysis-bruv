###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise fish data
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
library(sf)
library(here)
library(leaflet)

# Set the study name (e.g. campaignID) - this way your code is reuseable for a different stereo-video campaign
name <- "2024_Albany_stereo-BRUVs"

# Load the count data
count <- read.csv(paste0("data/raw/", name, "_count.csv")) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  glimpse()

# Make cumulative MaxN - to visualise the most abundant species
top_species <- count %>%
  group_by(scientific) %>%
  summarise(sum_maxn = sum(count)) %>%
  arrange(sum_maxn) %>%
  glimpse()

# Plot the most abundant species using ggplot
ggplot(data = top_species, aes(x = reorder(scientific, sum_maxn), y = sum_maxn)) +
  geom_bar(stat = "identity", colour = "black", fill = "lightgrey", position = position_dodge()) +
  coord_flip() +
  labs(x = expression(Overall~abundance~(Sigma~MaxN)), y = "Species") +
  theme_classic()

# Plot individual species spatially, with a bubble plot where size of the bubble = MaxN
# Select your species - e.g. King George Whiting Sillaginodes punctatus
species_name <- 'Sillaginodes punctatus'

# Split the data into samples with fish, and samples with no fish
overzero <-  count %>% 
  filter(scientific %in% species_name & count > 0) 

# Zeroes are important!
equalzero <- count %>% 
  filter(scientific %in% species_name & count == 0)

# Create a leaflet basemap - this is an interactive plot with a choice of multiple basemaps
base_plot <- leaflet(data = count) %>%                     
  addTiles() %>%                                                    
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), options = layersControlOptions(collapsed = FALSE))

# Visualise the bubble plots - white bubble = deployment with none of the species
if (nrow(overzero)) {                                               
  bubble_plot <- base_plot %>%
    addCircleMarkers(data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = ~ count / 2, fillOpacity = 0.5, stroke = FALSE, label = ~ as.character(opcode))}

if (nrow(equalzero)) {                                            
  bubble_plot <- bubble_plot %>%
    addCircleMarkers(data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = 5, fillOpacity = 0.5, color = "white", stroke = FALSE, label = ~ as.character(opcode))}
bubble_plot

# Format the data to a format suitable to use in modelling scripts
# This needs to be long format data, with one line per deployment/opcode
# In this example we are just interested in the abundance of King George Whiting, so the data is filtered to this species, and a character column added (this will make sense when you run the modelling)
kgw <- count %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus") %>%
  dplyr::mutate(response = "Abundance of KGW") %>%
  glimpse()

# Save the count data
saveRDS(kgw, file = paste0("data/staging/", name, "_tidy-count.rds"))

# Load size of maturity data - this is loaded by the CheckEM package 
maturity_mean <- maturity %>%
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(Lm = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

# Load length annotation data and join with size of maturity dataset
length <- read.csv(paste0("data/raw/", name, "_length.csv")) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  left_join(maturity_mean) %>%
  glimpse()

# Create a 'metadata' file for lengths - this is a list of all the unique deployments/opcodes, so that the 0s can be added back in
metadata_length <- length %>%
  distinct(campaignid, opcode, latitude_dd, longitude_dd, depth_m, status)

# Visualise the length distributions
# Manually added on minimum legal size and length of maturity (Lm)
ggplot(filter(length, scientific %in% "Sillaginodes punctatus"), aes(length_mm)) +
  geom_density(fill = "grey40", alpha = 0.3) +
  labs(title = "King George Whiting length distribution") +
  geom_vline(xintercept = 280, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 410, colour = "blue", linetype = "dashed") +
  annotate(geom = "text", x = c(280, 410), y = 0.01, label = c("Minimum legal size", "Lm")) +
  theme_classic()

# Create a dataframe for the abundance of greater than size of maturity King George Whiting
mature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Mature KGW") %>%
  glimpse()

# Create a dataframe for the abundance of smaller than size of maturity King George Whiting
immature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Immature KGW") %>%
  glimpse()

# Create a dataframe for the abundance of greater than legal size King George Whiting
legal_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > 280) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Legal KGW") %>%
  glimpse()

# Create a dataframe for the abundance of smaller than legal size King George Whiting
sublegal_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < 280) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Sublegal KGW") %>%
  glimpse()

# Join the datasets
tidy_length <- bind_rows(mature_kgw, immature_kgw, legal_kgw, sublegal_kgw) %>%
  glimpse()

# Set the response variable
response_var <- "Legal KGW"

# Split the data into samples with fish, and samples with no fish
overzero <-  tidy_length %>% 
  dplyr::filter(response %in% response_var & number > 0) 

# Zeroes are important!
equalzero <- tidy_length %>% 
  filter(response %in% response_var & number == 0)

# Remove old plot files, otherwise the plots will keep adding to old versions
rm(base_plot, bubble_plot)

# Create a leaflet basemap - this is an interactive plot with a choice of multiple basemaps
base_plot <- leaflet(data = tidy_length) %>%                     
  addTiles() %>%                                                    
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), options = layersControlOptions(collapsed = FALSE))

# Visualise the bubble plots - white bubble = deployment with 0 of the response
if (nrow(overzero)) {                                               
  bubble_plot <- base_plot %>%
    addCircleMarkers(data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = ~ number / 2, fillOpacity = 0.5, stroke = FALSE, label = ~ as.character(opcode))}

if (nrow(equalzero)) {                                            
  bubble_plot <- bubble_plot %>%
    addCircleMarkers(data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = 5, fillOpacity = 0.5, color = "white", stroke = FALSE, label = ~ as.character(opcode))}
bubble_plot

# Save the length data
saveRDS(tidy_length, file = paste0("data/staging/", name, "_tidy-length.rds"))