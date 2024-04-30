
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
count <- read.csv(paste0("data/staging/", name, "_count.csv")) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  glimpse()

# Visualise data ----
top_species <- count %>%
  group_by(scientific) %>%
  summarise(sum_maxn = sum(count)) %>%
  arrange(sum_maxn) %>%
  glimpse()

ggplot(data = top_species, aes(x = reorder(scientific, sum_maxn), y = sum_maxn)) +
  geom_bar(stat = "identity", colour = "black", fill = "lightgrey", position=position_dodge()) +
  coord_flip() +
  labs(x = expression(Overall~abundance~(Sigma~MaxN)), y = "Species") +
  theme_classic()

species_name <- 'Sillaginodes punctatus'

overzero <-  count %>% 
  filter(scientific %in% species_name & count > 0) 

equalzero <- count %>% 
  filter(scientific %in% species_name & count == 0)

base_plot <- leaflet(data = count) %>%                     
  addTiles() %>%                                                    
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), options = layersControlOptions(collapsed = FALSE))

if (nrow(overzero)) {                                               
  bubble_plot <- base_plot %>%
    addCircleMarkers(data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = ~ count / 2, fillOpacity = 0.5, stroke = FALSE, label = ~ as.character(opcode))}

if (nrow(equalzero)) {                                            
  bubble_plot <- bubble_plot %>%
    addCircleMarkers(data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = 5, fillOpacity = 0.5, color = "white", stroke = FALSE, label = ~ as.character(opcode))}
bubble_plot

kgw <- count %>%
  left_join(maturity_mean) %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus") %>%
  dplyr::mutate(response = "Abundance of KGW") %>%
  glimpse()

maturity_mean <- maturity %>%
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(Lm = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

length <- read.csv(paste0("data/staging/", name, "_length.csv")) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  left_join(maturity_mean) %>%
  # uncount(number) %>%
  glimpse()

metadata_length <- length %>%
  distinct(campaignid, opcode, latitude_dd, longitude_dd, depth_m, status)

mature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Mature KGW") %>%
  glimpse()

immature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Immature KGW") %>%
  glimpse()

tidy_length <- bind_rows(mature_kgw, immature_kgw) %>%
  glimpse()
