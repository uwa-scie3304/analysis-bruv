library(ggplot2)
library(sf)
library(tidyverse)

points <- read.csv("data/raw/fish/pre-checkem/2024_Albany_stereo-BRUVs_Metadata.csv") %>%
  dplyr::select(opcode, latitude_dd, longitude_dd, depth_m) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  glimpse()
plot(points)

aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

access <- st_read("data/spatial/shapefiles/National Boat Ramps Reduced.shp") %>%
  dplyr::filter(State %in% "WA") %>%
  st_transform(4326)
plot(access)

ggplot() +
  geom_sf(data = aus) +
  geom_sf(data = points, aes(colour = depth_m)) +
  geom_sf(data = access, colour = "red") +
  coord_sf(xlim = c(117.839435, 117.949606), 
           ylim = c(-35.007973, -35.091606),
           crs = 4326) +
  theme_classic()
