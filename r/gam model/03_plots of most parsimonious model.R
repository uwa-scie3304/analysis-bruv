library(CheckEM)
library(tidyverse)
library(mgcv)
library(devtools)
library(FSSgam)
library(here)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(terra)
library(sf)
library(patchwork)

name <- '2024_Albany_stereo-BRUVs'

dat <- readRDS(here::here(paste0('data/tidy/', 
                                 name,'_tidy-length.rds'))) %>%
  dplyr::filter(!is.na(Macroalgae),
                !is.na(mean_relief)) %>%
  glimpse()

dat_immature <- dat %>% 
  dplyr::filter(response %in% "Immature KGW") %>%
  glimpse()

mod <- gam(number ~ s(Unconsolidated, k = 3, bs = 'cr'), family = tw(), data = dat_immature)
summary(mod)

testdata <- expand.grid(Unconsolidated = seq(min(dat$Unconsolidated), max(dat$Unconsolidated),length.out = 20)) %>%
  distinct() %>%
  glimpse()

fits <- predict.gam(mod, newdata = testdata, type = 'response', se.fit = T)

predicts_immature_unconsolidated <- testdata %>%
  data.frame(fits) %>%
  group_by(Unconsolidated) %>% # Only change here
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

ggplot() +
  geom_point(data = dat_immature, aes(x = Unconsolidated, y = number),  alpha = 0.5, size = 1, show.legend = F) +
  geom_line(data = predicts_immature_unconsolidated, aes(x = Unconsolidated, y = number), alpha = 0.5) +
  geom_line(data = predicts_immature_unconsolidated, aes(x = Unconsolidated, y = number - se.fit), linetype = "dashed", alpha = 0.5) +
  geom_line(data = predicts_immature_unconsolidated, aes(x = Unconsolidated, y = number + se.fit), linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  labs(x = "Sand", y = "", title = "Immature KGW") +
  theme(plot.title = element_text(hjust = 0))
