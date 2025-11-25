data <- readRDS("./data/processed/full_data_green.rds")
library(tidyverse)   ## data science package collection (incl. the ggplot2 package)
library(systemfonts) ## use custom fonts (need to be installed on your OS)  
library(scico)       ## scico color palettes(http://www.fabiocrameri.ch/colourmaps.php) in R 
library(ggtext)      ## add improved text rendering to ggplot2
library(ggforce)     ## add missing functionality to ggplot2
library(ggdist)      ## add uncertainity visualizations to ggplot2
library(patchwork)   ## combine outputs from ggplot2
library(gghalves)    ## make half plots

# Select variables of interest
plot_data <- data %>% 
  select(num_turtles_numeric, lat_group) %>% 
  na.omit() # Remove datapoints with unreported locations

ggplot(plot_data, aes(x = lat_group, 
                      y = num_turtles_numeric,
                      color = lat_group)) + 
  ## add half-violin from {ggdist} package
  ggdist::geom_dots(
    ## orientation to the left
    side = "right", 
    binwidth = 1,
    ## move geom to the left
    justification = 1.12) +
  geom_boxplot(
    width = .15, 
    outlier.shape = 4,
    na.rm = TRUE
    ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +
  coord_cartesian(ylim = c(0, 275)) +
  theme_classic()
  # ggdist::stat_dots(
  #   ## orientation to the left
  #   side = "left", 
  #   ## move geom to the left
  #   justification = 1.12, 
  #   ## adjust grouping (binning) of observations 
  #   binwidth = .25
  # ) + 
  ## remove white space on the sides

