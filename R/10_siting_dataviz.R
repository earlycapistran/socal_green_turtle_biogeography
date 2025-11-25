data <- readRDS("./data/processed/full_data_green.rds")
library(tidyverse)   ## data science package collection (incl. the ggplot2 package)
library(systemfonts) ## use custom fonts (need to be installed on your OS)  
library(scico)       ## scico color palettes(http://www.fabiocrameri.ch/colourmaps.php) in R 
library(ggtext)      ## add improved text rendering to ggplot2
library(ggforce)     ## add missing functionality to ggplot2
library(ggdist)      ## add uncertainity visualizations to ggplot2
library(patchwork)   ## combine outputs from ggplot2

plot_data <- data %>% 
  select(num_turtles_numeric, lat_group) %>% 
  na.omit()

ggplot(plot_data, aes(x = lat_group, 
                      y = num_turtles_numeric,
                      color = lat_group)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(## orientation to the left
    side = "left", 
    ## move geom to the left
    justification = 1.12, 

    na.rm = TRUE) +
  geom_boxplot(
    width = .15, 
    outlier.shape = 4,
    na.rm = TRUE
    ) +
  coord_cartesian(ylim = c(0, 150)) 
  # ggdist::stat_dots(
  #   ## orientation to the left
  #   side = "left", 
  #   ## move geom to the left
  #   justification = 1.12, 
  #   ## adjust grouping (binning) of observations 
  #   binwidth = .25
  # ) + 
  ## remove white space on the sides

