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

# Plot
ggplot(plot_data, aes(x = lat_group, 
                      y = num_turtles_numeric,
                      color = lat_group)) + 
  ## add half-violin from {ggdist} package
  ggdist::geom_dots(
    ## orientation to the left
    side = "left", 
    justification = 1.15) +
  geom_boxplot(
    width = .15, 
    outlier.shape = 4,
    na.rm = TRUE
    ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "r", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_classic()

### Make it fancy! -----------------

## calculate summary stats
df_turtle_stats <- 
  plot_data %>% 
  filter(!is.na(num_turtles_numeric)) %>% 
  group_by(lat_group) %>% 
  mutate(
    n = n(),
    median = median(num_turtles_numeric),
    max = max(num_turtles_numeric)
  ) %>% 
  ungroup() %>% 
  mutate(lat_group_num = as.numeric(fct_rev(lat_group)))  %>% 
  mutate(lat_group = fct_rev(lat_group))  

p2 <- 
  ggplot(df_turtle_stats, 
         aes(x = num_turtles_numeric,
             y = lat_group,
             fill = lat_group,
             color = lat_group)) +
  # geom_point(
  #   aes(x = num_turtles_numeric - .15),
  #   size = 2,
  #   alpha = .3
  # ) +
  stat_interval(aes(color_ramp = after_stat(level)),
                linewidth = 3  # Thicker lines for visibility
                ) +
  ggdist::geom_dots(
    position = position_nudge(y = 0.1)) +
  
  geom_jitter(
    aes(x = num_turtles_numeric - 0.15),
    position = position_nudge(y = -0.1),  # Align with dots
    size = 1,
    alpha = 0.3
  ) +

  # stat_pointinterval(color = "black") +
  scale_color_manual(values = c("#3d6721", "#a86826"), guide = "none") +
  scale_fill_manual(values = c("#3d6721", "#a86826"), guide = "none") +
  theme_classic()
p2

