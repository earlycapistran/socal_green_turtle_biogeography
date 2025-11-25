data <- readRDS("./data/processed/full_data_green.rds")
library(tidyverse)

# Select variables of interest
plot_data <- data %>% 
  select(num_turtles_numeric, lat_group) %>% 
  # Remove datapoints with unreported locations
  na.omit()

ggplot(plot_data, aes(x = lat_group, 
                            y = num_turtles_numeric, 
                            color = lat_group)) +
  # Raw data
  geom_jitter(
    size = 3,
    width = 0.1,
    cex = 1,
    alpha = 0.6
  ) +
  geom_boxplot(
    width = 0.15, 
    alpha = 0.5, 
    outlier.shape = 4,
    outlier.size = 5) +
  scale_color_manual(values = c("#a86826", "#3d6721")) +
  theme_classic()

