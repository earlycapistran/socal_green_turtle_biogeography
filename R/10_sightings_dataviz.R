data <- readRDS("./data/processed/full_data_green.rds")
library(tidyverse)

# Select variables of interest
plot_data <- data %>% 
  select(num_turtles_numeric, lat_group) %>% 
  # Remove datapoints with unreported locations
  na.omit()

# Plot
ggplot(plot_data, aes(x = lat_group, 
                      y = num_turtles_numeric, 
                      color = lat_group)) +
  # 1. Boxplot (behind points)
  geom_boxplot(
    width = 0.25, 
    alpha = 0.5, 
    outlier.shape = 4,
    outlier.size = 4,  # Slightly smaller for balance
    color = "grey20",
    show.legend = FALSE  # Hide duplicate legend
  ) +
  
  # 2. Jittered points (on top)
  geom_jitter(
    size = 3,
    width = 0.05,
    alpha = 0.6,
    stroke = 0.5  # For better point definition
  ) +
  
  # Color scheme
  scale_color_manual(
    name = NULL,
    values = c("#a86826", "#3d6721"),
    labels = c("Southern California Bight", 
               "South of Southern California Bight")
  ) +
  
  # Labels
  labs(
    x = NULL,  # No x-axis label
    y = "Number of Turtles per Report (by location)"
  ) +

  
  # Theme adjustments
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Lato"),  # Apply Lato to all text
    axis.text.x = element_blank(),  # Remove x-axis text completely
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    plot.title = element_text(face = "bold"),
    plot.margin = unit(c(5,5,5,5), "pt")
  ) 