data <- readRDS("./data/processed/full_data_green.rds")
library(tidyverse)

# Prepare data ------------------------------------------
plot_data <- data %>% 
  select(num_turtles_numeric, lat_group) %>% 
  na.omit()

# Visualization 
ggplot(plot_data, aes(x = num_turtles_numeric, 
                      y = lat_group, 
                      fill = lat_group)) +
  # Make dotplot
  geom_dotplot(binaxis = "x",           
               stackdir = "up",
               dotsize = 3.5,
               binwidth = 1,
               stackratio = 0.75,
               color = NA,
               alpha = 0.9) +
  # Make boxplot
  geom_boxplot(width = 0.3,             
               alpha = 0.3, 
               outlier.shape = NA,
               color = "grey20",
               position = position_nudge(y = -0.2)) +
  # Color scheme - use colorblind-friendly palette
  scale_fill_manual(values = c("north" = "#f0a226", "south" = "#28a685")) +
  scale_y_discrete(labels = c("north" = "Southern California Bight\n (>32.5°N)", 
                              "south" = "Baja California Peninsula\n(<32.5°N)")
                   ) + 
  # Facet by group
  facet_grid(lat_group ~ ., 
             scales = "free_y",
             space = "free_y") +
  labs(y = "Region", x = "Number of Turtles per Report (by location)") +
  coord_cartesian(xlim = c(0, NA)) +
  theme_classic(base_size = 16) +
  theme(
    # Font
    element_text(family = "Lato"),
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 22),
    axis.text = element_text(size = 22),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    strip.background = element_blank(),  # removes facet background
    strip.text = element_blank()         # removes facet text
  )

