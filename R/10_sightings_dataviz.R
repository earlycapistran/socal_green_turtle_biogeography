data <- readRDS("./data/processed/full_data_green.rds")
library(tidyverse)

plot_data <- data %>% 
  filter(data_quality_cat == "high") %>%  # DQI filter: retain only high-quality records
  select(num_turtles_numeric, lat_group) %>% 
  na.omit()

p <- ggplot(plot_data, aes(x = num_turtles_numeric, y = lat_group, fill = lat_group)) +
  geom_dotplot(binaxis = "x",           # CHANGED from "y"
               stackdir = "up",
               dotsize = 2.25,
               binwidth = 1,
               stackratio = 0.75,
               color = NA,
               alpha = 0.9) +
  geom_boxplot(width = 0.15,             # Now controls height
               alpha = 0.3, 
               outlier.shape = NA,
               color = "grey20",
               position = position_nudge(y = -0.2)) +
  # Color scheme - use colorblind-friendly palette
  scale_fill_manual(values = c("north" = "#f0a226", "south" = "#28a685")) +
  scale_y_discrete(labels = c("north" = "Southern California Bight\n (>32.5°N)", 
                              "south" = "Baja California Peninsula\n(<32.5°N)")
  ) + 
  facet_grid(lat_group ~ ., 
             scales = "free_y",
             space = "free_y") +
  labs(y = "Region", x = "Number of Turtles per Report (by location)") +
  coord_cartesian(xlim = c(0, NA)) +
  theme_classic(base_size = 14) +
  theme(
    # Font
    text = element_text(family = "Lato"),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    strip.background = element_blank(),  # removes facet background
    strip.text = element_blank()         # removes facet text
  )

ggsave("./plots/sightings_dotplot.png", plot = p, width = 8, height = 5, 
       dpi = 300, device = ragg::agg_png)