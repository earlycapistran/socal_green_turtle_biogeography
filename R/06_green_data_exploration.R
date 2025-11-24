# Exploratory data visualization

# Load data and libraries
data <- readRDS("./data/processed/full_data_green.rds")
library("ggplot2")
library("sf")
library("dplyr")
library("ggbeeswarm")
library("ridgeline")

# Select columns of interest and filter out rows w/o num_turtles
green <- data %>% 
  select(num_turtles_numeric, 
         latitude, 
         longitude,
         location,
         location_certainty,
         data_quality_index,
         data_quality_cat,
         year,
         decade,
         state,
         county_muni,
         lat_group) %>% 
  filter(!is.na(num_turtles_numeric)) %>% 
  filter(num_turtles_numeric != 0) %>% 
  filter(location != "not_reported") %>% 
  filter(location != "na") %>% 
  filter(county_muni != "na") %>% 
  filter(year < 1935) # Limit to commercial fishing years

# Datasets by latitude group (inside and outside SoCal Bight)

green_scb <- green %>% 
  filter(lat_group == "socal_bight")

green_non_scb <- green %>% 
  filter(lat_group == "non_socal_bight")

# Time series by latitude group
time <- ggplot(green, aes(x=year, 
                         y=num_turtles_numeric, 
                         size=num_turtles_numeric, 
                         color = lat_group)) +
  geom_point()
time

# Spatial
space <- ggplot(green, aes(x=latitude, 
                          y=longitude, 
                          size=num_turtles_numeric, 
                          color = lat_group)) +
  geom_point(alpha = .5)
space

# Boxplot north and south
box <- ggplot(green, aes(y = num_turtles_numeric,
                         color = lat_group)) +
  geom_boxplot() +
  theme_classic() 
box

# Beeswarm plot
ggplot(green, aes(x = lat_group, 
                  y = num_turtles_numeric,
                  color = lat_group)) +
  geom_beeswarm(cex = 1,
                priority = "density", 
                alpha = 0.7,
                size = 2) +
  stat_summary(fun.y = median, 
               fun.ymin = median, 
               fun.ymax = median, 
               geom = "crossbar", 
               color = "darkgrey",
               width = 0.5) +
  theme_classic()

# Jitter strip plot
ggplot(green, aes(x = lat_group, 
                  y = num_turtles_numeric,
                  color = lat_group)
       ) +
  geom_jitter(width = 0.1,
              alpha = 0.5,
             size = 3) + 
  stat_summary(fun.y = median, 
               fun.ymin = median, 
               fun.ymax = median, 
               geom = "crossbar", 
               width = 0.5) +
  theme_classic()         # Use a clean theme

# Ridgeline plot
ridgeline(green$num_turtles_numeric, green$lat_group,
  palette = hcl.colors(6, palette = "viridis",
                       alpha = 0.85),
  mode = TRUE,
  border = hcl.colors(6, palette = "viridis",
                      alpha = 0.85))

# Histogram
ggplot(green, aes(x = num_turtles_numeric,
                  fill = factor(lat_group)
                  )
       ) +
  geom_dotplot(binwidth = 15,
               method = "histodot") +
  scale_y_continuous(NULL, breaks = NULL) +
  facet_wrap(~lat_group)
  
  