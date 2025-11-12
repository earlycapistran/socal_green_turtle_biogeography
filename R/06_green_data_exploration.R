# Exploratory data visualization

# Load data and libraries
data <- readRDS("./data/processed/full_data_green.rds")
library("ggplot2")
library("sf")
library("mapview")
library("dplyr")

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
         county_muni) %>% 
  filter(!is.na(num_turtles_numeric)) %>% 
  filter(num_turtles_numeric != 0) %>% 
  filter(location != "not_reported") %>% 
  filter(location != "na") %>% 
  filter(county_muni != "na") %>% 
  filter(year < 1935) # Limit to commercial fishing years

# Datasets by location

green_cali <- green %>% 
  filter(state == "california")

green_baja <- green %>% 
  filter(state == "baja_california_sur" | state == "baja_california")

# Time series
time <- ggplot(green, aes(x=year, 
                         y=num_turtles_numeric, 
                         size=num_turtles_numeric, 
                         color = state)) +
  geom_point()
time


time <- ggplot(green_cali, aes(x=year, 
                          y=num_turtles_numeric, 
                          size=num_turtles_numeric, 
                          color = state)) +
  geom_point()
time


# Spatial
space <- ggplot(green, aes(x=latitude, 
                          y=longitude, 
                          size=num_turtles_numeric, 
                          color = state)) +
  geom_point(alpha = .5)
space


# Histograms & boxplots
hist_cali <- ggplot(data = green_cali, 
                    aes(x = num_turtles_numeric)) +
  geom_histogram()
hist_cali

box_cali <- ggplot(data = green_cali, 
                    aes(y = num_turtles_numeric,
                        group = decade)) +
  geom_boxplot()
box_cali


hist_cali <- ggplot(data = green_cali, 
                    aes(x = num_turtles_numeric)) +
  geom_histogram()
hist_cali

hist_baja <- ggplot(data = green_baja, 
                   aes(x = num_turtles_numeric)) +
  geom_histogram()
hist_baja

box_baja <- ggplot(data = green_baja, 
                    aes(y = num_turtles_numeric,
                        group = decade)) +
  geom_boxplot()
box_baja

# What happens if we get mean annual values per state?
green_means <- green %>% 
  group_by(decade, state) %>% 
  summarise(mean_annual = mean(num_turtles_numeric))

means <- ggplot(green, aes(x=decade, 
                          y=num_turtles_numeric, 
                          group = state,
                          color = state)) +
  geom_bar(stat="identity", position=position_dodge())
means

# More boxplots
all_box <- ggplot(green, aes(y = num_turtles_numeric,
                             group = state,
                             fill = state)) +
  geom_boxplot()
all_box

kruskal.test(num_turtles_numeric ~ state, data = green)

# Map
mapview(green, 
        xcol = "longitude", 
        ycol = "latitude", 
        crs = 4269, 
        grid = FALSE,
        cex = "num_turtles_numeric")

