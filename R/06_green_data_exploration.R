# Exploratory data visualization

# Load data and libraries
data <- readRDS("./data/processed/full_data_green.rds")
library("ggplot2")

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
         state,
         county_muni) %>% 
  filter(!is.na(num_turtles_numeric)) %>% 
  filter(location != "not_reported") %>% 
  filter(location != "na") %>% 
  filter(year < 1940) # Limit to commercial fishing years

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

# Spatial
space <- ggplot(green, aes(x=latitude, 
                          y=longitude, 
                          size=num_turtles_numeric, 
                          color = state)) +
  geom_point()
space

# Histograms & boxplots
hist_cali <- ggplot(data = green_cali, 
                    aes(x = num_turtles_numeric)) +
  geom_histogram()
hist_cali

box_cali <- ggplot(data = green_cali, 
                    aes(x = num_turtles_numeric)) +
  geom_boxplot()

hist_cali <- ggplot(data = green_cali, 
                    aes(x = num_turtles_numeric)) +
  geom_histogram()
hist_cali

hist_baja <- ggplot(data = green_baja, 
                   aes(x = num_turtles_numeric)) +
  geom_histogram()
hist_baja

box_baja <- ggplot(data = green_baja, 
                    aes(x = num_turtles_numeric)) +
  geom_boxplot()
box_baja
