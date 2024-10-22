# Exploratory data visualization

# Load data and libraries
data <- readRDS("./data/processed/full_data_green.rds")
library("ggplot2")
library("sf")
library("mapview")
library("dplyr")
library("psych")

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
         country) %>% 
  filter(!is.na(num_turtles_numeric)) %>% 
  filter(num_turtles_numeric != 0) %>% 
  filter(location != "not_reported") %>% 
  filter(location != "na") %>% 
  filter(county_muni != "na") %>% 
  filter(year < 1940) # Limit to commercial fishing years

# Datasets by location split by southern limit of CCLME 
# (Punta Eugenia, BCS)
green_lat <- green %>% 
  mutate(lat_group = as.factor(ifelse(latitude <28, "south", "north")))

north <- green_lat %>% 
  filter(lat_group == "north")

south <- green_lat %>% 
  filter(lat_group == "south")

# Get descriptive stats to look at differences by latitude
describeBy(green_lat$num_turtles_numeric, 
           green_lat$lat_group,
           IQR=FALSE, skew = FALSE)

# Boxplot
box <- ggplot(data = green_lat, 
              aes(x = lat_group,
              y = num_turtles_numeric,
              fill = lat_group)) +
  geom_boxplot()
box  

# Test normality
shapiro.test(north$num_turtles_numeric)
shapiro.test(south$num_turtles_numeric)

# Kruskall test
kruskal.test(num_turtles_numeric ~ lat_group, 
             data = green_lat)
