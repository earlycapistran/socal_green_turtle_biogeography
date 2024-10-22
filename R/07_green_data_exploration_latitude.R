# Exploratory data visualization

# Load data and libraries
data <- readRDS("./data/processed/green_quant.rds")
library("ggplot2")
library("sf")
library("mapview")
library("dplyr")
library("psych")
library("ggmap")
library("osmdata")

# Select columns of interest and filter out rows w/o num_turtles
green <- data %>% 
  select(num_turtles_numeric, 
         latitude, 
         longitude,
         location,
         year,
         decade,
         state,
         county_muni,
         country) 

green_means <- green %>% 
  group_by(location, year) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup()


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

# Map
mapview(green, 
        xcol = "longitude", 
        ycol = "latitude", 
        crs = 4269, 
        grid = FALSE,
        cex = "num_turtles_numeric")

# ggmap

cclme <- c(-125, 20, -115, 35)

cclme_map <- get_stadiamap(c(left = -115, 
                             bottom = 2208, 
                             right = -125, 
                             top = 35), 
                           maptype = "stamen_terrain_background", 
                           crop=FALSE)
ggmap(cclme_map)
