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
         lat_group,
         location,
         year,
         decade,
         state,
         county_muni,
         country) 

green_means <- green %>% 
  group_by(location, year, lat_group) %>% 
  summarise(across(where(is.numeric), 
                   ~ mean(.x, na.rm = TRUE)))

north <- green_means %>% 
  filter(lat_group == "north")

south <- green_means %>% 
  filter(lat_group == "south")

# Get descriptive stats to look at differences by latitude
describeBy(green_means$num_turtles_numeric, 
           green_means$lat_group,
           IQR=FALSE, skew = FALSE)

# Boxplot
box <- ggplot(data = green_means, 
              aes(x = lat_group,
              y = num_turtles_numeric,
              fill = lat_group)) +
  geom_boxplot()
box  

# Map
mapview(green_means, 
        xcol = "longitude", 
        ycol = "latitude", 
        crs = 4269, 
        grid = FALSE,
        cex = "num_turtles_numeric")

cclme_map <- get_stadiamap(c(left = -120, 
                             bottom = 20, 
                             right = -110, 
                             top = 40), 
                           maptype = "stamen_terrain_background", 
                           crop=FALSE)
ggmap(cclme_map) + 
  geom_point(data=green_means,
             aes(x=longitude,y=latitude,color=lat_group),
             size=4,alpha=.7)

