# Exploratory data visualization

# Load data and libraries
data <- readRDS("./data/processed/green_means_full_chronology.rds")
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
         year) 

north <- green %>% 
  filter(lat_group == "north")

south <- green %>% 
  filter(lat_group == "south")

# Get descriptive stats to look at differences by latitude
describeBy(green$num_turtles_numeric, 
           green$lat_group,
           IQR=FALSE, skew = FALSE)

# Boxplot
box <- ggplot(data = green, 
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
                           crop=TRUE,
                           source = "stamen")

save(cclme_map, file = "cclme_map.RData") # Save for future use
load(file = "cclme_map.RData") # Load for future use

ggmap(cclme_map) + 
  geom_point(data=green %>% 
               na.omit(),
             aes(x=longitude,
                 y=latitude,
                 color=lat_group,
                 size=num_turtles_numeric),
             alpha=.7)

