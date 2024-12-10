# Load data and libraries
library("dplyr")
library("ggplot2")
library("ggmap")
library("osmdata")
library("mapview")

data <- readRDS("./data/processed/green_means.rds")
enso <- readRDS("./data/processed/enso_years.rds")

green_enso <- left_join(data, 
                        enso, 
                        by = "year",
                        relationship = "many-to-many") %>% 
  filter(year >= 1899) %>% 
  filter(lat_group == "north")

# Map ---------------------------------------
mapview(green_enso, 
        xcol = "longitude", 
        ycol = "latitude", 
        crs = 4269, 
        grid = FALSE,
        cex = "num_turtles_numeric",
        zcol = "enso")

