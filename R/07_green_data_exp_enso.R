# Load data and libraries
library("dplyr")
library("ggplot2")
library("mapview")

data <- readRDS("./data/processed/green_means.rds")
enso <- readRDS("./data/processed/enso_years.rds")

green_enso <- left_join(data, enso, by = "year",
                        relationship = "many-to-many") %>% 
  filter(year >= 1899)

plot1 <- ggplot(green_enso %>% 
                  filter(lat_group =="nort"), 
                aes(x = year,
                    y = num_turtles_numeric,
                    color = enso)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("nino" = "red",
                                "nina"="blue",
                                "normal"="grey"))
plot1

plot2 <- ggplot(green_enso, 
                aes(x = longitude,
                    y = latitude,
                    size = num_turtles_numeric,
                    color = enso)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("nino" = "red",
                                "nina"="blue",
                                "normal"="grey")) 
plot2

# Are outliers from ENSO years?
box <- ggplot(green_enso,
              aes(y = num_turtles_numeric)) +
  geom_boxplot()
box

outliers <- green_enso %>% 
  filter(num_turtles_numeric >= 2)

outliers$enso # All are El Niño years

# Map ------------------------------
mapview(green_enso, 
        xcol = "longitude", 
        ycol = "latitude", 
        crs = 4269, 
        grid = FALSE,
        cex = "num_turtles_numeric",
        zcol = "enso")

