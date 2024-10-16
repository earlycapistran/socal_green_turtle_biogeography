# Green turtle data wrangling

# Load data and libraries
library(dplyr)
full_data <- readRDS("./data/processed/full_data_historical_locations.rds")

# Select primary columns of interest
green_data <- full_data %>% 
  select(-c(p_number, 
            secondary_p_number, 
            url, 
            num_turtles, 
            length_ft_inches, 
            weight_lb))

# Select green turtles
green_data <-green_data %>% 
  filter(species == "green")

# Save

saveRDS(green_data, "./data/processed/full_data_green.rds")
