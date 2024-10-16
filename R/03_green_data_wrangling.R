# Green turtle data wrangling

# Load data and libraries
library(dplyr)
full_data <- readRDS("./data/processed/full_processed_data_historical.rds")

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
