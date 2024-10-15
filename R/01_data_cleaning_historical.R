# DATA CLEANING

# Load data and packages
raw_data <- readRDS("./data/raw/raw_historical_data.rds")
library(dplyr)

# Clean up data with mixed characters and numbers -----------------

# Starting with number of turtles: generate placeholder for "cargo" using
# mean value of reported shipments

# Make vector with only numeric values
num_turtles_numeric <- as.numeric(raw_data$num_turtles) %>% 
  na.omit()

# Cut off based on smallest shipment
cargo_values <- (num_turtles_numeric[num_turtles_numeric >= 17]) 
mean_cargo <- mean(cargo_values)

# Replace "cargo" with mean values and "not reported" with "NA"
raw_data <- raw_data %>% 
  mutate(num_turtles_numeric = ifelse(
    num_turtles == "cargo", mean_cargo, num_turtles))%>% 
  mutate(num_turtles_numeric = ifelse(
    num_turtles_numeric == "not_reported", NA, num_turtles_numeric)) %>% 
  mutate(num_turtles_numeric = as.numeric(num_turtles_numeric))
