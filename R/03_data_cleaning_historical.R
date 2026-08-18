# DATA CLEANING

# Load data and packages -----------------------------------
hist_data <- readRDS("./data/processed/historical_dqi.rds")
library(dplyr)
library(measurements)
library(tidyr)

# Function to classify year by decade
classify_by_decade <- function(year) {
  # Ensure the input is numeric
  year <- as.numeric(year)
  # Create the decade classification
  decade <- floor(year / 10) * 10
  return(decade)
}

# Clean up data with mixed characters and numbers -----------------
# Starting with number of turtles: generate placeholder for "cargo" using
# mean value of reported shipments

# Make vector with only numeric values
num_turtles_numeric <- as.numeric(hist_data$num_turtles) %>% 
  na.omit()

# Cut off based on smallest shipment
cargo_values <- (num_turtles_numeric[num_turtles_numeric >= 17]) 
mean_cargo <- mean(cargo_values)

# Replace "cargo" with mean values and "not reported" with "NA"
processed_data <- hist_data %>% 
  mutate(num_turtles_numeric = ifelse(
    num_turtles == "cargo", mean_cargo, num_turtles)) %>% 
  mutate(num_turtles_numeric = ifelse(
    num_turtles_numeric == "not_reported", NA, num_turtles_numeric)) %>% 
  mutate(num_turtles_numeric = as.numeric(num_turtles_numeric))

# Add NAs to weight and length, and convert weight to numeric
processed_data <- processed_data %>% 
  mutate(weight_lb = ifelse(weight_lb == "not_reported", NA, weight_lb)) %>% 
  mutate(weight_lb = as.numeric(weight_lb)) %>% 
  mutate(weight_kg = round(weight_lb * 0.45359237, 2)) # Convert to kg

# For length, convert feet to cm
processed_data <- processed_data %>% 
  # 0s as placeholders for missing values
  mutate(length_ft_inches = ifelse(length_ft_inches == "not_reported", 
                                   "0'0", 
                                   length_ft_inches)) %>% 
  mutate(length_ft_inches = gsub('["]', '', length_ft_inches)) # Remove inch symbol

length_df <- processed_data %>% # Wrangle length to cm in a separate dataframe
  select(length_ft_inches) %>% 
  # Make separate columns for feet and inches, then convert to cm
  separate(length_ft_inches, into = c('feet', 'inches'), "'", convert = TRUE) %>% 
  mutate(inches = ifelse(is.na(inches), 0, inches)) %>% 
  mutate(length_cm = (12*feet + inches)*2.54) %>% 
  mutate(length_cm = ifelse(length_cm == 0, NA, length_cm)) # remove placeholder 0s

# Add to the main dataframe
# FIX: previously only length_cm was copied back from processed_data,
# silently dropping num_turtles_numeric and weight_kg. Restored below.
hist_data <- hist_data %>% 
  mutate(length_cm = length_df$length_cm) %>%
  mutate(num_turtles_numeric = processed_data$num_turtles_numeric) %>%
  mutate(weight_kg = processed_data$weight_kg) %>%
  mutate(length_ft_inches = ifelse(length_ft_inches == "0'0", 
                                   NA, 
                                   length_ft_inches)) # Remove placeholder 0s

# Add variable by decade
hist_data <- hist_data %>% 
  mutate(decade = classify_by_decade(year))

# Save ------------------------------------------------
write.csv(hist_data, "./data/processed/full_data_historical_round1")
saveRDS(hist_data, "./data/processed/full_data_historical_round1.rds")