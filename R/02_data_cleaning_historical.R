# DATA CLEANING

# Load data and packages -----------------------------------
raw_data <- readRDS("./data/raw/raw_historical_data.rds")
library(dplyr)
library(measurements)
library(tidyr)

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


# Add NAs to weight and length, and convert weight to numeric
raw_data <- raw_data %>% 
  mutate(weight = ifelse(weight == "not_reported", NA, weight)) %>% 
  mutate(weight = as.numeric(weight)) %>% 
  mutate(weight_kg = round(weight * 0.45359237, 2)) # Convert to kg

# For length, convert feet to cm
raw_data <- raw_data %>% 
  # 0s as placeholders for missing values
  mutate(length_ft_inches = ifelse(length_ft_inches == "not_reported", 
                                   "0'0", 
                                   length_ft_inches)) %>% 
  mutate(length_ft_inches = gsub('["]', '', length_ft_inches)) # Remove inch symbol

length_df <- raw_data %>% # Wrangle length to cm in a separate dataframe
  select(length_ft_inches) %>% 
  # Make separate columns for feet and inches, then convert to cm
  separate(length_ft_inches, into = c('feet', 'inches'), "'", convert = TRUE) %>% 
  mutate(length_cm = (12*feet + inches)*2.54) %>% 
  mutate(length_cm = ifelse(length_cm == 0, NA, length_cm)) # remove placeholder 0s

# Add to the main dataframe
raw_data <- raw_data %>% 
  mutate(length_cm = length_df$length_cm) %>%
  mutate(length_ft_inches = ifelse(length_ft_inches == "0'0", 
                                   NA, 
                                   length_ft_inches)) # Remove placeholder 0s

# Save ------------------------------------------------
write.csv(raw_data, "./data/processed/full_processed_data_historical")
saveRDS(raw_data, "./data/processed/full_processed_data_historical.rds")

