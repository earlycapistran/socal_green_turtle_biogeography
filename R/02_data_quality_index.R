# DATA QUALITY INDEX

# Load data and libraries
historical <- readRDS("./data/raw/clean_historical_data.rds")
library("fastDummies")
library("dplyr")
library("tidyr")

# Check factor levels for data quality indicators
levels(historical$location_certainty)
levels(historical$spp_id)

# Create numberical dummy varialbes
historical <- historical %>%
  mutate(location_certainty_dummy = case_when(
    location_certainty == "known" ~ 2,
    location_certainty == "inferred" ~ 1,
    location_certainty == "not_reported" ~ 0))

historical <- historical %>% 
  mutate(spp_id_dummy = ifelse(spp_id == "reported", 1, 0)) %>% 
  mutate(weight_dummy = ifelse(is.na(weight_lb), 0, 1)) %>% 
  mutate(length_dummy = ifelse(is.na(length_ft_inches), 0, 1)) %>% 
  mutate(habitat_dummy = ifelse(habitat == "not_reported", 0, 1)) %>% 
  mutate(narrative_detail = as.numeric(narrative_detail)) %>% 
  mutate(num_turtles_dummy = case_when(
    num_turtles == "not_reported" ~ 0,
    num_turtles == "cargo" ~ 1,
    num_turtles != "not_reported" | num_turtles != "cargo" ~ 2))

# Calculate data quality index as sum of location certainty, 
# spp_id, narrative detail and presence of weight, length, 
# and/or habitat

historical <- historical %>% 
  mutate(data_quality_index = rowSums(
    across(c(location_certainty_dummy,
             spp_id_dummy,
             weight_dummy,
             length_dummy,
             habitat_dummy,
             narrative_detail,
             num_turtles_dummy))))

# Save another data quality index as ordinal
historical <- historical %>%
  mutate(data_quality_cat = case_when(
    between(data_quality_index, 1,3) ~ "low",
    between(data_quality_index, 4,6) ~ "medium",
    between(data_quality_index, 7, 10) ~ "high"))

# Convert narrative detail back to factor and export
# dataset only with relevant indices

historical_save <- historical %>% 
  mutate(narrative_detail = as.factor(narrative_detail)) %>% 
  select(-c(location_certainty_dummy,
            spp_id_dummy,
            weight_dummy,
            length_dummy,
            habitat_dummy,
            num_turtles_dummy))

# This is a full set of historical data cleaned and prepped for use in R
saveRDS(historical_save, "./data/processed/historical_dqi.rds")
