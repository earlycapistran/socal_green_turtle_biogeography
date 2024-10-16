# DATA QUALITY INDEX

# Load data and libraries
historical <- readRDS("./data/processed/full_data_historical_round2.rds")
library("fastDummies")
library("dplyr")

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
  mutate(weight_dummy = ifelse(is.na(weight_kg), 0, 1)) %>% 
  mutate(length_dummy = ifelse(is.na(length_cm), 0, 1)) %>% 
  mutate(habitat_dummy = ifelse(habitat == "not_reported", 0, 1)) %>% 
  mutate(narrative_detail = as.numeric(narrative_detail))

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
             narrative_detail))))

# Save another data quality index as ordinal
historical <- historical %>%
  mutate(data_quality_cat = case_when(
    between(data_quality_index, 1,3) ~ "low",
    between(data_quality_index, 4,6) ~ "medium",
    between(data_quality_index, 7, 9) ~ "high"))

