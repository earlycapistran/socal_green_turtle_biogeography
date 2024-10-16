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
  mutate(spp_id_dummy = ifelse(spp_id == "reported", 1, 0))
