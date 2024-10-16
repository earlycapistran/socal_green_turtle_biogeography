# Load data and libraries
location <- read.csv("./data/raw/location_raw.csv")
historical <- readRDS("./data/processed/full_data_historical_round1.rds")
library("dplyr")
library("stringr")

# Compare site names across datasets ----------------------------
head(location)
head(historical)

# Extract locations as vectors
locations_base <- location$location
locations_historical <- historical$location

 # Compare
identical(locations_base, locations_historical)
intersect(locations_base, locations_historical)
setdiff(locations_base, locations_historical)

# Make a vector of locations that aren't in the historical dataset
diff_loc <- unlist(setdiff(locations_base, locations_historical))

# Remove lines outside study area
locations_processed <- location %>% 
  filter(!location %in% diff_loc) %>% 
  filter(!location %in% c("bahia_de_los_angeles",
                          "french_frigate_shoals")) # Outside study area

historical <- historical %>% 
  filter(!location %in% c("bahia_de_los_angeles",
                          "french_frigate_shoals"))

# Compare again with reverse order and save output
setdiff(locations_historical, locations_processed$location)
# List rows with more than one location
mult_loc <- unlist(setdiff(locations_historical, locations_processed$location))
# We want to keep  "not_reported" in the original df
mult_loc <- mult_loc[ !mult_loc == 'not_reported'] 

# Make a new dataframe to parse out rows with multiple locations
historical_2 <- historical %>% 
  filter(location %in% mult_loc) %>% 
  # Split sites into two new columns (one for each)
  separate(location, into = c('loc_1', 'loc_2'), ",", convert = TRUE) %>% 
  # Pivot long to make one row for each location
  pivot_longer(cols=c('loc_1', 'loc_2'),
               values_to='location') %>% 
  select(-c("name")) %>% 
  mutate(location = str_trim(location)) # Remove whitespace

# Put everything together and remove rows with multiple sites
historical_3 <- rbind(historical_2, historical) %>% 
  filter(!location %in% mult_loc)

# Let's compare the new dataframe ---------------------------
setdiff(historical_3$location, locations_base) # Everything is in the new df

# Add latitude and longitude
historical_3 <- full_join(historical_3, locations_processed) %>% 
  filter(!location %in% c("la_paz", "french_frigate_shoals"))

# Convert locations to factor
historical_3 <- historical_3 %>% 
  mutate(location= as.factor(location))

# Save dataframe with clean locations
saveRDS(historical_3, "./data/processed/full_data_historical_round2.rds")
write.csv(historical_3, "./data/processed/full_data_historical_round2.csv")

