# Load data
location <- read.csv("./data/raw/location_raw.csv")
historical <- readRDS("./data/processed/full_processed_data_historical.rds")

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

# [1] "la_paz"              "salina_cruz"         "gulf_of_calfornia"
# [4] "astoria"             "san_juan_capistrano" "east_san_pedro"

# Remove lines outside study area
locations_processed <- location %>% 
  filter(!location %in%  c("la_paz", "salina_cruz", "gulf_of_calfornia", "astoria"))

# Compare again with reverse order and save output
setdiff(locations_historical, locations_processed$location)
mult_loc <- unlist(setdiff(locations_historical, locations_processed$location))

# Let's duplicate these rows in the historical dataset
loc_historical_duplicate <- historical %>% filter(location %in% mult_loc)
