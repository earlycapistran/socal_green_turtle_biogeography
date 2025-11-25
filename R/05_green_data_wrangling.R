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

# Datasets by location split by southern limit of SoCal Bight
# (Punta Colonet, BC)
green_data <- green_data %>% 
  mutate(lat_group = as.factor(ifelse(latitude <31, "south", "north")))

# Save
saveRDS(green_data, 
        "./data/processed/full_data_green.rds")

# Make an analysis dataset with primary variables  
# and timeframe of interest 

green_quant <- green_data %>% 
  select(num_turtles_numeric, 
         latitude, 
         longitude,
         lat_group,
         location,
         location_certainty,
         data_quality_index,
         data_quality_cat,
         year,
         decade,
         state,
         county_muni,
         country) %>% 
  filter(!is.na(num_turtles_numeric)) %>% 
  filter(num_turtles_numeric != 0) %>% 
  filter(location != "not_reported") %>% 
  filter(location != "na") %>% 
  filter(county_muni != "na") %>% 
  filter(year < 1940) %>%  # Limit to commercial fishing years 
  filter(latitude < 35) # Remove values north of SoCal Bight

# Save
saveRDS(green_quant, "./data/processed/green_quant.rds")

# Select columns of interest and filter out rows w/o num_turtles
green_means <- green_quant %>% 
  select(num_turtles_numeric, 
         latitude, 
         longitude,
         lat_group,
         location,
         year,
         decade,
         state,
         county_muni,
         country) 

green_means <- green_quant %>% 
  group_by(location, year, lat_group) %>% 
  summarise(across(where(is.numeric), 
                   ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(year = as.numeric(year))

# Save
saveRDS(green_means, "./data/processed/green_means.rds")

green_means_full_chronology <- green_means %>% 
  group_by(location, lat_group) %>% 
  summarise(across(where(is.numeric), 
                   ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(year = as.numeric(year)) 

# Save
saveRDS(green_means_full_chronology, "./data/processed/green_means_full_chronology.rds")
write.csv(green_means_full_chronology, "./data/processed/green_means_full_chronology.csv")

