# DATA PREPARATION
# Fixing data classes and making .rds for cleaning and wrangling

# Load data and packages
raw_data <- read.csv("./data/raw/raw_data_historical.csv")
library("lubridate") # To set up dates
library("dplyr")
library("stringr") # To handle strings
library("janitor") # To tidy up data

# View data
str(raw_data)
head(raw_data)

# Convert string NAs to real NAs
clean_data <- raw_data %>%
  mutate(across(
    where(is.character), # Run across all character columns
    ~ na_if(.x, "na")  # Directly replaces "na" with NA
  ))

# Check for and identify empty rows
problem_index <- which(rowSums(is.na(raw_data) | raw_data == "") == ncol(clean_data))

# Remove it
clean_data <- clean_data %>% 
  slice(-problem_index)

# Fix data classes ---------------
# Convert to dates
clean_data$fulldate <- ymd(clean_data$fulldate)

# Convert to numeric 
clean_data$p_number <- as.numeric(clean_data$p_number)

# Convert to factors
clean_data <-clean_data %>% 
  mutate(across(c(species, 
                  spp_id, 
                  location, 
                  location_certainty, 
                  habitat, 
                  narrative_detail), 
                as.factor))

# Specify units
clean_data <- clean_data %>% 
  rename(length_ft_inches = length) %>% 
  rename(weight_lb = weight)

# Make a column for year
clean_data <- clean_data %>% 
  mutate(year = format(as.Date(fulldate, format="%Y/%m/%d"),"%Y"))

# Change species to factor
clean_data$spp_id <- as.factor(clean_data$spp_id)

# Save as .rds
saveRDS(clean_data, "./data/raw/clean_historical_data.rds")

