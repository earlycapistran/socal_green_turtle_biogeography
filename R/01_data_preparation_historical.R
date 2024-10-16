# DATA PREPARATION
# Fixing data classes and making .rds for cleaning and wrangling

# Load data and packages
raw_data <- read.csv("./data/raw/raw_data_historical.csv")
library("lubridate")
library("dplyr")

# View data
str(raw_data)
head(raw_data)

# Fix data classes ---------------

# Convert to dates
raw_data$fulldate <- ymd(raw_data$fulldate)

# Convert to numeric 
raw_data$p_number <- as.numeric(raw_data$p_number)

# Convert to factors
raw_data <-raw_data %>% 
  mutate(across(c(species, spp_id, location, location_certainty, habitat), 
                as.factor))

# Specify units
raw_data <- raw_data %>% 
  rename(length_ft_inches = length) %>% 
  rename(weight_lb = weight)

# Make a column for year
raw_data <- raw_data %>% 
  mutate(year = format(as.Date(fulldate, format="%Y/%m/%d"),"%Y"))

# Fix typo
raw_data$spp_id <- str_replace(raw_data$spp_id, "infered", "inferred")
raw_data$spp_id <- as.factor(raw_data$spp_id)

# Save as .rds
saveRDS(raw_data, "./data/raw/raw_historical_data.rds")

