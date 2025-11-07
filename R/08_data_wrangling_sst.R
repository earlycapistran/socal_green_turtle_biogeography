library(tidyverse)
library(readr)
library(dplyr)

# Replace 'your_file.csv' with the path to your CSV file
file_path <- './data/raw/sea-surface-temp_fig_readonly.csv'

# Read the CSV data
raw_data <- read_csv(file_path, col_names = FALSE, na = "NA")

# Verify the raw data structure
cat("Raw data dimensions:\n")
print(dim(raw_data))

# Extract latitudes and longitudes ensuring all are numeric
latitudes <- as.numeric(raw_data$X1[-1])  # Latitude values from the first column, omitting the first row
longitudes <- as.numeric(unlist(raw_data[1, -1]))  # Longitude values from the first row, omitting the first column

# Check the obtained latitudes and longitudes
cat("Number of latitudes:\n")
print(length(latitudes))
cat("Number of longitudes:\n")
print(length(longitudes))

# Exclude the first row and column, which are used for latitudes and longitudes
data <- raw_data[-1, -1]

# Check dimensions of the data to verify correctness
cat("Data dimensions (before conversion):\n")
print(dim(data))

# Convert the data frame to numeric type safely
data <- data.frame(lapply(data, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)

# Verify the structure of the converted data
cat("Data dimensions (after conversion):\n")
print(dim(data))
print(head(data))

# Ensure no remaining rows or columns are being removed/mismatched
cat("Data structure:\n")
str(data)

# Reshape the data into a long format ------------------------------
# Make sure the number of rows in the reshaped data matches the length(latitudes) * length(longitudes)

if (nrow(data) == length(latitudes) & ncol(data) == length(longitudes)) {
  # Proceed if dimensions are aligned correctly
  df_long <- expand_grid(Latitude = latitudes, Longitude = longitudes) %>% 
    mutate(Value = as.vector(t(as.matrix(data))))
  
  # Filter out the NA values if necessary
  df_long <- df_long %>% filter(!is.na(Value))
  
  # Save the reshaped data to a new CSV file suitable for QGIS
  write_csv(df_long, './data/processed/reshaped_data.csv')
  
  # Display the first few rows of the transformed data
  cat("Transformed data sample:\n")
  print(head(df_long))
} else {
  cat("Error: Mismatch in data dimensions and reshaped data.\n")
  cat("Expected Rows - Latitudes:", length(latitudes), "\n")
  cat("Expected Columns - Longitudes:", length(longitudes), "\n")
  cat("Data Dimensions:", dim(data), "\n")
}

# Sample several random points for comparison -------------------------
set.seed(123)  # For reproducibility
sample_indices <- sample(1:nrow(df_long), 10)

# Function to retrieve original value based on latitude and longitude
get_original_value <- function(lat, lon, raw_data) {
  row_index <- which(as.numeric(raw_data$X1) == lat)
  col_index <- which(as.numeric(raw_data[1,]) == lon)
  if (length(row_index) == 1 & length(col_index) == 1) {
    return(as.numeric(raw_data[row_index, col_index]))
  } else {
    return(NA)
  }
}

validation_results <- df_long[sample_indices, ] %>%
  rowwise() %>%
  mutate(
    Original_Value = get_original_value(Latitude, Longitude, raw_data)
  )

# Print the validation results
print(validation_results)

# Convert Fahrenheit to Celsius and add a new column 'Value_Celsius'
df_long <- df_long %>%
  mutate(Value_Celsius = Value * 5 / 9)

# Save the reshaped data with the new column to a new CSV file suitable for QGIS
write_csv(df_long, './data/processed/sst_data_qgis.csv')
