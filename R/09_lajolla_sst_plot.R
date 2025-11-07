# Load Libraries and data
library("tidyverse")
library("janitor")
library("zyp")

# Load data
base_data <- read.csv("./data/raw/scripps_lajolla_temp/LaJolla_temp_1916-202506_dataOnly.csv")

# Load pallettes and theme
# devtools::install_github("thomasp85/scico") Scientific color maps
library("scico")
theme_set(theme_light())

# Check data structure
str(base_data)

# Clean data --------------------
base_data <- base_data %>% 
  janitor::clean_names()

# Select variables of interest
filtered_data <- base_data %>% 
  select(c(year, surf_temp_c)) %>% 
  na.omit()

# Calculate baseline temp and anomalies --------

# Establish baseline for 1916-2024
baseline_sst <- mean(filtered_data$surf_temp_c)

# Get mean annual temp for 1916-2024
graphing_temps <- filtered_data %>% 
  mutate(year = year) %>% 
  group_by(year) %>% 
  summarize(annual_mean_sst = mean(surf_temp_c)) %>% 
  ungroup()

# Calculate anomaly and add to dataframe
graphing_temps <- graphing_temps %>% 
  mutate(temp_anomaly = annual_mean_sst - baseline_temp)

# Look over data ---------------------------------------------------
graphing_temps %>% skimr::skim()

graphing_temps %>% 
  ggplot(aes(x = year, y = temp_anomaly)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, lty = 2)

# Plot -------------------------------------------------------------
graphing_temps %>% 
  ggplot(aes(x = year, y = 1, fill = temp_anomaly)) +
  geom_tile() +
  scale_fill_scico(palette = 'vik') +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = "Year",
    y = NULL
  )

# Statistical tests -------------------------------------------

# Linear regression
graphing_temps %>% 
  ggplot(aes(x = year, y = annual_mean_sst)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(year ~ annual_mean_sst, data = graphing_temps)
summary(model)
summary(model)$r.squared

# Mann-Kendall
mk_result <- MannKendall(graphing_temps$annual_mean_sst)
mk_result

# Calculate Kendall's tau and its associated p-value
tau_result <- cor.test(graphing_temps$annual_mean_sst,
                       graphing_temps$year, 
                       method = "kendall")

# Print Kendall's tau and its associated p-value
print(tau_result)
