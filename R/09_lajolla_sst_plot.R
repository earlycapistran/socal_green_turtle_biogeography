# Load Libraries and data
library("tidyverse")
library("janitor")
library("zyp")
library("RColorBrewer")

# Load data
base_data <- read.csv("./data/raw/scripps_lajolla_temp/LaJolla_temp_1916-202506_dataOnly.csv")

# Load pallettes and theme
# devtools::install_github("thomasp85/scico") Scientific color maps
library("scico")
theme_set(theme_light())

# Alternate: RColorBrewer
col_strip <- brewer.pal(11, "RdBu")

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
  mutate(temp_anomaly = annual_mean_sst - baseline_sst)

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

# Plot with RColorBrewer----------------------------------------------
graphing_temps %>% 
  ggplot(aes(x = year, y = 1, fill = temp_anomaly)) +
  geom_tile() +
  scale_fill_gradientn(colors = rev(col_strip))  +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "La Jolla Pier, 1916-2025",
  )


graphing_temps %>% 
  ggplot(aes(x = year, y = temp_anomaly, fill = temp_anomaly)) +
  geom_col() +
  scale_fill_gradientn(colors = rev(col_strip))  +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = "none") +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top", title.hjust = 0.5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Temperature Change at La Jolla Pier",
    subtitle = "Relative to average of 1916-2025 [°C]"
  ) +
  theme(axis.text=element_text(size=16),
         axis.title=element_text(size=18))
  