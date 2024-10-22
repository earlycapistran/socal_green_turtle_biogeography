# Load data and libraries
library ("dplyr")
enso <- read.csv("./data/raw/enso_years.csv") 

enso <- enso %>% 
  select(c(year, enso)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(enso = as.factor(enso))

# save
saveRDS(enso, "./data/processed/enso_years.rds")
