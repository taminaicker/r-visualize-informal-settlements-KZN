#1. Data Import
# ------------------------------------

# Load packages 
library(here)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(sf)

# Construct the file path (absolute path) 
#data_path <- ("/Users/xxx/Informal_Settlements_KZN/Informal_Settlement_Programme.csv")

# Read and load the data using here() and read_csv
data <- read_csv(here("Informal_Settlement_Programme.csv")) 

#2. Data Cleaning and Pre-processing 
# ------------------------------------

top_10_rows <- head(data, 10)
print(top_10_rows)

# Removing 'ALT_NAME' Column 
data <- data %>% 
  select(-ALT_NAME)

top_10_rows <- head(data, 10)
print(top_10_rows)

# Check for missing values 
missing_values <- is.na(data)
missing_summary <- colSums(missing_values)
print(missing_summary)


# Check for duplicate rows 
dups <- data[duplicated(data), ]
print(dups)

# Get the unique values in the 'REGION' column 
unique_region <- unique(data$REGION)
print(unique_region)

# Get the unique values in the 'WATER_SUPP' column 
unique_water <- unique(data$WATER_SUPP)
print(unique_water)

# Standardize the class types in "WATER_SUPP' 
data <- data %>%
  mutate(WATER_SUPP_STANDARDIZED = case_when(
    tolower(WATER_SUPP) %in% c("ablution facility taps & standpipes", "ablution facility taps & 1 standpipe", "ablution facility taps & 3 standpipe", "1 standpipe", "ablution facility taps & 30 standpipe", "ablution facility taps & 25 standpipe", "ablution facility taps & 2 standpipe", "ablution facility taps & 6 standpipe", "ablution facility taps & 4 standpipe", "2 standpipes and on-yard taps", "ablution facility taps, 2 standpipes & ground water tanks", "ablution facility taps, 3 standpipes and 2 washing basins", "ablution facility taps, standpipes & ground water tank", "1 standpipe", "ablution facility taps, varsity water & standpipes", "ablution facility taps & 5 standpipe", "5 standpipe", "4 standpipe", "ablution facility taps, standpipes & low-pressure tanks", "ablution facility taps & individual connections", "ablution facility taps and ground water tank", "ablution facility taps, standpipes & low-pressure tanks", "ablution facility taps, standpipes & individual connections") ~ "Ablution Facility Taps & Standpipes",
    tolower(WATER_SUPP) %in% c("individual connections", "standpipe and exposed waterpipe", "standpipes / individual household connection") ~ "Individual Connections",
    tolower(WATER_SUPP) %in% c("standpipe") ~ "Standpipe",
    tolower(WATER_SUPP) %in% c("ground water tank", "communal tanks") ~ "Water Tanks",
    tolower(WATER_SUPP) %in% c("no water supply", "no") ~ "No Water Supply",
    tolower(WATER_SUPP) %in% c("nearby houses") ~ "Nearby Houses",
    TRUE ~ WATER_SUPP  # keep the original value if it doesn't match any of the conditions above
  ))

# Remove original 'WATER_SUPP' column 
data <- data %>% 
  select(-WATER_SUPP)

# Get the unique values in the 'SANITATION' column 
unique_sanitation <- unique(data$SANITATION)
print(unique_sanitation)

# Standardize the class types in 'SANITATION'
data <- data %>%
  mutate(SANITATION_STANDARDIZED = case_when(
    tolower(SANITATION) %in% c("pit privies", "pit latrine") ~ "Pit Privies",
    tolower(SANITATION) %in% c("ablution facility & pit privies", "ablution facility & vip toilets", "ablution facility & pit privies", "ablution facility", "ablution facility, pit privies", "ablution facillity", "ablution facility & pit privies", "ablution facility & vip toilets", "ablution facility & vip", "vip toilets", "ablution facilty", "planned ablution block", "ablution facility & vips", "ablution facilities & pit privies", "pit latrine", "vip & pit privies", "ablution facility & vip", "pit latrine", "water borne sewer", "pit privies, vip, flush toilets", "ablutiion facility") ~ "Ablution Facility & Pit Privies",
    tolower(SANITATION) %in% c("tbc") ~ "TBC",
    tolower(SANITATION) %in% c("no sanitation facilities") ~ "No Sanitation Facilities",
    tolower(SANITATION) %in% c("1 flushtoilet", "flush toilets") ~ "Flush Toilets",
    TRUE ~ SANITATION  # Keep the original value if it doesn't match any of the conditions above
  ))

# Remove the original 'SANITATION' column 
data <- data %>% 
  select(-SANITATION)

# Get the unique values in the 'REFUSE' column 
unique_refuse <- unique(data$REFUSE)
print(unique_refuse)

# Get the unique values in the 'ELECTRIC_2' column 
unique_elec <- unique(data$ELECTRIC_2)
print(unique_elec)

# Standardize the 'ELECTRIC_2' class types 
data <- data %>%
  mutate(ELECTRIC_2_STANDARDIZED = case_when(
    tolower(ELECTRIC_2) %in% c("yes", "yes_investigate / prioritize", "yes_under construction", "yes_35 units planned", "yes_investigate  / prioritise") ~ "Yes",
    tolower(ELECTRIC_2) %in% c("planned", "planned_street lights", "planned?") ~ "Planned",
    tolower(ELECTRIC_2) %in% c("tbc") ~ "TBC",
    tolower(ELECTRIC_2) %in% c("yes_under construction") ~ "Yes_Under Construction",
    tolower(ELECTRIC_2) %in% c("yes_high mast") ~ "Yes_High Mast",
    tolower(ELECTRIC_2) %in% c("yes_shared prepaid meter") ~ "Yes_Shared Prepaid Meter",
    tolower(ELECTRIC_2) %in% c("no", "not electrified", "not electrified") ~ "No",
    TRUE ~ ELECTRIC_2  # Keep the original value if it doesn't match any of the conditions above
  ))

# Remove the original 'ELECTRIC_2' column 
data <- data %>% 
  select(-ELECTRIC_2) 

# Remove irrelevant features
data <- data %>% 
  select(-EST_HHC, -NUSP_CAT)

#3: Exploratory Data Analysis (EDA) 
# ------------------------------------

# Which region has the most informal settlements? 
region_plot <- ggplot(data, aes(x = REGION)) + 
  geom_bar(fill = "pink") + 
  labs(title = "Informal Settlements by Region", 
       x = "Region", 
       y = "Count") + 
  theme_minimal()

print(region_plot)


# What types of water supply are used? 
water_plot <- ggplot(data, aes(x = WATER_SUPP_STANDARDIZED, fill = WATER_SUPP_STANDARDIZED)) + 
  geom_bar(fill = "lightgreen")+ 
  labs(title = "Water Supply Types", 
       x = "Count", 
       y = "Water Supply Type") + 
  theme_minimal()+ 
  coord_flip()

print(water_plot)


# What types of sanitation are used?
sanitation_plot <- ggplot(data, aes(x = SANITATION_STANDARDIZED, fill = SANITATION_STANDARDIZED)) + 
  geom_bar(fill = "orange")+ 
  labs(title = "Sanitation Types", 
       x = "Count", 
       y = "Sanitation Type") + 
  theme_minimal()+ 
  coord_flip()

print(sanitation_plot)


# Do the settlements have electricity access? 
electric_plot <- ggplot(data, aes(x = ELECTRIC_2_STANDARDIZED, fill = ELECTRIC_2_STANDARDIZED)) + 
  geom_bar(fill = "purple")+ 
  labs(title = "Electricity Access", 
       x = "Count", 
       y = "Electricity Supply Type") + 
  theme_minimal()+ 
  coord_flip()

print(electric_plot) 

# What is the most common method of refuse disposal? 
refuse_counts <- data %>%  #create a data frame with counts of each refuse type
  group_by(REFUSE) %>% 
  summarise(count = n()) 

#Create a pie chart: 
refuse_plot <- ggplot(refuse_counts, aes(x = "", y = count, fill = REFUSE))+ 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  labs(title = "Refuse Types",
       x = NULL, 
       y = NULL, 
       fill = "Refuse Type")+ 
  theme_minimal()+ 
  theme(legend.position = "right")

print(refuse_plot)


#4. Create a map to show the locations of informal settlements in KZN
# ------------------------------------

# Load the GeoJSON map of informal settlements
informal_settlements_sf <- st_read(here("Informal_Settlement_Programme.geojson"))

# Create the map 
informal_settlements_map <- ggplot() +
  geom_sf(data = informal_settlements_sf, color = "lightblue", size = 1) +  
  labs(title = "Informal Settlements in KwaZulu-Natal") +
  theme_minimal()

# Show the map
print(informal_settlements_map)
