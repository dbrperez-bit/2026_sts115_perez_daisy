library (tidyverse)
library(lubridate)
#setting up data 
read_csv("Railroad_Equipment_Accident_Incident_Source_Data_(Form_54)_20260224.csv")

raw_data <- read_csv("Railroad_Equipment_Accident_Incident_Source_Data_(Form_54)_20260224.csv")


# similair workflow to least terns 
dim(raw_data)
glimpse(raw_data)
duplicate_count <- sum(duplicated(raw_data))
print(paste("Number of duplicate rows:", duplicate_count))

#cleaning and indicating
clean_data <- raw_data %>%
rename_with(tolower)%>%

  # Command 1: Create clean_data
  clean_data <- raw_data %>%
  rename_with(tolower)  # No pipe here!

# Command 2: Create all_names
all_names <- colnames(raw_data)
#make a formal date object 
all_names <- colnames(raw_data)
print(all_names)

clean_data <- clean_data %>%
  mutate(
    # 1. Use lowercase names (year, month, day) because you used rename_with(tolower)
    date_formatted = make_date(year, month, day), 
    
    # 2. Use 'month' without quotes so it looks for the column, not the word "MONTH"
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter", 
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  )
time_of_day = case_when(
  ampm == "AM" & timehr >= 6 & timehr < 12 ~ "Morning",
  ampm == "PM" & (timehr < 6 | timehr == 12) ~ "Afternoon",
  ampm == "PM" & timehr >= 6 & timehr < 12 ~ "Evening",
  TRUE ~ "Night" # Covers late night AM and early morning
)
)

clean_data <- clean_data %>% 
  mutate(
    time_of_day = case_when(
      ampm == "AM" & timehr >= 6 & timehr < 12 ~ "Morning",
      ampm == "PM" & (timehr < 6 | timehr == 12) ~ "Afternoon",
      ampm == "PM" & timehr >= 6 & timehr != 12 ~ "Evening", # Changed to != 12 to exclude noon
      TRUE ~ "Night" 
    )
  )
# statistics 
# Check the counts for your new variables
clean_data %>% count(season)
clean_data %>% count(time_of_day)


unique(clean_data$weather)

colnames(clean_data)

# Check unique values for impairment
clean_data %>% count(alcohol)
clean_data %>% count(drug)

# Check unique values for weather
clean_data %>% count(weather)

#updating mutate block 
clean_data <- clean_data %>% 
  mutate( 
    is_impaired = case_when(
      alcohol > 0 | drug > 0 ~ "Yes",
      alcohol == 0 & drug == 0 ~ "No", 
      TRUE ~ "Unknown"
    ),

#use data dictionary 
weather_desc = case_when(
  weather == 1 ~ "Clear",
  weather == 2 ~ "Cloudy",
  weather == 3 ~ "Rain",
  weather == 4 ~ "Fog",
  weather == 5 ~ "Sleet",
  weather == 6 ~ "Snow", 
  TRUE ~ "Other"
  )
) %>%

unite("full_narrative", narr1:narr15, sep = " ", remove = FALSE, na.rm = TRUE)

#visuals 

#accidents by season
library(ggplot2)

ggplot(clean_data, aes (x = season)) + 
  geom_bar(fill = "pink") +
  theme_minimal() + 
  labs(title = "Figure 1:Distribution of Accidents by Season", 
       x = "Season",
       y = "Count")
#accidents by weather condition 
ggplot(clean_data, aes(x = weather_desc)) + 
  geom_bar (fill = "pink") + 
  theme_minimal() + 
  labs(title = "Figure 2: Accidents by Weather Condition ",
       x = "Weather Condition",
       y= "Count")

ggplot(clean_data, aes(x = is_impaired)) + 
  geom_bar(fill = "pink") + 
  theme_minimal() + 
  labs(title= "Figure 3: Engineer Impairment Status",
       x = "Impairment Recorded?",
       y = "Count")

sample(clean_data$full_narrative, 5)

# Figure 4: Accidents by Time of Day
ggplot(clean_data, aes(x = time_of_day)) +
  geom_bar(fill = "pink") +
  theme_minimal() +
  labs(title = "Figure 4. Accidents by Time of Day",
       x = "Time of Day",
       y = "Count")

# Convert Type codes to words (Based on Data Dictionary)
clean_data <- clean_data %>%
  mutate(
    type_desc = case_when(
      as.numeric(type) == 1 ~ "Derailment",
      as.numeric(type) == 2 ~ "Head-on Collision",
      as.numeric(type) == 3 ~ "Rear-end Collision",
      as.numeric(type) == 7 ~ "Hwy-Rail Crossing",
      as.numeric(type) == 11 ~ "Fire/Explosion",
      TRUE ~ "Other" # Groups everything else
    )
  )

# Figure 5: Accident Type
ggplot(clean_data, aes(x = type_desc)) +
  geom_bar(fill = "pink") +
  theme_minimal() +
  labs(title = "Figure 5. Distribution of Accident Types",
       x = "Type of Accident",
       y = "Count") +
  coord_flip() # Flips the chart sideways so labels are easy to read


# Create the type_desc column and SAVE it to clean_data
clean_data <- clean_data %>%
  mutate(
    type_desc = case_when(
      as.numeric(type) == 1 ~ "Derailment",
      as.numeric(type) == 2 ~ "Head-on Collision",
      as.numeric(type) == 3 ~ "Rear-end Collision",
      as.numeric(type) == 7 ~ "Hwy-Rail Crossing",
      as.numeric(type) == 11 ~ "Fire/Explosion",
      TRUE ~ "Other" # Groups everything else
    )
  )
