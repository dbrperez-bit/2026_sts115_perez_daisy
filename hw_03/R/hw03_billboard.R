# HW 03 - Billboard Hot 100 #1s EDA
# Name: Daisy Perez Cruz
# Course: Data cleaning, plotting, and forensics

library(readxl)
library(tidyverse)

# setting up the data 
billboard <- read_excel("Billboard Hot 100 Number Ones Dataset.xlsx")
glimpse(billboard)
summary(billboard)

#data forensics 
#checking for missing values 
colSums(is.na(billboard))
#checking for duplicates 
sum(duplicated(billboard))
#identifying column names 
names(billboard)

# idnetifying top artists
top_artists <- billboard %>%
  count(artist, sort = TRUE) %>%
  head(10)

print(top_artists)


colSums(is.na(billboard))

summarize
print(top_artists)


# figure 1 
ggplot(top_artists, aes(x= reorder(artist, n), y = n ))+
  geom_col(fill = "pink") +
  coord_flip() + 
  labs( 
    title = "Top 10 Artists with the Most 1 Hits",
    x = "Artsit", 
    y = "Total Number of Hits"
  ) + 
    theme_minimal()

billboard %>%
  select(artist, track, date.entered) %>%
  summarize_all(~ sum(is.na(.)))
  
billboard %>% 
  select(artist, track, wk1:wk10) %>%
  rowwise() %>%
  mutate(total_presence = sum(!is.na(c_across(wk1:wk10)))) %>%
    arrange(desc(total_presence))

#figure 2 

ggplot(billboard, aes(x = date.entered)) + 
  geom_histogram(fill ="pink",color = "white", bins = 30)+
  labs(
    title = "timeline of billboard #1 Hits", 
    x="Date Entered Chart", 
    y= "Number of Songs"
  ) + 
  theme_minimal()

  
  
# Calculate total weeks on chart for each track
billboard_longevity <- billboard %>%
  pivot_longer(cols = starts_with("wk"), names_to = "week", values_to = "rank") %>%
  drop_na(rank) %>%
  group_by(artist, track) %>%
  summarize(total_weeks = n(), .groups = "drop")

# See the 'Outliers' (songs that stayed the longest)
billboard_longevity %>% arrange(desc(total_weeks))
  
  
ggplot(billboard_longevity, aes(x = total_ weeks)) + 
  geom_histogram(fill = "pink", color = "white", bandwith = 2) + 
  labs( 
    title = " how long do hits stay on the chart?"
    x = " number of weeks ", 
    y= "number of songs"
    ) + 
  theme_ minimal()

#  figure 3 
ggplot(billboard_longevity, aes(x = total_weeks)) +
  geom_histogram(fill = "pink", color = "white", binwidth = 2) +
  labs(
    title = "How Long Do #1 Hits Stay on the Charts?",
    x = "Number of Weeks",
    y = "Number of Songs"
  ) +
  theme_minimal()


#check 
billboard %>%
  count(artist, track) %>%
  filter(n >1)

# figure 4 
names(billboard)[stringr::str_detect(tolower(names(billboard)), "date|week|year")]
# FIGURE 4 — Number of #1 songs per year

billboard %>%
  mutate(year = lubridate::year(date.entered)) %>%
  filter(!is.na(year)) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  labs(
    title = "Number of Billboard #1 Songs Per Year",
    x = "Year",
    y = "Count of #1 Songs"
  )

# FIGURE 5 — Distribution of #1 Hits Per Artist

artist_counts <- billboard %>%
  count(artist)

artist_counts %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Distribution of #1 Hits Per Artist",
    x = "Number of #1 Hits",
    y = "Number of Artists"
  )






