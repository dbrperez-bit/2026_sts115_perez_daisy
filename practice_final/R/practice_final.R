#setting up 
library(rvest)
 url <- "https://www.scrapethissite.com/pages/simple/"
doc <- read_html(url) 

# extract one thing first: country names 

country_names <- html_elements(doc, "h3") |>
  html_text2()

head(country_names)
length(country_names)

#data forensics after extraction 

countries_df <- data.frame(country = country_names)

str(countries_df)
head(countries_df)
nrow(countries_df)
names(countries_df)
colSums(is.na(countries_df))

summary(df)

#scraping practice 
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

#scraping more than one field 
country_names <- html_elements(doc, "Armenia") |>
  html_text2()
capitals <- html_elements(doc, "Yerevan") |> 
  html_text()
populations <- html_elements(doc, "2968000") |>
  html_text2()
areas <- html_elements(doc, "29800.0") |> 
  html_text2()


country_names <- html_elements(doc, ".Armenia") |> html_text2()
capitals <- html_elements(doc, ".Yerevan") |> html_text2()
populations <- html_elements(doc, ".2968000") |> html_text2()
areas <- html_elements(doc, ".29800.0") |> html_text2()

#3rd attempt 
country_names <- html_elements(doc, ".country-name") |> html_text2()
capitals <- html_elements(doc, ".country-capital") |> html_text2()
populations <- html_elements(doc, ".country-population") |> html_text2()
areas <- html_elements(doc, ".country-area") |> html_text2()

head(country_names)
head(capitals)
head(populations)
head(areas)

length(country_names)
length(capitals)
length(populations)
length(areas)

#making the data frame 
countries_df <- data.frame( 
  country = country_names, 
  capital = capitals, 
  population = populations, 
  area = areas 
  )

head(countries_df) 
str(countries_df)
nrow(countries_df)
colSums(is.na(countries_df))

#cleaning the numeric columns 

countries_df <- countries_df |> 
  mutate( 
    population = as.numeric(str_replace_all(population, ",","")),
    area = as.numeric(str_replace_all(area, ",", ""))
  )
# i had forgotten  a comma one my first try 
countries_df <- countries_df |>
  mutate(
    population = as.numeric(str_replace_all(population, ",", "")),
    area = as.numeric(str_replace_all(area, ",", ""))
  )

str(countries_df)
summary(countries_df$population)
summary(countries_df$area)

#practice test for uploading into github becuase i had problems last time 

