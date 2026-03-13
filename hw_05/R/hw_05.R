
#Female Nobel Laureates From Wikipedia 
# Web scraping with two approaches: 
#1.Wikipedia API 
#2 Direct Web Scraping 

#1 Load packages 
library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
page_title <- "List_of_female_Nobel_laureates"
page_url <- "https://en.wikipedia.org/wiki/List_of_female_Nobel_laureates"

prize_categories <- c( 
  "Physiology or Medicine",
  "Physics",
  "Chemistry",
  "Literature",
  "Peace",
  "Economic Sciences"
  )
dir.create("data", showWarnings = FALSE)

# make a helper function to clean the nobel tables, this will help identify and scrape the required varibles like full name, age, country and prize category 

clean_nobel_tables <- function(table_list, prize_categories) {
  all_tables <- vector("list", length = 6)
  for (i in 1:6) {
    x <- table_list[[i]]
    x <- x [, c("Year", "Name","Born")]
    names(x) <- c("year", "full_name", "born")
    
    x$prize_category <- prize_categories[i]
    
    all_tables[[i]] <- x 
      }

df <- bind_rows(all_tables)

df$fill_name <-str_squish(df$full_name)
df$born <- str_squish(df$born)
df$year <- as.integer(df$year)

df$birth_date_text <- str_extract(df$born, "^[0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4}")
df$birth_date <- dmy(df$birth_date_text)

df$award_date <- dmy(paste("10 December", df$year))
df$age <- floor(as.numeric(df$award_date - df$birth_date) / 365.25)

df$born_place <- str_remove(df$born, "^[0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4}\\s*")
df$born_place <- str_remove_all(df$born_place, "\\([^\\)]*\\)")
df$born_place <- str_squish(df$born_place)

df$country <- str_extract(df$born_place, "[^,]+$")
df$country <- str_remove(df$country, "\\.$")
df$country <- str_squish(df$country)

df <- df[, c("full_name", "year", "age", "country", "prize_category")]

df <- distinct(df)

return(df)
}
#3 Using Approach #1 to get data using wikipedia API, this section is about uses the Wikipedia API and making the result/output into HTML tables then cleaning them 

api_url <- paste0(
  "https://en.wikipedia.org/w/api.php?",
  "action=parse",
  "&page=", page_title,
  "&prop=text",
  "&format=json"
)
response <- GET(api_url)

response$status_code

bdy <- content(response, "text")
bdy_json <- fromJSON(bdy)
api_html <- bdy_json$parse$text$`*`
api_doc <- read_html(api_html)

api_tables_nodes <- html_elements(api_doc, xpath = "//table[contains(@class, 'wikitable')]")
length(api_tables_nodes)

api_tables <- html_table(api_tables_nodes, fill = TRUE)
clean_nobel_tables <- function(table_list, prize_categories) {

female_nobel_api <- clean_nobel_tables(api_tables, prize_categories)

clean_nobel_tables <- function(table_list, prize_categories) {
  
  all_tables <- vector("list", length = 6)
  
  for (i in 1:6) {
    x <- table_list[[i]]
    x <- x[, c("Year", "Name", "Born")]
    names(x) <- c("year", "full_name", "born")
    x$prize_category <- prize_categories[i]
    all_tables[[i]] <- x
  }
  
  df <- bind_rows(all_tables)
  
  df$full_name <- str_squish(df$full_name)
  df$born <- str_squish(df$born)
  df$year <- as.integer(df$year)
  
  df$birth_date_text <- str_extract(df$born, "^[0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4}")
  df$birth_date <- dmy(df$birth_date_text)
  
  df$award_date <- dmy(paste("10 December", df$year))
  df$age <- floor(as.numeric(df$award_date - df$birth_date) / 365.25)
  
  df$born_place <- str_remove(df$born, "^[0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4}\\s*")
  df$born_place <- str_remove_all(df$born_place, "\\([^\\)]*\\)")
  df$born_place <- str_squish(df$born_place)
  
  df$country <- str_extract(df$born_place, "[^,]+$")
  df$country <- str_remove(df$country, "\\.$")
  df$country <- str_squish(df$country)
  
  df <- df[, c("full_name", "year", "age", "country", "prize_category")]
  df <- distinct(df)
  
  return(df)
}

exists("clean_nobel_tables")
female_nobel_api <- clean_nobel_tables(api_tables, prize_categories)
head(female_nobel_api)
dim(female_nobel_api)

write.csv(female_nobel_api, "data/female_nobel_api.csv", row.names = FALSE)

#Taking appraoch #2 to scrape the webpage directely, this will read the live website and take out the HTML tables without using API 

wiki_doc <- read_html(page_url)

wiki_tables_nodes <- html_elements(
  wiki_doc,
  xpath = "//table[contains(@class, 'wikitable')]"
)

length(wiki_tables_nodes)

wiki_tables <- html_table(wiki_tables_nodes, fill = TRUE)

female_nobel_scrape <- clean_nobel_tables(wiki_tables, prize_categories)

head(female_nobel_scrape)
dim(female_nobel_scrape)

write.csv(female_nobel_scrape, "data/female_nobel_scrape.csv", row.names = FALSE)

#5.Verifying and comparing both datasets for accuracy, using summary command for identfying missing values, and if two appraches yielded the same result. 

str(female_nobel_api)
str(female_nobel_scrape)

summary(female_nobel_api$year)
summary(female_nobel_api$age)

summary(female_nobel_scrape$year)
summary(female_nobel_scrape$age)

table(female_nobel_api$prize_category)
table(female_nobel_scrape$prize_category)

colSums(is.na(female_nobel_api))
colSums(is.na(female_nobel_scrape))

#comparing two datasets 
anti_join(
  female_nobel_api,
  female_nobel_scrape,
  by = c("full_name", "year", "age", "country", "prize_category")
)

anti_join(
  female_nobel_scrape,
  female_nobel_api,
  by = c("full_name", "year", "age", "country", "prize_category")
)

#6. Make visuals with clean data and variables, these help further clarify the accuracy of the data, distribution and award years for each dataset. 

ggplot(female_nobel_api, aes(x = prize_category)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Female Nobel Laureates by Prize Category (API)",
    x = "Prize Category",
    y = "Count"
  )

ggplot(female_nobel_api, aes(x = year)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Female Nobel Laureates by Award Year (API)",
    x = "Year",
    y = "Count"
  )

ggplot(female_nobel_scrape, aes(x = prize_category)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Female Nobel Laureates by Prize Category (Scrape)",
    x = "Prize Category",
    y = "Count"
  )

ggplot(female_nobel_scrape, aes(x = year)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Female Nobel Laureates by Award Year (Scrape)",
    x = "Year",
    y = "Count"
  )

dir.create("data", showWarnings = FALSE)

write.csv(female_nobel_api, "data/female_nobel_api.csv", row.names = FALSE)
write.csv(female_nobel_scrape, "data/female_nobel_scrape.csv", row.names = FALSE)

getwd()





