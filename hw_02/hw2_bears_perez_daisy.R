#This loads the data 
url <- "https://raw.githubusercontent.com/the-pudding/kids-book-animals/main/static/assets/data/kids-book-animals.csv"
bears <- read_csv(url)
# shape and nature of dataset 
dim(bears)
str(bears)
bears$rating <- as.numeric(bears[[7]])
bears$reviews <- as.numeric(gsub(",", "", bears[[8]]))
names(bears)
#This is to make the visual of average review scores. 
hist(bears$rating, 
     main = "Distribution of Average Review Scores", 
     xlab = "Rating (out of 5)", 
     col = "pink",
     border = "white",
     breaks = 25)
# this make the graph for the number of reviews 
hist(bears$reviews, 
     main = "Distribution of Number of Reviews", 
     xlab = "Number of Reviews", 
     col = "pink", 
     border = "black")
#  this demonstrates the differnce between reviews and ratings data 
plot(bears$reviews, bears$rating,
     main = "Relationship: Number of Reviews vs. Average Rating",
     xlab = "Number of Reviews",
     ylab = "Average Rating",
     pch = 16, 
     col = rgb(1, 0.75, 0.8, 0.6))
# this tool/package helps identify the "popular" animals 
library(dplyr)
# First I will try to rank by ratings 
pop_by_rating <- bears %>%
  group_by(animal) %>%
  summarize(mean_score = mean(rating, na.rm = TRUE)) %>%
  arrange(desc(mean_score))
# Second I will rank by engagement
pop_by_engagement <- bears %>%
  group_by(animal) %>%
  summarize(total_reviews = sum(reviews, na.rm = TRUE)) %>%
  arrange(desc(total_reviews))
head(pop_by_rating, 3)
head(pop_by_engagement, 3)
#This helps find specfific or favorable publishers 
table(bears$publisher, bears$animal)
# question 1, is their less books being reviewed due to the internet? 
bears %>%
  group_by(decade_category) %>%
  summarize(avg_engagement = mean(reviews, na.rm = TRUE)) %>%
  arrange(decade_category) 
# For question 2 I would like to answer how being are more likley to review, either stars, or written reviews? 
#this code with compare each variable with a ratio of reviews throughout the years. 
library(dplyr)
engagement_comparison <- bears %>%
  group_by(decade_category) %>%
  summarize(
    avg_star_ratings = mean(num_ratings, na.rm = TRUE),
    avg_written_reviews = mean(num_reviews, na.rm = TRUE)
  ) %>%
  arrange(decade_category)
print(engagement_comparison)


