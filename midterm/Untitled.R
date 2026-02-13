library(readr)
unzip("rail_accidents.csv")
library(tidyverse)
accidents <- read_csv ("desktop/2026_sts115_perez_daisy/data/rail_accidents.csv")

df <- read_csv("rail_acidents.csv")

accidents <- read_csv("data/rail_accidents.csv")

library(readr)

raildata <- read_csv("rail_accidents.csv")

head(df)
glimpse(df)
names(df)
summary(df)
nrow(df)
ncol(df)

#question 
# explore structure
str(raildata)

# preview data
head(raildata)
tail(raildata)

# dimensions
dim(raildata)
nrow(raildata)
ncol(raildata)


# variable names
names(raildata)

# summary statistics
summary(raildata)

# missing values
sum(is.na(raildata))


str(raildata)


names(raildata)




