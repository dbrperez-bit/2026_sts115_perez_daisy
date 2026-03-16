setwd("~/Desktop/2026_sts115_perez_daisy")

install.packages("rvest")
install.packages("stringr")

library(rvest)
library(stringr)

html = r"(<html>
  <head> 
   <title> This is the page title</title>
  </head>
  <body>
   <h1>This is my header</h1>
   <p>This is a paragraph</p>
       <a href= "https://google.com"> Go to Google!</a>
     </p>
    <p id="hello">Hello, I'm happy you are here. </p>
  </body>
</html>)"

class(html)

doc <- read_html(html)
class(doc)

tags = html_children(doc)

length(tags)

head = tags [1]
body = tags [2]

title = html_children(head)
html_text(title)

cat(html_text(title))



html_elements(doc, xpath = '/html/body/p')

paragraphs <- html_elements(doc, xpath = '//p')

html_elements(doc, xpath = '//p/a')

html_elements(doc, xpath = '//h1|//p')

html_elements(doc, xpath = "//*[@id = 'hello']")

#for real scrapper, all of our other code is called 
#within this loop 
for (i in 1:569) { 
  url = paste0("https://ebba.english.ucsb.edu/search_combined/?ft=king&p=569", i) 
  print(url)
  }

# code will run for today 

#set the the url 
url = "https://ebba.english.ucsb.edu/search_combined/?ft=king&p=1"
ballards_doc <- read_html(url)

#load the url into an xml doc 
ballads_doc <- read_html(url)

#get the ids 
ids_element <- html_elements(ballards_doc, xpath ="//span[@class = 'ebba-id w3-hide-small']")

ids_element[1]
ids <- html_text(ids_element)

ids <-as.numeric(gsub("[^0-9]", "", ids))

#get titles 
title_elements <- html_elements(ballards_doc, xpath = "//div[@class = 'title']")
titles <- html_text(title_elements)

# get holding libraray 
library_element <- html_elements(ballads_doc, xpath = "//div[@class = 'collection w3-cell w3-left']")
library <- html_text(library_element)
library <- gsub ("•", "", library)
library <- gsub ("[0-9]", "", library)
library <- trimws(library)

#get ballad links 
links_elements <- html_elements(ballads_doc, xpath = "//div[@class = 'title']/a")
links <- html_attr(links_elements, "href")

#package as a data. frame 
ballads_df <- data.frame(
  id = ids, 
  title = titles, 
  library = library, 
  link = links,
  stringsAsFactors = FALSE
)
str(ballads_df)

#add column for cleaned titles 
ballads_df$titleCleaned <- ballads_df$title

#clean the titles 
ballads_df$titleCleaned <- gsub("/", " ", ballads_df$titleCleaned)
ballads_df$titleCleaned <- trimws(ballads_df$titleCleaned)
view(ballads_df)

#fix links 
ballads_df$link <- paste0("https://ebba.english.ucsb.edu", ballads_df$link)
citation_links <- paste0("https://ebba.english.ucsb.edu/ballad/",ballads_df$link)

cd ~/projects/daisy_first-shared_repo
git remote -v




