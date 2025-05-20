

## clean memory
rm(list = ls(all.names = TRUE))
gc(full = TRUE)
gc(full = T, reset = T)


## Read and sample data


set.seed(500)
con <- file("D:/R/capstone/projectData/en_US/en_US.blogs.txt", open = "r")
blogs <- sample(readLines(con), size = 500)
close(con)

set.seed(500)
con <- file("D:/R/capstone/projectData/en_US/en_US.news.txt", open = "r")
news <- sample(readLines(con), size = 500)
close(con)

set.seed(500)
con <- file("D:/R/capstone/projectData/en_US/en_US.twitter.txt", open = "r")
tweets <- sample(readLines(con), size = 500)
close(con)


## libraries  

library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)

## create tidy data
blogs <- tibble(data = "blogs", line = 1:length(blogs), text = blogs, length = sapply(str_split(blogs, " "), length))
news <- tibble(data = "news", line = 1:length(news), text = news, length = sapply(str_split(news, " "), length))
tweets <- tibble(data = "tweets", line = 1:length(tweets), text = tweets, length = sapply(str_split(tweets, " "), length))
sampleData <- bind_rows(blogs,news, tweets)


## clean tidy data
sampleData <- sampleData %>% 
  mutate(text = str_to_lower(text),
         text = str_remove_all(text, "[:punct:]"),
         text = str_remove_all(text, "[:digit:]"),
         text = str_squish(text)) 
data("stop_words")
stop_words <- stop_words %>% mutate(word = str_to_lower(word)) 

bigram <- sampleData %>% 
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>% 
  count(data, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, data,n) %>% 
  arrange(desc(tf_idf)) %>% 
  separate(bigram, into = c("word1","word2"), sep = " ", remove = F, fill = "right") %>% 
  anti_join(stop_words, by = join_by(word1== word)) %>% 
  anti_join(stop_words, by = join_by(word2== word)) 


predictWord <- function(word,n=1){
  word <- str_to_lower(as.character(word))
  if(word %in% bigram$word1) {
    prediction <- bigram %>% 
      filter(word1 == word) %>% 
      slice_max(order_by = tf_idf, n = n, with_ties = F) %>% 
      pull(word2)
    paste("The word",word,"could be followed by: ",paste(prediction, collapse = ","))
  } else {return(word)}
}
predictWord("city",5)


bigram %>% 
  filter(word1 == "happy") %>% 
  slice_max(order_by = tf_idf, n = 3, with_ties = FALSE) %>% 
  pull(word2)
