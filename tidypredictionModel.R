

## clean memory
rm(list = ls(all.names = TRUE))
gc(full = TRUE)
gc(full = T, reset = T)


## Read and sample data


set.seed(50000)
con <- file("data/final/en_US/en_US.blogs.txt", open = "r")
blogs <- sample(readLines(con), size = 50000)
close(con)

set.seed(50000)
con <- file("data/final/en_US/en_US.news.txt", open = "r")
news <- sample(readLines(con), size = 50000)
close(con)

set.seed(50000)
con <- file("data/final/en_US/en_US.twitter.txt", open = "r")
tweets <- sample(readLines(con), size = 50000)
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


trigram <- sampleData %>% 
    unnest_tokens(output = trigram, input = text, token = "ngrams", n = 3) %>% 
    count(data, trigram, sort = TRUE) %>% 
    bind_tf_idf(trigram, data,n) %>% 
    arrange(desc(tf_idf)) %>% 
    separate(trigram, into = c("word1","word2","word3"), sep = " ", remove = F, fill = "right") %>% 
    anti_join(stop_words, by = join_by(word1 == word)) %>% 
    anti_join(stop_words, by = join_by(word2 == word)) %>% 
    anti_join(stop_words, by = join_by(word3 == word))


predictWord <- function(word,n=1){
  word <- str_to_lower(as.character(word))
  word <- str_extract(word, "[a-z]+$")
  if(word %in% trigram$word1) {
          prediction <- trigram %>% 
              filter(word1 == word) %>% 
              slice_max(order_by = tf_idf, n = n, with_ties = F) %>% 
              pull(word2)
          paste("Your word",word, "could be followed by: ",paste(prediction, collapse = ","))
      } else if(word %in% trigram$word2) {
          prediction <- trigram %>% 
              filter(word2 == word) %>% 
              slice_max(order_by = tf_idf, n = n, with_ties = F) %>% 
              pull(word3)
          paste("Your word",word, "could be followed by: ",paste(prediction, collapse = ","))
      } else return(word)
}

head(sentences)
predictWord("bought", n = 5)

