rm(list = ls()) #-- remove object in environment
gc(full = TRUE) #-- clean memory
gc(reset = TRUE)

getwd() #-- working directory

## download files
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("data")) {
    dir.create("data")
}

if(!file.exists("data/final/en_US")) {
    tempFile <- tempfile()
    download.file(fileUrl, tempFile)
    unzip(tempFile, exdir = "data")
}



## read the blogs data
con <- file("data/final/en_US/en_US.blogs.txt", open = "r")
blogs <- readLines(con)
close(con)


## read the news data
con <- file("data/final/en_US/en_US.news.txt", open = "r")
news <- readLines(con)
close(con)

### read the tweets data
con <- file("data/final/en_US/en_US.twitter.txt", open = "r")
tweets <- readLines(con)
close(con)

library(dplyr)
library(tidyr)
library(tidytext)
library(tm)

set.seed(50000)
blogsSample <- sample(blogs, size = 50000)
newsSample <- sample(news, size = 50000)
tweetsSample <- sample(tweets, size = 50000)

saveRDS(dataSample, file = "dataSample.rds")
rm(list = ls())
gc()

txt <- readRDS("dataSample.rds")

txt <- VectorSource(txt) ##--create vector source for Copora
txt <- Corpus(txt) ##-- create Copora
txt <- tm_map(txt, content_transformer(tolower)) #- to lower case
txt <- tm_map(txt, removeNumbers) #- remove the numbers in text
txt <- tm_map(txt, removePunctuation) #- remove the punctuation
txt <- tm_map(txt, removeWords, stopwords("english")) #- remove stopword
txt <- tm_map(txt, stripWhitespace) #- remove white space


unigram <- data.frame(txt = as.character(txt)) %>% 
    unnest_tokens(output = unigram, input = txt, token = "ngrams", n= 1)
bigram <- data.frame(txt = as.character(txt)) %>% 
    unnest_tokens(output = bigram, input = txt, token = "ngrams", n = 2)
trigram <- data.frame(txt = as.character(txt)) %>% 
    unnest_tokens(output = trigram, input = txt, token = "ngrams", n= 3)
quadgram <- data.frame(txt = as.character(txt)) %>% 
    unnest_tokens(output = quadgram, input = txt, token = "ngrams", n= 4)


unigramdf <- unigram %>% count(unigram)  %>% arrange(desc(n))
bigramdf <- bigram %>% count(bigram) %>% arrange(desc(n)) %>% separate(bigram, sep = " ", into = c("word1","word2"))
trigramdf <- trigram %>% count(trigram) %>% arrange(desc(n)) %>% separate(bigram, sep = " ", into = c("word1","word2","word3"))
quadgramdf <- quadgram %>% count(quadgram) %>% arrange(desc(n))%>% separate(bigram, sep = " ", into = c("word1","word2","word3","word4"))
