
# Get more memory ---------------------------------------------------------

rm(list = ls(all.names = T))
gc(full = T, reset = T)

# Download data -----------------------------------------------------------

fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
profaneUrl <- "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
englishURL <- "https://raw.githubusercontent.com/dwyl/english-words/refs/heads/master/words_alpha.txt"
contractionURL <- "https://raw.githubusercontent.com/andrewbury/contractions/refs/heads/master/contractions.txt"


if(!file.exists("data")) {
    dir.create("data")
}

if(!file.exists("data/final/en_US")) {
    tempFile <- tempfile()
    download.file(fileUrl, tempFile)
    unzip(tempFile, exdir = "data")
}

if(!file.exists("data/final/en_US/profane.txt")) {
    download.file(profaneUrl, destfile = "data/final/en_US/profane.txt")
}

if(!file.exists("data/final/en_US/english.txt")) {
    download.file(englishURL, destfile = "data/final/en_US/english.txt")
}

if(!file.exists("data/final/en_US/contractions.txt")) {
    download.file(englishURL, destfile = "data/final/en_US/contractions.txt")
}
rm(list = ls(all.names = T))
gc()

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)

# Load data ---------------------------------------------------------------

blogs <- read_lines("data/final/en_US/en_US.blogs.txt")
blogs <- tibble(txt = blogs) %>% mutate(line = row_number(), file = "blogs")

news <- read_lines("data/final/en_US/en_US.news.txt")
news <- tibble(txt = news) %>% mutate(line = row_number(), file = "news")

tweets <- read_lines("data/final/en_US/en_US.twitter.txt")
tweets <- tibble(txt = tweets) %>% mutate(line = row_number(), file = "tweets")

bad <- read_lines("data/final/en_US/profane.txt")
bad <- tibble(word = bad)

english <- read_lines("data/final/en_US/english.txt")
english <- tibble(word = english)

contractions <- read_lines("data/final/en_US/contractions.txt")
contractions <- tibble(word = contractions)

# Sample data -------------------------------------------------------------

set.seed(20000)
text <- bind_rows(blogs, news, tweets) %>% slice_sample(n = 20000)
rm(blogs, news, tweets)
gc()

# Remove profane words and non English words ------------------------------

vocab <- bind_rows(english, contractions) %>% anti_join(bad)

# Save data ---------------------------------------------------------------

save(text, file = "dataSample.Rdata")
save(vocab, file = "chosenVocab.Rdata")

# Ngrams ------------------------------------------------------------------

## Unigram

unigram <- text %>% 
    unnest_tokens(output = ngram, input = txt, token = "ngrams", n = 1) %>%  
    semi_join(vocab, by = join_by(ngram == word)) %>% 
    count(line, ngram) %>% 
    bind_tf_idf(term = ngram, document = line, n =n) %>% 
    arrange(desc(tf_idf))
    
save(unigram, file = "unigram.RData")


## Bigram

bigram <- text %>% 
    unnest_tokens(output = ngram, input = txt, token = "ngrams", n= 2) %>% 
    count(line, ngram) %>% 
    bind_tf_idf(term = ngram, document = line, n =n) %>% 
    arrange(desc(tf_idf)) %>% 
    separate(col = ngram, 
             into = c("word1","word2"), 
             sep = " ",
             remove = F, 
             fill = "right") %>%  
    semi_join(vocab, by = join_by(word1 == word)) %>%  
    semi_join(vocab, by = join_by(word2 == word)) 
    
save(bigram, file = "bigram.RData")


## Trigram

trigram <- text %>% 
    unnest_tokens(output = ngram, input = txt, token = "ngrams", n= 3) %>% 
    count(line, ngram) %>% 
    bind_tf_idf(term = ngram, document = line, n =n) %>% 
    arrange(desc(tf_idf)) %>%
    separate(col = ngram, 
             into = c("word1","word2","word3"), 
             sep = " ",
             remove = F, 
             fill = "right") %>%  
    semi_join(vocab, by = join_by(word1 == word)) %>%  
    semi_join(vocab, by = join_by(word2 == word)) %>%  
    semi_join(vocab, by = join_by(word3 == word))  
    
save(trigram, file = "trigram.RData")


## Quadgram

quadgram <- text %>% 
    unnest_tokens(output = ngram, input = txt, token = "ngrams", n= 4) %>% 
    count(line, ngram) %>% 
    bind_tf_idf(term = ngram, document = line, n =n) %>% 
    arrange(desc(tf_idf)) %>%
    separate(col = ngram, 
             into = c("word1","word2","word3","word4"), 
             sep = " ",
             remove = F, 
             fill = "right") %>%  
    semi_join(vocab, by = join_by(word1 == word)) %>%  
    semi_join(vocab, by = join_by(word2 == word)) %>%  
    semi_join(vocab, by = join_by(word3 == word)) %>% 
    semi_join(vocab, by = join_by(word4 == word))

save(quadgram, file = "quadgram.RData")

## Pentagram

pentagram <- text %>% 
    unnest_tokens(output = ngram, input = txt, token = "ngrams", n= 5) %>% 
    count(line, ngram) %>% 
    bind_tf_idf(term = ngram, document = line, n =n) %>% 
    arrange(desc(tf_idf)) %>%
    separate(col = ngram, 
             into = c("word1","word2","word3","word4","word5"), 
             sep = " ",
             remove = F, 
             fill = "right") %>%  
    semi_join(vocab, by = join_by(word1 == word)) %>%  
    semi_join(vocab, by = join_by(word2 == word)) %>%  
    semi_join(vocab, by = join_by(word3 == word)) %>% 
    semi_join(vocab, by = join_by(word4 == word)) %>% 
    semi_join(vocab, by = join_by(word5 == word))

save(pentagram, file = "pentagram.RData")

rm(list = ls(all.names = T))
gc()
