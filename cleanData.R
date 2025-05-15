

# Get data -----------------------------------------------------------

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


# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Summarize data ------------------------------------------------------------

## File size 
Size <- c(blogs = format(object.size(blogs), units = "Mb"),
  news = format(object.size(news), units = "Mb"),
  tweets = format(object.size(tweets), units = "Mb"))
gc()

## Number of lines
Lines <- c(blogs = length(blogs),
           news = length(news),
           tweets = length(tweets))
gc()


## Number of characters
Characters <- c(blogs = sum(sapply(blogs, nchar)),
                news = sum(sapply(news, nchar)),
                tweets = sum(sapply(blogs, nchar)))
gc()

## Numbers of words
wordCount <- function(x) {
  length(unlist(strsplit(x, split = " ")))
}
Words <- c(blogs = wordCount(blogs),
           news = wordCount(news),
           tweets = wordCount(tweets))

desdf <- data.frame(Lines, Characters,Words,Size)
desdf
gc()

## Words per lines
wordPerLine <- function(x) {
    sapply(strsplit(x, split = " "), length)
}


blogs_wpl <-  wordPerLine(blogs)
news_wpl <-  wordPerLine(news)
tweets_wpl <- wordPerLine(tweets)

blogHist <- ggplot(data = data.frame(wpl=blogs_wpl), aes(wpl))+ 
    geom_histogram(fill = "navy", binwidth = 1)+
    labs(x = "Number of Words per line",
         title = "File: blogs")+
    theme_bw()

newsHist <- ggplot(data = data.frame(wpl=news_wpl), aes(wpl))+ 
    geom_histogram(fill="orange", binwidth = 1)+
    labs(x = "Number of Words per line",
         title = "File: news")+
    theme_bw()

tweetsHist <- ggplot(data = data.frame(wpl=tweets_wpl), aes(wpl))+ 
    geom_histogram(fill="skyblue", binwidth = 1)+
    labs(x = "Number of Words per line",
         title = "File: tweets")+
    theme_bw()

allHist <- ggplot(data = data.frame(wpl=c(blogs_wpl,news_wpl,tweets_wpl)), 
                    aes(wpl))+ 
    geom_histogram(fill="skyblue", binwidth = 1)+
    labs(x = "Number of Words per line",
         title = "File: all")+
    theme_bw()

cowplot::plot_grid(blogHist, newsHist, tweetsHist, allHist)

max <- max(length(blogs_wpl),length(news_wpl), length(tweets_wpl))
length(blogs_wpl) <- max
length(news_wpl) <- max
length(tweets_wpl) <- max

words_per_line <- data.frame(blogs = blogs_wpl,
                             news = news_wpl,
                             tweets = tweets_wpl)
rm("max")
gc()



words_per_line %>% 
    summarise(across(everything(), list(Mean = mean, SD = sd, 
                                        Median = median, IQR = IQR,
                                        Max = max, Min = min), 
                     na.rm = T)) %>%
    mutate(across(c(ends_with("Mean"), ends_with("SD")), round)) %>% 
    pivot_longer(
    cols = everything(),
    names_to = c("File", "statistic"),
    names_sep = "_",
    values_to = "value") %>%
    pivot_wider(names_from = statistic, values_from = value)



# Exploratory data analysis of word occurrence ----------------------------


## subset 5% of each text as sample
set.seed(5000)
blogsSample <- sample(blogs, size = 5000)
newsSample <- sample(news, size = 5000)
tweetsSample <- sample(tweets, size = 5000)
dataSample <- c(blogsSample, newsSample, tweetsSample)

## clear memory
saveRDS(dataSample, file = "dataSample.rds")
rm(list = ls())
gc()

## libraries for EDA
library(tm)
library(wordcloud)
library(tidytext)


## Read the sample data
txt <- readRDS("dataSample.rds")

## Preprocess data

#### clean the text
txt <- VectorSource(txt) ##--create vector source for Copora
txt <- Corpus(txt) ##-- create Copora
txt <- tm_map(txt, content_transformer(tolower)) #- to lower case
txt <- tm_map(txt, removeNumbers) #- remove the numbers in text
txt <- tm_map(txt, removePunctuation) #- remove the punctuation
txt <- tm_map(txt, removeWords, stopwords("english")) #- remove stopword
# txt <- tm_map(txt, removeSparseTerms, 0.2)
txt <- tm_map(txt, stripWhitespace) #- remove white space

#### Explore the text
##### Frequent words
doc.matrix <- TermDocumentMatrix(txt) #- term document matrix created
m <- as.matrix(doc.matrix) #- convert to matirx
v <- sort(rowSums(m),decreasing = T); v #- convert to vector of word and their frequency

df <- data.frame(word = names(v), freq = v) #- word cloud data created


head(df)
wordcloud(words = df$word, freq = df$freq,
          min.freq = 1, max.words = 200,
          random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, "BuPu"))

df %>% 
  arrange(desc(freq)) %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(reorder(word,-freq), freq))+
  geom_col(fill = "purple4")+
  labs(x ="Words", 
       y = "Frequency", 
       title = "Top 10 most frequent words in sample text")+
  theme_bw()


##### Bi grams
library(tidytext)

bigram <- data.frame(txt = as.character(txt)) %>% 
    unnest_tokens(output = bigram, input = txt, token = "ngrams", n=2)
bigram %>% 
    count(bigram, sort = T) %>% 
    slice_head(n = 20) %>% 
    ggplot(aes(reorder(bigram,-n),n))+
    geom_col(fill = "steelblue")+
    labs(x = "Bigrams", y = "Frequency")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90))

saveRDS(bigram, file = "bigram.RDS")
bigram_prob <- bigram %>% 
    count(bigram, sort = T) %>% 
    separate(bigram, sep = " ", into = c("word1","word2")) %>% 
    mutate(prob = n/sum(n))

predictNext <- function(word) {
    library(dplyr)
    word <- as.character(word)
    if(word %in% bigram_prob$word1) {
        bigram_prob %>% 
            filter(word1 == word) %>% 
            arrange(desc(prob)) %>%
            slice_head(n=1) %>% 
            pull(word2)
    } else print("Word not found in bigram")
    
}    
predictNext("sex")
