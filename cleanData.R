
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


## File size 
Size <- c(blogs = format(object.size(blogs), units = "Mb"),
  news = format(object.size(news), units = "Mb"),
  tweets = format(object.size(tweets), units = "Mb"))

## Number of lines
Lines <- c(blogs = length(blogs),
           news = length(news),
           tweets = length(tweets))

## Number of characters
Characters <- c(blogs = sum(sapply(blogs, nchar)),
                news = sum(sapply(news, nchar)),
                tweets = sum(sapply(blogs, nchar)))

## Numbers of words
wordCount <- function(x) {
  length(unlist(strsplit(x, split = "[:space:]")))
}
Words <- c(blogs = wordCount(blogs),
           news = wordCount(news),
           tweets = wordCount(tweets))

desdf <- data.frame(Lines, Characters,Words,Size)
desdf


