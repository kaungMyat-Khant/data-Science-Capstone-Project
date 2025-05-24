
# Load ngrams -------------------------------------------------------------

load("bigram.RData")
load("trigram.RData")
load("quadgram.RData")
load("pentagram.RData")
load("chosenVocab.RData")


# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidytext)
library(knitr)

# Test words --------------------------------------------------------------

test <- sample(sentences, size = 1)
test

# Clean text input --------------------------------------------------------

cleanInput <- function(text) {
    text <- as.character(text)
    text <- tibble(text = text)
    text <- text %>% 
        unnest_tokens(output = word, input = text, token = "ngrams",n=1) %>% 
        semi_join(vocab,by = join_by(word)) %>% 
        slice_tail() %>% 
        pull(word)
    return(text)
}

cleanInput(test)
cleanInput("The knife was hung inside")


# Predict words -----------------------------------------------------------

predictWords <- function(input, n = c(1,2,3,4)){
    if(n == 1) {
        input <- cleanInput(input)
        output <- bigram %>% 
            filter(word1 == input) %>% 
            slice_max(n = 5, order_by = tf_idf, with_ties = T) %>% 
            select(word2) %>% 
            mutate(predict = paste(word2)) %>% 
            select(predict) %>% slice_head(n =5)
        return(output)
    }
    
    if(n == 2) {
        input <- cleanInput(input)
        output <- trigram %>% 
            filter(word1 == input) %>% 
            slice_max(n = 5, order_by = tf_idf, with_ties = T) %>% 
            select(word2,word3) %>% 
            mutate(predict = paste(word2,word3)) %>% 
            select(predict) %>% slice_head(n =5)
        return(output)
    }
    
    if(n == 3) {
        input <- cleanInput(input)
        output <- quadgram %>% 
            filter(word1 == input) %>% 
            slice_max(n = 5, order_by = tf_idf, with_ties = T) %>% 
            select(word2,word3,word4) %>% 
            mutate(predict = paste(word2,word3,word4)) %>% 
            select(predict) %>% slice_head(n =5)
        return(output)
    }
    
    if(n == 4) {
        input <- cleanInput(input)
        output <- pentagram %>% 
            filter(word1 == input) %>% 
            slice_max(n = 5, order_by = tf_idf, with_ties = T) %>% 
            select(word2,word3,word4,word5) %>% 
            mutate(predict = paste(word2,word3,word4,word5)) %>% 
            select(predict) %>% slice_head(n =5)
        return(output)
    }
    
    if(n>4 | n<1) {
        stop("This model can predict only 1 to 4 words")
    }
}

test
predictWords(input = test, n = 2)
predictWords("Happy", n = 3)


# Generate table ----------------------------------------------------------

generateTable <- function(input, n = c(1,2,3,4)) {
    input <- cleanInput(input)
    if(n == 1) {
        df <- bigram
    } else if(n == 2) {
        df <- trigram
    } else if(n == 3) {
        df <- quadgram
    } else if(n == 4) {
        df <- pentagram
    } else {
        stop("This model can predict only 1 to 4 words")
    }
    
    df %>% 
        filter(word1 == input) %>% 
        slice_max(n = 5, order_by = tf_idf, with_ties = T) %>% 
        select(ngram, n, tf_idf) %>% 
        slice_head(n = 5) %>% 
        kable(col.names = c("Word chunk","Frequency","Importance(TF-IDF score)"),
              row.names = F, align = c("l","r","r"), digits = 2)
}

generateTable(input = test, n = 4)
generateTable(input = "Last", n = 4)

# Plotting ----------------------------------------------------------------

creatPlot <- function(input, n = c(1,2,3,4)) {
    input <- cleanInput(input)
    if(n == 1) {
        df <- bigram
    } else if(n == 2) {
        df <- trigram
    } else if(n == 3) {
        df <- quadgram
    } else if(n == 4) {
        df <- pentagram
    } else {
        stop("This model can predict only 1 to 4 words")
    }
    
    df %>% 
        filter(word1 == input) %>% 
        mutate(tf_idf = round(tf_idf,2)) %>% 
        slice_max(order_by = tf_idf, n = 5) %>% 
        slice_head(n = 5) %>% 
        ggplot(aes(x = reorder(ngram,tf_idf), y = tf_idf))+
        geom_col(fill = "#2C0C18")+
        geom_text(aes(label = tf_idf), hjust = 1, color = "white")+
        theme_minimal()+
        labs(title = "Top 5 important word chunks",
             x = "Word chunks",
             y = "Word Importance Score (TF-IDF)")+
        coord_flip()+
        theme(axis.text.x = element_blank())
}

creatPlot(input = test, n = 2)
creatPlot(input = "Happy", n = 1)


# Test functions' efficiency ----------------------------------------------

## Run time
library(microbenchmark)
microbenchmark(cleanInput(test))
microbenchmark(predictWords(input = test, n = 4))
microbenchmark(generateTable(input = test, n = 4))
microbenchmark(creatPlot(input = test, n = 4))
##' Functions take less than 1 second and efficient

## RAM used
library(pryr)

mem_before <- mem_used()
cleanInput(test)
mem_after <- mem_used()
print(mem_after - mem_before)  

mem_before <- mem_used()
predictWords(input = test, n = 4)
mem_after <- mem_used()
print(mem_after - mem_before)  

mem_before <- mem_used()
generateTable(input = test, n = 4)
mem_after <- mem_used()
print(mem_after - mem_before)  

mem_before <- mem_used()
creatPlot(input = test, n = 4)
mem_after <- mem_used()
print(mem_after - mem_before)  

mem_before <- mem_used()
load("bigram.RData")
load("trigram.RData")
load("quadgram.RData")
load("pentagram.RData")
load("chosenVocab.RData")
library(dplyr)
library(ggplot2)
library(tidytext)
library(knitr)
predictWords(input = "A quick brown fox", n = 4)
generateTable(input = "A quick brown fox", n = 4)
creatPlot(input = "A quick brown fox", n = 4)
mem_after <- mem_used()
print(mem_after - mem_before)  

##' Around 200MB of RAM Usage