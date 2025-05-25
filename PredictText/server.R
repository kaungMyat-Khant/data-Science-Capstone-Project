#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

function(input, output) {
    
    ## load ngrams
    bigram <- readRDS("bigram.rds")
    trigram <- readRDS("trigram.rds")
    quadgram <- readRDS("quadgram.rds")
    pentagram <- readRDS("pentagram.rds")
    vocab <- readRDS("chosenVocab.rds")
    
    
    ## libraries to use
    library(dplyr)
    library(ggplot2)
    library(tidytext)
   
    
    ## clean text input
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
    
    ## predict the coming words
    predictWords <- function(input, n = c(1,2,3,4)){
        if(n == 1) {
            input <- cleanInput(input)
            output <- bigram %>% 
                filter(word1 == input) %>% 
                slice_max(n = 5, order_by = tf_idf, with_ties = F) %>% 
                select(word2) %>% 
                mutate(predict = paste(word2)) %>% 
                pull(predict) 
            return(output)
        }
        
        if(n == 2) {
            input <- cleanInput(input)
            output <- trigram %>% 
                filter(word1 == input) %>% 
                slice_max(n = 5, order_by = tf_idf, with_ties = F) %>% 
                select(word2,word3) %>% 
                mutate(predict = paste(word2,word3)) %>% 
                pull(predict) 
            return(output)
        }
        
        if(n == 3) {
            input <- cleanInput(input)
            output <- quadgram %>% 
                filter(word1 == input) %>% 
                slice_max(n = 5, order_by = tf_idf, with_ties = F) %>% 
                select(word2,word3,word4) %>% 
                mutate(predict = paste(word2,word3,word4)) %>% 
                pull(predict) 
            return(output)
        }
        
        if(n == 4) {
            input <- cleanInput(input)
            output <- pentagram %>% 
                filter(word1 == input) %>% 
                slice_max(n = 5, order_by = tf_idf, with_ties = F) %>% 
                select(word2,word3,word4,word5) %>% 
                mutate(predict = paste(word2,word3,word4,word5)) %>% 
                pull(predict) 
            return(output)
        }
    }  
    
    ## generate table
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
            slice_max(n = 5, order_by = tf_idf, with_ties = F) %>% 
            select(ngram, n, tf_idf) %>% 
            rename(Phrase = ngram, Frequency = n, Importance = tf_idf)
    }
    
    ## create plot
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
            slice_max(order_by = tf_idf, n = 5, with_ties = F) %>% 
            ggplot(aes(x = reorder(ngram,tf_idf), y = tf_idf))+
            geom_col(fill = "#2C0C18")+
            theme_minimal()+
            labs(title = "",
                 x = "Word chunks",
                 y = "Word Importance Score (TF-IDF)")+
            coord_flip()
    }  
    
    output$textOutput <- renderText({
        txt <- predictWords(input = input$textInput,n = input$ngramInput)
        paste(txt, collapse = ",")
    })
    
    output$tableOutput <- renderTable({
        generateTable(input = input$textInput, n = input$ngramInput)
    })
    
    output$plotOutput <- renderPlot({
        creatPlot(input = input$textInput, n = input$ngramInput)
    })
}