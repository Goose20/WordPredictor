 #
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(quanteda)
library(data.table)
library(ggplot2)
library(ngram)

ngrams1<- readRDS("data/unigram.RDS")
ngrams2 <- readRDS("data/bigram.RDS")
ngrams3 <- readRDS("data/trigram.RDS")
ngrams4 <- readRDS("data/quadgram.RDS")

match <- function(statement) {
    len <- sapply(strsplit(statement, " "), length)
    statement <- tolower(statement)
    statement <- removePunctuation(statement)
    statement <- removeNumbers(statement)
    if (len > 2) {
        st <- word(statement, -3,-1)
        pred <- ngrams4 %>% filter(tok == st)
        
        if (nrow(pred) > 0){
            return(word(pred$pred)[1:3])
        }
        
        len <- 2
    }
    
    if (len == 2) {
        st <- word(statement, -2,-1)
        pred <- ngrams3 %>% filter(tok == st)
        
        if (nrow(pred) > 0){
            return(word(pred$pred)[1:3])
        }
        len <- 1
    }
    
    if (len < 2) {
        st <- word(statement, -1)
        pred <- ngrams2 %>% filter(tok == st)
        
        if (nrow(pred) > 0){
            return(word(pred$pred)[1:3])
        }
        
        return(ngrams1$tok[1:3])
    }
}

prediction <- function(st){
    
    st <- iconv(st,"latin1","ASCII",sub = " ")
    if ((st) == ""){
        out <- ngrams1$tok[1:3]
    } else {
        out <- match(st)
    }
    return(out)
}


shinyServer(function(input, output) {
    phrase <- reactive({input$phrase})
    
    output$phrase <- (phrase)
    
    
    output$pred1 <- reactive(prediction(input$phrase)[1])
    output$pred2 <- reactive(prediction(input$phrase)[2])
    output$pred3 <- reactive(prediction(input$phrase)[3])
})
