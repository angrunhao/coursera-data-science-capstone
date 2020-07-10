#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tm)
library(tokenizers)

bigram <- read.csv("data/Nbigram.csv")
trigram <- read.csv("data/Ntrigram.csv")

predictor <- function(input, bigram, trigram) {
    returnLastWord <- function(stringText) {
        wordList <- strsplit(as.String(stringText), " ")[[1]]
        wordList[length(wordList)]
    }
    
    searcher <- function(before, ngram) {
        after <- subset(ngram, str_detect(ngram$word, fixed(paste(before, "", sep = " "))))
        after <- arrange(after, -frequency)[1:3,]
        sapply(after$word, returnLastWord)
    }
    input <- tolower(input)
    inputSplit <- strsplit(as.String(input), " ")[[1]]
    
    if (length(inputSplit) == 1) {
        result <- searcher(inputSplit[length(inputSplit)], bigram)
        
    } else {
        result <- searcher(paste(inputSplit[length(inputSplit)-1], 
                                 inputSplit[length(inputSplit)], sep = " "), trigram)
        if (result[1] == "NA") {
            result <- searcher(inputSplit[length(inputSplit)], bigram)
        }
        
    }
    result
}

checkNA <- function(textToCheck) {
    if (is.na(textToCheck)) {
        return("NA")
    } else {
        return(textToCheck)
    }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    result <- reactive ({
        current <- input$current
        if (current == "") {
            "Please type something"
        } else {
            predictions <- predictor(input = current, bigram = bigram, trigram = trigram)
            paste("1.", predictions[1], "\n2.", predictions[2], "\n3.", predictions[3], sep = " ")
        }
        
    })
    
    output$value <- renderText ({
        input$current  
    })
    
    output$suggestions <- renderText({
        result()
    })
        
    
})
