#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(tm)

ui <- fluidPage(
    
    #use of shinyjs to apply css on the app
    useShinyjs(),
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Nanum+Gothic&display=swap'); 
       
       body { background-color: #F7FDFF;}
       
       
       #subtitulo, h3, h4, h5, a, p, #texto, tabPanel, actionButton {font-family: 'Nanum Gothic';
                                               font-size: 20px;}
       h1 {font-family: 'Nanum Gothic';
                font-size: 35px;
                text-align: center;}
                   
    ")),

    
    titlePanel(h1("Text Prediction App")),
    p("This app allows you to enter words and it will show you a predicted word (the green word), depending on what you enter."),
    
 
    #sidebar with the information for the user
    sidebarLayout(
        sidebarPanel(
            div(id = "subtitulo",h2("Instructions:")), 
            h5("1. Enter a word or words in the text box, the app only accept English words."),
            h5("2. No need to hit enter of submit."),
            h5("3. If you enter a strange character, a misspelled or meaningless word, the application will let you know so that you can enter a correct word."),
            h5("4. The top ngram words tab will present you with buttons that will show you the words most frequently depending on each ngram.")
            ),
        

        #main panel with the input area for the user and the tab for the graphs
        mainPanel(
            tabsetPanel(
                tabPanel("Prediction",
                         div(id = "texto",textInput("myInput", h3("Input your words:"), 
                                   value = "Input any word you want")),
                         h3("Next predicted word:"),
                         h4(em(span(textOutput("ngramOutput"), style="color:green")))),
                
                tabPanel("Top words for prediction",
                         br(),
                         actionButton("bigram","Top 20 bigram words"),
                         actionButton("trigram","Top 20 trigram words"),
                         actionButton("quadgram","Top 20 quadgram words"),
                         actionButton("quintgram","Top 20 quintgram words"),
                         hidden(img(src = "bigrams.png", id = "bigImg", heigth = 500, width = 700)),
                         hidden(img(src = "trigrams.png", id = "triImg", heigth = 500, width = 700)),
                         hidden(img(src = "quadgrams.png" , id = "quadImg", heigth = 500, width = 700)),
                         hidden(img(src = "quintgrams.png" , id = "quintImg", heigth = 500, width = 700)))
            )   
        )
    )
)

server <- function(input, output) {
    
    #load of the ngrams
    
    biWords <- readRDS("words/biWords.rds")
    triWords  <- readRDS("words/triWords.rds")
    quadWords <- readRDS("words/quadWords.rds")
    quintWords <- readRDS("words/quintWords.rds")
    
    profanityFile <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
    con <- file(profanityFile, open = "r")
    profanityFilter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
    profanityFilter <- iconv(profanityFilter, "latin1", "ASCII", sub = "")
    close(con)
    
    
    #bigram function to get the prediction in the bigrams
    bigramWords <- function(data){
        number <- length(data)
        filter(biWords, word1 == data[number]) %>%
            top_n(1,n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word",2)) %>%
            as.character() -> predict
        ifelse(predict == "character(0)", "Enter a valid word for the prediction", return(predict)) 
    }
    
    #trigram function to get the prediction in the trigrams
    trigramWords <- function(data){
        number <- length(data)
        filter(triWords, word1 == data[number-1],
               word2 == data[number]) %>%
            top_n(1,n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word",3)) %>%
            as.character() -> predict
        ifelse(predict == "character(0)", bigramWords(data), return(predict)) 
    }
    
    #quadgram function to get the prediction in the quadgrams
    quadgramWords <- function(data){
        number <- length(data)
        filter(quadWords, word1 == data[number-2],
               word2 == data[number-1],
               word3 == data[number]) %>%
            top_n(1,n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word",4)) %>%
            as.character() -> predict
        ifelse(predict == "character(0)", trigramWords(data), return(predict)) 
    }
    
    #quintgram function to get the prediction in the quintgrams
    quintgramWords <- function(data){
        number <- length(data)
        filter(quintWords, word1 == data[number-3],
               word2 == data[number-2],
               word3 == data[number-1],
               word4 == data[number]) %>%
            top_n(1,n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word",5)) %>%
            as.character() -> predict
        ifelse(predict == "character(0)", quadgramWords(data), return(predict)) 
    }
    
    
    ngramInput <- function(data){
        
        
        #transform the data into data frame for its cleaning
        inputData <- data_frame(text = data)
        
        #cleaning of the data, removing punctuation, url, special characters, numbers, hastags,
        #mention.
        inputData <- inputData %>%
            mutate(text = str_replace_all(text, "(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
            mutate(text = str_replace_all(text, "\\S+[@]\\S+", "")) %>%
            mutate(text = str_replace_all(text, "@[^\\s]+", "")) %>%
            mutate(text = str_replace_all(text, "#[^\\s]+", "")) %>%
            mutate(text = str_replace_all(text, "[0-9](?:st|nd|rd|th)", "")) %>%
            mutate(text = str_replace_all(text, "[^\\p{L}'\\s]+", "")) %>%
            mutate(text = str_replace_all(text, "[.\\-!]", "")) %>%
            mutate(text = str_replace_all(text, "^\\s+|\\s+$", ""))
        inputData <- tolower(inputData)
        
        
        #get the number of words to decide what fuction it should call
        countWords <- str_count(inputData, boundary("word"))
        words <- unlist(str_split(inputData,boundary("word")))
        
        predictedWord <- ifelse(countWords == 0, "Enter a valid word for the prediction",  
                                ifelse(countWords == 1,bigramWords(words),
                                ifelse(countWords == 2, trigramWords(words),
                                       ifelse(countWords == 3, quadgramWords(words), quintgramWords(words)))))
        
        return(predictedWord)
        
    }
    
    
    #output predicted word
    output$ngramOutput <- renderText({
        
        ifelse((input$myInput == "" || input$myInput == "Input any word you want"), "", ngramInput(input$myInput))
        
    })
    
    #functions to hide and show the graph in their tab
    observeEvent(input$bigram, {
        hide("triImg")
        hide("quadImg")
        hide("quintImg")
        show("bigImg")
    })
    
    observeEvent(input$trigram, {
        hide("bigImg")
        hide("quadImg")
        hide("quintImg")
        show("triImg")
    })
    
    observeEvent(input$quadgram, {
        hide("bigImg")
        hide("triImg")
        hide("quintImg")
        show("quadImg")
    })
    
    observeEvent(input$quintgram, {
        hide("bigImg")
        hide("triImg")
        hide("quadImg")
        show("quintImg")
    })
    
}

 
shinyApp(ui = ui, server = server)