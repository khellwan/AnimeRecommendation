library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinythemes)
library(shinyWidgets)


server <- function(input, output) {
    # Import datasets ----
    # User data
    userData <- read.csv('./data/users.csv', sep=';', header=TRUE)
    userData$my_score = as.numeric(as.character(userData$my_score))
    userData <- userData[ which( userData$my_score > 0 & userData$my_score <= 10) , ]
    # Animes data
    animesData <- read.csv('./data/anime_cleaned.csv', sep=',', header=TRUE)
    animesData$aired_from_year = as.numeric(as.character(animesData$aired_from_year))

    # Menu bar ----
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Menu item", icon = icon("calendar"))
        )
    })
    
    # Histogram plot ----
    output$scoreHist <- renderPlot({
        data <- userData$my_score[seq_len(input$slider)]
        hist(data,
             main="Avaliações dos Usuários", 
             xlab="Avaliação",
             prob=TRUE,
             xaxt="n",
             col="blue",
             ylab="Densidade")
        axis(1, at=seq(1,10,by=1), labels=seq(1,10,by=1))
    })
    
    # Ratings per Anime plot ----
    output$ratingsPerAnime <- renderPlot({
        data <- animesData[which(animesData$scored_by <= 100000), ]
        data <- data$scored_by
        hist(data,
             main="Avaliações por Anime", 
             xlab="Avaliações",
             col="yellow",
             ylab="Frequência")
    })
    
    # Release Year plot ----
    output$releaseYear <- renderPlot({
        data <- animesData[which(animesData$aired_from_year >= 1958), ]
        data <- data$aired_from_year
        hist(data,
             main="Lançamento dos Animes por Ano", 
             xlab="Anos",
             col="red",
             ylab="Frequência")
    })
    
    # Test ----
    
   
    
}