library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinythemes)
library(shinyWidgets)
library(qlcMatrix)

getAnimeFromName <- function(animeData, animeName){
  return(animeData[which(animeData$title==animeName),])
}


server <- function(input, output, session) {
    # Import datasets ----
    # User data
    userData <- read.csv('./data/users.csv', sep=';', header=TRUE)
    userData$my_score = as.numeric(as.character(userData$my_score))
    userData <- userData[ which( userData$my_score > 0 & userData$my_score <= 10) , ]
    # Animes data
    animesData <- read.csv('./data/anime_cleaned.csv', sep=',', header=TRUE)
    animesData$aired_from_year = as.numeric(as.character(animesData$aired_from_year))

    #Anime x User rating table
    userAnime <- read.table(file="./data/userAnime.csv",row.names=1,sep=",",header=TRUE)
    #Normalized by mean
    userAnimeNorm <- userAnime
    for(i in 1:ncol(userAnimeNorm)){
        colMean <- mean(userAnimeNorm[,i],na.rm=TRUE)
        userAnimeNorm[,i] <- userAnimeNorm[,i]-colMean
        userAnimeNorm[,i][is.na(userAnimeNorm[,i])]=0
        if(i %% 50==0){
            print(i)
        }
    }
    
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
             xlab="Número de Avaliações",
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
    
    # Render movie title table
    df <- data.frame(animesData$title)
    colnames(df) <- c("Animes")
    output$anime_table = DT::renderDataTable({
        df
        
    })
    
    # ----  Input stuff ----
    
    # Creates vector of user selected animes
    anime_vec = reactive({
        return(c(input$anime1, input$anime2, input$anime3, input$anime4, input$anime5))
    })
    
    # Creates vector of user selected ratings
    score_vec = reactive({
        return(c(as.numeric(input$rating1)/10, as.numeric(input$rating2)/10, as.numeric(input$rating3)/10, as.numeric(input$rating4)/10, as.numeric(input$rating5)/10))
    })
    
    # Creates a proxy table for clearing rows
    proxy = dataTableProxy('anime_table')
    
    # Initialize equivalent variables in UI for anime title
    output$anime1 = renderText({ input$anime1 })
    output$anime2 = renderText({ input$anime2 })
    output$anime3 = renderText({ input$anime3 })
    output$anime4 = renderText({ input$anime4 })
    output$anime5 = renderText({ input$anime5 })
    
    # Allows user to clear selected rows from the anime table. Also resets
    # textInput box text to default
    observeEvent(input$clearRows, {
        proxy %>% selectRows(NULL)
        updateTextInput(session, 'anime1', value = '---Primeiro Anime---')
        updateTextInput(session, 'anime2', value = '---Segundo Anime---')
        updateTextInput(session, 'anime3', value = '---Terceiro Anime---')
        updateTextInput(session, 'anime4', value = '---Quarto Anime---')
        updateTextInput(session, 'anime5', value = '---Quinto Anime---')
    })
    
    # Allows user to reset selected ratings to default
    observeEvent(input$clearRating, {
        updatePrettyRadioButtons(session, 'rating1', selected = 0)
        updatePrettyRadioButtons(session, 'rating2', selected = 0)
        updatePrettyRadioButtons(session, 'rating3', selected = 0)
        updatePrettyRadioButtons(session, 'rating4', selected = 0)
        updatePrettyRadioButtons(session, 'rating5', selected = 0)
    })
    
    # Updates movie selection based on data table rows selected
    observeEvent(input$anime_table_rows_selected, {
        rows = input$anime_table_rows_selected
        if (length(rows) == 1){
            updateTextInput(session, 'anime1', value = animesData$title[rows[1]])
        }
        
        else if (length(rows) == 2){
            updateTextInput(session, 'anime1', value = animesData$title[rows[1]])
            updateTextInput(session, 'anime2', value = animesData$title[rows[2]])
        }
        
        else if (length(rows) == 3){
            updateTextInput(session, 'anime1', value = animesData$title[rows[1]])
            updateTextInput(session, 'anime2', value = animesData$title[rows[2]])
            updateTextInput(session, 'anime3', value = animesData$title[rows[3]])
        }
        
        else if (length(rows) == 4){
            updateTextInput(session, 'anime1', value = animesData$title[rows[1]])
            updateTextInput(session, 'anime2', value = animesData$title[rows[2]])
            updateTextInput(session, 'anime3', value = animesData$title[rows[3]])
            updateTextInput(session, 'anime4', value = animesData$title[rows[4]])
        }
        
        else if (length(rows) == 5){
            updateTextInput(session, 'anime1', value = animesData$title[rows[1]])
            updateTextInput(session, 'anime2', value = animesData$title[rows[2]])
            updateTextInput(session, 'anime3', value = animesData$title[rows[3]])
            updateTextInput(session, 'anime4', value = animesData$title[rows[4]])
            updateTextInput(session, 'anime5', value = animesData$title[rows[5]])
        }
    })
    
    # Checks if user anime selections are valid
    observeEvent(input$toRate, {
        default_animes = c('---Primeiro Anime---', '---Segundo Anime---', '---Terceiro Anime---', '---Quarto Anime---', '---Quinto Anime---')
        if ((length(unique(anime_vec())) == 5) & (all(anime_vec() %in% animesData$title))){
            output$wrongAnimes = renderText({NULL})
            updateTabsetPanel(session, 'inTabset', selected='avalie')
        }
        else if (length(unique(anime_vec())) != 5){
            output$wrongAnimes = renderText({'Por favor, selecione 5 animes DIFERENTES e clique em "enviar" novamente.'})
        }
        else if (any(anime_vec() %in% default_animes)){
            output$wrongAnimes = renderText({'Um ou mais animes não foram escolhidos. Por favor, preencha os 5 campos de animes e clique em "enviar" novamente.'})
        }
        else if (any(anime_vec() %in% c('', ' ', '   '))){
            output$wrongAnimes = renderText({'Um dos campos de animes está vazio, certifique de preenchê-lo e clique em "enviar" novamente.'})
        }
        else{
            output$wrongAnimes = renderText({'Um erro estranho ocorreu. Cheque se todos os animes preenchidos estão corretos e clique em "enviar" novamente.'})
        }
    })
    
    # Creates the users match dataframe and changes to result tab
    observeEvent(input$toMatches, {
        # Criar dataframe com matches de outros usuários
        # Renderizar tabela de match usando o dataframe
        # Change tab to result
        updateTabsetPanel(session, 'inTabset', selected = 'result')
        
        # Pegar os nomes dos animes inseridos pelo usuário (que estão no vetor anime_vec)
        userChosenAnime <- getAnimeFromName(input$anime1)
        rbind(userChosenAnime ,getAnimeFromName(input$anime2))
        rbind(userChosenAnime ,getAnimeFromName(input$anime3))
        rbind(userChosenAnime ,getAnimeFromName(input$anime4))
        rbind(userChosenAnime ,getAnimeFromName(input$anime5))
        # Gerar uma amostra aleatória do csv de avaliações por usuário
        #Pega 100 usu�rios
        #ratingSample <- userAnimeNorm[,sample(colnames(userAnimeNorm),100)]
        ratingSample <- userAnimeNorm
        ratingSample <- cbind(ratingSample, "user"=rep(0,nrow(userAnimeNorm)))
        #Adiciona os ratings do usuario para comparacao
        for(i in 1:length(userChosenAnime$anime_id)){
            ratingSample[userChosenAnime$anime_id[i],"user"]=score_vec()[i]-mean(score_vec())
        }
        
        # Fazer uma matriz de correlação com outros usuários
        userCorrelation=sort(cosSparse(as.matrix(ratingSample))["user",][names(ratingSample)!="user"],decreasing=TRUE)
        #Considera os 5 usuarios com maior relacao
        userCorrelation=userCorrelation[1:5]
        userRating=ratingSample[ratingSample$user==0,]
        #Media ponderada de cada usuario para determinar a nota estimada de cada anime
        for(i in 1:length(userRating[,"user"])){
            numerador=0
            denominador=sum(userCorrelation)
            for(j in 1:length(userCorrelation)){
                numerador = numerador + userCorrelation[j]*userAnimeNorm[rownames(userRating)[i],colnames(userCorrelation)[j]]
            }
            userRating[i,"user"]=numerador/denominador
        }
        #Top 10 animes
        top10Ratings = userRating[which(userRating[,"user"]>=sort(userRating[,"user"],decreasing=TRUE)[10]),"user"]
        top10Animes = animesData[colnames(top10Ratings),]
        # Devolver outros animes que esses usuários com alta correlação avaliaram positivamente (na tabela see_animes)
        # Devolver outros animes que esses usuários com alta correlação avaliaram negativamente (na tabela avoid_animes)
    })
    
    
    # ---- Calculate correlation ----
    
    
    
    
    
}