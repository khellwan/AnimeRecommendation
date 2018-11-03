## ui.R ##
library(shiny)
library(shinydashboard)

# Sidebar ----
sidebar <- dashboardSidebar(
    sidebarMenu(
    menuItem("Exploração dos Dados", tabName = "explore", icon = icon("dashboard")),
    menuItem("Modelagem", icon = icon("th"), tabName = "model"),
    menuItem("Recomendação", icon = icon("th"), tabName = "recommend")
    )
)

# Body ----
body <- dashboardBody(
    # Data Expolration ----
    tabItems(
        tabItem(tabName = "explore",
                h2("Exploração dos Dados"),
                # First row
                fluidRow(
                    box(
                        plotOutput("scoreHist", height = 250),
                        status = "primary"
                    ),
                    
                    box(
                        title = "Controls",
                        status = "primary",
                        sliderInput("slider", "Number of observations:", 1, 400000, 200000)
                    )
                ),
                # Second row
                fluidRow(
                    box(
                        plotOutput("ratingsPerAnime", height = 250),
                        status = "primary"
                    ),
                    
                    box(
                        plotOutput("releaseYear", height = 250),
                        status = "primary"
                    )
                )
        ),
        # Modeling ----
        tabItem(tabName = "model",
                h2("Criação de Modelos")
        ),
        # Recommendation ----
        tabItem(tabName = "recommend",
                h2("Recomendação de Animes")
        )
        
        
    )
)

# DashboardPage ----
dashboardPage(
    dashboardHeader(title = "Animes"),
    sidebar,
    body
)