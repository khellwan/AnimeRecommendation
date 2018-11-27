## ui.R ##

# Sidebar ----
sidebar <- dashboardSidebar(
    sidebarMenu(
    menuItem("Exploração dos Dados", tabName = "explore", icon = icon("dashboard")),
    menuItem("Recomendação", icon = icon("th"), tabName = "choose")
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
                    tabBox(
                        tabPanel(
                            title = "Histograma das avaliações",
                            plotOutput("scoreHist", height = 250),
                            status = "primary"
                        ),
                        
                        tabPanel(
                            title = "Controls",
                            status = "primary",
                            sliderInput("slider", "Number of observations:", 1, 400000, 200000)
                        )
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
        # Recommendation ----
        tabItem(tabName = "choose", 
            tabBox(
                tabPanel( 
                    title = "Escolha seus Animes",
                    h2('Escolha seus Animes', style = 'text-align:center'),
                    h4('Por favor escolha 5 animes da tabela abaixo.'),
                    h4('Você pode clicar em "Limpar Animes" para limpar os animes que você escolheu.'),
                    DT::dataTableOutput("anime_table"),
                    fluidRow(
                       column(
                         textInput("anime1", "Anime 1", "---Primeiro Anime---"),
                         width = 6
                       ),
                       column(
                         textInput("anime2", "Anime 2", "---Segundo Anime---"),
                         width = 6
                       )
                    ),
                     fluidRow(
                       column(
                         textInput("anime3", "Anime 3", "---Terceiro Anime---"),
                         width = 6
                       ),
                       column(
                         textInput("anime4", "Anime 4", "---Quarto Anime---"),
                         width = 6
                       )
                     ),
                     fluidRow(
                       column(
                         textInput("anime5", "Anime 5", "---Quinto Anime---"),
                         width = 6
                       )
                     ),
                     fluidRow(
                       column(
                       actionButton("toRate", "Enviar", icon("arrow-right"), 
                                    style="color: #ffffff; background-color: #337ab7; border-color: #318fe0"),
                       actionButton("clearRows", "Limpar Animes", icon("eraser"),
                                    style="color: #ffffff; background-color: #337ab7; border-color: #318fe0"),
                       width = 6
                       )
                     ),
                    fluidRow(
                        h3(textOutput("wrongAnimes"))
                    )
                ),
                
                # Rate animes tab
                tabPanel(
                  title = "Avalie",
                  fluidRow(
                     h2('Dê a nota para os Animes', style = 'text-align:center'),
                     h4('Por favor, dê uma nota de 0 a 10 para os Animes que escolheu', style = 'text-align:center')
                   ),
                   column(
                       fluidRow(
                         # Display user's selected animes
                         textOutput('anime1'),
                         # Add radio buttons for user to rate the anime
                         prettyRadioButtons(inputId = "rating1", label = NULL,
                                            0:10, inline = TRUE, selected = 0, animation = 'pulse', icon = icon('star'))
                       ),
                       fluidRow(
                         textOutput('anime2'),
                         prettyRadioButtons(inputId = "rating2", label = NULL,
                                            0:10, inline = TRUE, selected = 0, animation = 'pulse', icon = icon('star'))
                       ),
                       fluidRow(
                         textOutput('anime3'),
                         prettyRadioButtons(inputId = "rating3", label = NULL,
                                            0:10, inline = TRUE, selected = 0, animation = 'pulse', icon = icon('star'))
                       ),
                       fluidRow(
                         textOutput('anime4'),
                         prettyRadioButtons(inputId = "rating4", label = NULL,
                                            0:10, inline = TRUE, selected = 0, animation = 'pulse', icon = icon('star'))
                       ),
                       fluidRow(
                         textOutput('anime5'),
                         prettyRadioButtons(inputId = "rating5", label = NULL,
                                            0:10, inline = TRUE, selected = 0, animation = 'pulse', icon = icon('star'))
                       ),
                      
                       
                       # Action button to go to the next tab
                       fluidRow(
                         actionButton("toMatches", "Enviar", icon("arrow-right"), 
                                      style="color: #ffffff; background-color: #337ab7; border-color: #318fe0"),
                         actionButton("clearRating", "Redefinir avaliações", icon("eraser"),
                                      style="color: #ffffff; background-color: #337ab7; border-color: #318fe0")
                       ),
                       width = 12,
                       offset = 1
                   )
              ),
              # Show results  ----
              tabPanel(title = "Seus Matches",
                       fluidRow(
                           h2('Encontramos animes que combinam com você!', style = 'text-align:center'),
                           DT::dataTableOutput("match_table"), width = 12, title = 'Your Closest Matched Critics'),
                       fluidRow(
                           column(h3('Animes para assistir', style = 'text-align:center'),
                                  DT::dataTableOutput('see_animes'), width = 6),
                           column(h3('Animes para evitar', style = 'text-align:center'),
                                  DT::dataTableOutput('avoid_animes'), width = 6))
              )
          )
        )
       )
    )
# DashboardPage ----
dashboardPage(
    dashboardHeader(title = "Animes"),
    sidebar,
    body
)