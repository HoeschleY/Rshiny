library(shiny)
library(httr)
library(jsonlite)
library(reactable)
library(ggplot2)
library(jpeg)
library(dplyr)
library(bslib)
library(leaflet)
library(mapview)
library(plotly)
library(DT)
library(ggrepel)
library(shinyjs)
library(shinydashboard)

df_69 <- read.csv("data/df_69.csv", header = TRUE, dec = ".", sep = ",")
df_var_quanti <- read.csv("data/df_var_quanti.csv", header = TRUE, dec = ".", sep = ",")
df_adresses <- read.csv("data/df_adresses.csv", header = TRUE, dec = ".", sep = ",")
df_labels <- read.csv("data/df_labels.csv", header = TRUE, dec = ".", sep = ",")
df_conso_cout <- read.csv("data/df_conso_cout.csv", header = TRUE, dec = ".", sep = ",")
df_filtered <- read.csv("data/df_filtered.csv")

users <- data.frame(
  username = c("user1", "user2"),
  password = c("pass1", "pass2")
)


fluidPage(
  # Theme selector
  shinythemes::themeSelector(),
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # Interface utilisateur de connexion
  div(id = "login", navbarPage(
    tagList(tags$img(src = "logo.png", height = "30px")),
    tabPanel("Connexion",
             textInput("username", "Nom d'utilisateur"),
             passwordInput("password", "Mot de passe"),
             actionButton("login_button", "Connexion"),
             textOutput("login_message")
    )
  )),
  
  # Interface après connexion
  div(id = "app_content", style = "display: none;", 
      actionButton("logout_button", "Se déconnecter"),
      navbarPage(
        tagList(tags$img(src = "logo.png", height = "30px")),
        
        # Onglet 1 : Accueil
        tabPanel("Accueil",
                 h1(id = "main-title", "Impact de la classe (DPE) sur les consommations électriques des logements du Rhône"),
                 fluidRow(
                   column(12, 
                          h2("Contexte"),
                          p("Avec l’accélération du changement climatique et la hausse des prix de l’énergie, la sobriété énergétique est au cœur des préoccupations des Français."),
                          br(),
                          fluidRow(
                            column(4, wellPanel(
                              h3("Données"),
                              p("Les données brutes sur la consommation énergétique des habitants du Rhône sont disponibles avec l'option de filtrer vos recherches.")
                            )),
                            column(4, wellPanel(
                              h3("Carte"),
                              p("Une cartographie interactive est disponible.")
                            )),
                            column(4, wellPanel(
                              h3("Statistiques"),
                              p("Une étude statistique a été réalisée pour mettre en avant la consommation énergétique.")
                            ))
                          )
                   )
                 ),
                 tags$div(style = "display: flex; justify-content: center; align-items: center; height: 90vh; margin: 0;",
                          img(src = "dpe_image.jpeg", height = "100%", width = "100%", style = "border: none;"))
        ),
        
        # Onglet 2 : Données
        tabPanel("Données",
                 h1("Tableau de données"),
                 fluidRow(
                   column(4, selectInput("date_reception_DPE", "Date de réception DPE:",
                                         c("All", unique(as.character(df_69$Date_réception_DPE))))),
                   column(4, selectInput("cp", "Code postal:",
                                         c("All", unique(as.character(df_69$Code_postal_.BAN.))))),
                   column(4, selectInput("Etiquette_DPE", "Etiquette DPE:",
                                         c("All", unique(as.character(df_69$Etiquette_DPE))))),
                   column(4, textInput("filename", "Nom de fichier :", value = "data"),
                          downloadButton("downloadData", "Télécharger CSV"))
                 ),
                 DT::dataTableOutput("table")
        ),
        
        # Onglet 3 : Carte
        tabPanel("Carte", 
                 tags$div(style = "display: flex; justify-content: center; align-items: center; height: 90vh; margin: 0;",
                          tags$iframe(src = "map.html", height = "100%", width = "100%", style = "border: none;"))
        ),
        
        # Onglet 4 : Régression Plot
        tabPanel("Regression Plot",
                 sidebarPanel(
                   selectInput("x_axis", "Sélectionnez l'axe X :", choices = names(df_var_quanti)),
                   selectInput("y_axis", "Sélectionnez l'axe Y :", choices = names(df_var_quanti)),
                   actionButton("plot_regression", "Tracer la régression")
                 ),
                 mainPanel(plotOutput("regressionPlot")),
                 downloadButton("downloadPlot3", "Télécharger le Graphique")
        ),
        
        # Onglet 5 : Statistiques
        tabPanel("Statistiques",
                 h1("Performance énergétique : Par catégorie"),
                 h2("Habitations du rhône") ,
                 plotOutput("donutPlot"),
                 downloadButton("downloadPlot1", "Télécharger le Graphique"),
                 h2("Cout par consommation : Par catégorie"),
                 plotOutput("scatterplot"),
                 downloadButton("downloadPlot2", "Télécharger le Graphique")
        ),
        
        # Onglet 6 : Corrélation
        tabPanel("Corrélation",
                 sidebarPanel(
                   selectInput("var1", "Variable 1:", choices = names(df_var_quanti)),
                   selectInput("var2", "Variable 2:", choices = names(df_var_quanti)),
                   actionButton("lancer_corr", "Lancer la Corrélation")
                 ),
                 mainPanel(plotOutput("corr_plot"))
        ),
        
        # Nouvel Onglet 7 - KPIs
        tabPanel("KPIs",
                 h1("Indicateurs Clés de Performance"),
                 fluidRow(
                   valueBoxOutput("kpi1"),
                   valueBoxOutput("kpi2"),
                   valueBoxOutput("kpi3"),
                   valueBoxOutput("kpi4"),
                   valueBoxOutput("kpi5"),
                   valueBoxOutput("kpi6")
                 ),
        ),
        
        # Onglet 8 : Mise à jour
        tabPanel("Update",
                 h1("Mettre à jour les données"),
                 dateRangeInput("update_date_range", "Période:", 
                                start = max(as.Date(df_69$Date_réception_DPE)), 
                                end = Sys.Date(), 
                                min = min(as.Date(df_69$Date_réception_DPE)),
                                max = Sys.Date(),
                                format = "yyyy-mm-dd"),
                 actionButton("update_button", "Mettre à jour les données"),
                 tags$div(id = "update_status", p("Mise à jour en attente...")),
                 tags$div(id = "progress_output", p("Progression: 0%"))
        )
      )
  )
)

