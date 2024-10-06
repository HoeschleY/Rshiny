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
library(shiny)
library(shinyjs)

# Données
df_69 <- read.csv("data/df_69.csv", header = T, dec = ".", sep =",")
df_adresses <- read.csv("data/df_adresses.csv", header = T, dec = ".", sep =",")

# Liste des utilisateurs autorisés
users <- data.frame(
  username = c("user1", ""),
  password = c("pass1", "")
)

fluidPage(
  # Chargement style css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
#################################Onglet connexion######################################
  tabPanel("Connexion",
           useShinyjs(),  # Utiliser shinyjs
           div(id = "login",
               navbarPage(
                 tagList(
                   tags$img(src = "logo.png", height = "30px"),  # Logo
               ),
                          tabPanel("Connexion",
                                   textInput("username", "Nom d'utilisateur"),
                                   passwordInput("password", "Mot de passe"),
                                   actionButton("login_button", "Connexion"),
                                   textOutput("login_message")
                          )
               )
           ),
           
#################################Contenu Application####################################          
div(id = "app_content", style = "display: none;", 
  actionButton("logout_button", "Se déconnecter"),
  navbarPage(
    # Logo
    tagList(
      tags$img(src = "logo.png", height = "30px"),
    ),
    # Onglet 1
    tabPanel("Accueil",
      h1(id = "main-title","Impacte de la classe (DPE) sur les
      consommations électriques des logements du Rhône"),
      fluidRow(
        column(12, 
          h2("Contexte"),
          p("Avec l’accélération du 
          changement climatique et la hausse des prix de l’énergie, 
          la sobriété énergétique est au coeur des préoccupations 
          des Français. C’est pourquoi Enedis nous sollicite en vue 
          d’évaluer l’impact de la classe de Diagnostic de Performance
          Energétique (DPE) sur les consommations électriques de logements."),
          br(),
        
        # Section avec des cartes pour présenter les fonctionnalités
        fluidRow(
          column(4, 
                 wellPanel(
                   h3("Données"),
                   p("Les données brutes sur la consommation énergétique
                     des habitants du rhône sont disponibles avec l'option 
                     de filtrer vos recherches. Le résultat peut
                     être exporter sous format csv.")
                 )
          ),
          column(4, 
                 wellPanel(
                   h3("Carte"),
                   p("Une cartographie intéractive est disponible. 
                     Elle affiche la localisation des habitations 
                     répertoriées dans notre base ainsi que quelques
                     informations à leur sujet.")
                 )
          ),
          column(4, 
                 wellPanel(
                   h3("Statistiques"),
                   p("Une étude statistique a été réalisée pour mettre
                     en avant l'importance de se préoccuper de sa consonmmation 
                     énergétique.")
                    )
          )
          )
         )
        ),
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 90vh; margin: 0;",
        img(src = "dpe_image.jpeg",  height = "100%", width = "100%", style = "border: none;")
    )
      ),
    
    # Onglet 2
    tabPanel("Données",
      h1("Tableau de données"),
      
      # Ajout de filtres par colonnes
      fluidRow(
        column(4,
          selectInput("date_reception_DPE",
                    "Date de réception DPE:",
                    c("All",
                      unique(as.character(df_69$Date_réception_DPE))))
      ),
        column(4,
          selectInput("cp",
                    "Code postal:",
                    c("All",
                      unique(as.character(df_69$Code_postal_.BAN.))))
      ),
      column(4,
        selectInput("Etiquette_DPE",
                  "Etiquette DPE:",
                  c("All",
                    unique(as.character(df_69$Etiquette_DPE))))
      ),
      column(4,
        textInput("filename", "Nom de fichier :", value = "data"),
        downloadButton("downloadData", "Télécharger CSV")
      )
      ),
      # Affiche le tableau
      DT::dataTableOutput("table")
    ),
    
    # Onglet 3
    tabPanel("Carte",
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 90vh; margin: 0;",
        tags$iframe(src = "map.html", height = "100%", width = "100%", style = "border: none;"))
    ),
    
    # Onglet 4
    tabPanel("Statistiques",
      h1("Performance énergétique : Par catégorie"),
      h2("Habitations du rhône") ,
      plotOutput("donutPlot"),
      downloadButton("downloadPlot1", "Télécharger le Graphique"),
      h2("Cout par consommation : Par catégorie"),
      plotOutput("scatterplot"),
      downloadButton("downloadPlot2", "Télécharger le Graphique")))))
)