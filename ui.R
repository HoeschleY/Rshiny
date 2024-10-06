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

# Données
df_69 <- read.csv("data/df_69.csv", header = T, dec = ".", sep =",")
df_adresses <- read.csv("data/df_adresses.csv", header = T, dec = ".", sep =",")


navbarPage(
  
  tagList(
    tags$img(src = "logo.png", height = "30px"),  # Ajustez la hauteur selon vos besoins
  ),
  
  # Onglet 1
  tabPanel("Projet",
           h1("Impacte de la classe (DPE) sur les
consommations électriques des logements du Rhône"),
           plotOutput("donutPlot")
           
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
             # A FINIR
             column(4,
                    selectInput("cp",
                                "Code postal:",
                                c("All",
                                  unique(as.character(df_69$`Code_postal_(BAN)`))))
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
             tags$iframe(src = "map.html", height = "100%", width = "100%", style = "border: none;")
           )
  )
)