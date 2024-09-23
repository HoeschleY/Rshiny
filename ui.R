library(shiny)
library(httr)
library(jsonlite)
library(reactable)
library(ggplot2)
library(jpeg)
library(dplyr)

# Données
df <- read.csv("data/existants_69.csv", header = T, dec = ".", sep =",")
cpr <- distinct(df,df$`Code_postal_(BAN)`,)
cpr

navbarPage(

  title = tags$img(src = "img/logo.jng"), # A CORRIGER
  
  # Onglet 1
  tabPanel("Projet",
           h1("Impacte de la classe (DPE) sur les
consommations électriques des logements du Rhône"),
           p("This is some text on the first tab.")
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
                                  unique(as.character(df$Date_réception_DPE))))
             ),
             # A FINIR
             column(4,
                    selectInput("trans",
                                "Code postal:",
                                c("All",
                                  unique(as.character(cpr$df$`Code_postal_(BAN)`))))
             ),
             column(4,
                    selectInput("Etiquette_DPE",
                                "Etiquette DPE:",
                                c("All",
                                  unique(as.character(df$Etiquette_DPE))))
             )
           ),
           # Affiche le tableau
           DT::dataTableOutput("table")
  ),
  
  # Onglet 3
  tabPanel("Tab 3",
           h1("Content for Tab 3"),
           p("This is some text on the third tab.")
  )
)
