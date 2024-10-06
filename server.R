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
df_labels <- read.csv("data/df_labels.csv", header = T, dec = ".", sep =",")

function(input, output, session) {
  
  # Crée le tableau avec df_69
  filtered_data <- reactive({
    data <- df_69
    if (input$date_reception_DPE != "All") {
      data <- data[data$Date_réception_DPE == input$date_reception_DPE, ]
    }
    if (input$cp != "All") {
      data <- data[data$cp == input$cp, ]
    }
    if (input$Etiquette_DPE != "All") {
      data <- data[data$Etiquette_DPE == input$Etiquette_DPE, ]
    }
    data
  })
  
  # Afficher le tableau interactif
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
  
  # Gestion du téléchargement
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$carte_html <- renderUI({
    includeHTML("map.html")
  })
  
  output$donutPlot <- renderPlot({
    # Calculate the percentage
    df_plot <- df_labels %>%
      mutate(percentage = Freq / sum(Freq) * 100) %>%
      mutate(Var1 = factor(Var1, levels = Var1))
    
    # Create the donut chart
    ggplot(df_plot, aes(x = "", y = percentage, fill = Var1)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +  # Transformer le graphique en un diagramme en camembert
      theme_void() +  # Enlever les axes et les grilles
      labs(title = "Diagramme en Camembert")
  })
  

}
