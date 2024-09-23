library(shiny)
library(httr)
library(jsonlite)
library(reactable)
library(ggplot2)
library(jpeg)
library(dplyr)

df <- read.csv("data/existants_69.csv", header = T, dec = ".", sep =",")

function(input, output, session) {
  
  # Crée le tableau avec df
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df
    if (input$date_reception_DPE != "All") {
      data <- data[data$Date_réception_DPE == input$date_reception_DPE,]
    }
    if (input$trans != "All") {
      data <- data[data$trans == input$trans,]
    }
    if (input$Etiquette_DPE != "All") {
      data <- data[data$Etiquette_DPE == input$Etiquette_DPE,]
    }
    data
  }))

}
