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
df_labels <- read.csv("data/df_labels.csv", header = T, dec = ".", sep =",")
df_conso_cout <- read.csv("data/df_conso_cout.csv", header = T, dec = ".", sep =",")
# Copie pour avoir acces et telecharger le PNG (*1)
df_plot <- df_labels %>%
  mutate(percentage = Freq / sum(Freq) * 100 , ypos = cumsum(percentage) - 0.5 * percentage) %>%
  mutate(Categorie = factor(Categorie, levels = Categorie))

# Liste des utilisateurs autorisés
users <- data.frame(
  username = c("user1", "user2"),
  password = c("pass1", "pass2")
)


function(input, output, session) {
  
##################Connexion à l'app######################################################
  observeEvent(input$login_button, {
    req(input$username, input$password)  # Vérifier que les champs ne sont pas vides
    
    # Vérifier les informations d'identification
    user_row <- users[users$username == input$username & users$password == input$password, ]
    
    if (nrow(user_row) == 1) {
      # Authentification réussie
      shinyjs::hide("login")  # Cacher la zone de connexion
      shinyjs::show("app_content")  # Afficher le contenu de l'application
      output$login_message <- renderText("Connexion réussie !")
    } else {
      output$login_message <- renderText("Nom d'utilisateur ou mot de passe incorrect.")
    }
  })
  
  observeEvent(input$logout_button, {
    shinyjs::show("login")  # Afficher la zone de connexion
    shinyjs::hide("app_content")  # Cacher le contenu de l'application
    output$login_message <- renderText("")
  })
######################################################################################
  
  # Crée le tableau avec df_69
  filtered_data <- reactive({
    data <- df_69
    if (input$date_reception_DPE != "All") {
      data <- data[data$Date_réception_DPE == input$date_reception_DPE, ]
    }
    if (input$cp != "All") {
      data <- data[data$Code_postal_.BAN. == input$cp, ]
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
  
  #Import de la carte
  output$carte_html <- renderUI({
    includeHTML("map.html")
  })
  
  #Camambert
  output$donutPlot <- renderPlot({
    # Calculate the percentage
    df_plot <- df_labels %>%
      mutate(percentage = Freq / sum(Freq) * 100 , ypos = cumsum(percentage) - 0.5 * percentage) %>%
      mutate(Categorie = factor(Categorie, levels = Categorie))
    
    # Create the donut chart
    ggplot(df_plot, aes(x = "", y = percentage, fill = Categorie)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +  # Transformer le graphique en un diagramme en camembert
      theme_void() + # Enlever les axes et les grilles
      scale_fill_discrete(labels = paste0(df_plot$Categorie, ": ", round(df_plot$percentage, 1), "%"))  # Ajouter les pourcentages à la légende
  })
  
  # Nuage de points
  output$scatterplot <- renderPlot({
    ggplot(df_conso_cout, aes(x=Consommation, y=Cout, color = Categorie)) +
      xlim(-2,  3000) +  # Limiter l'axe X
      ylim(-2, 1000) + 
      geom_point(size = 3) +
      theme_minimal()
  })
  
  # Exporter le graphique en PNG (*1)
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)  # Ouvrir un fichier PNG
      print(ggplot(df_plot, aes(x = "", y = percentage, fill = Categorie)) +
             geom_bar(stat = "identity", width = 1, color = "white") +
             coord_polar(theta = "y") +  # Transformer le graphique en un diagramme en camembert
             theme_void() + # Enlever les axes et les grilles
             scale_fill_discrete(labels = paste0(df_plot$Categorie, ": ", round(df_plot$percentage, 1), "%")))
      dev.off()  # Fermer le fichier PNG
    }
  )
  
  # Exporter le graphique en PNG
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)  # Ouvrir un fichier PNG
      print(ggplot(df_conso_cout, aes(x=Consommation, y=Cout, color = Categorie)) +
             xlim(-2,  3000) +  # Limiter l'axe X
             ylim(-2, 1000) + 
             geom_point(size = 3) +
             theme_minimal())
             dev.off()  # Fermer le fichier PNG
    }
  )
  

}
