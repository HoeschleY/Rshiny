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

df_69 <- read.csv("C:/SAE R SHINY/df_69.csv", header = TRUE, dec = ".", sep =",")

df_adresses <- read.csv("C:/SAE R SHINY/df_adresses.csv", header = T, dec = ".", sep =",")
df_labels <- read.csv("C:/SAE R SHINY/df_labels.csv", header = T, dec = ".", sep =",")
df_conso_cout <- read.csv("C:/SAE R SHINY/df_conso_cout.csv", header = T, dec = ".", sep =",")
# Load the filtered CSV file
df_filtered <- read.csv("C:/SAE R SHINY/df_filtered.csv")

# Rename the column "Code_postal_.BAN." to "Code_postal"
colnames(df_filtered)[colnames(df_filtered) == "Code_postal_.BAN."] <- "Code_postal"
# Rename the column "Code_postal_.BAN." to "Code_postal"
colnames(df_69)[colnames(df_69) == "Code_postal_.BAN."] <- "Code_postal"
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
  
  # Reactive data based on DPE filter selection for the map
  filtered_data1 <- reactive({
    if (is.null(input$df_69) || length(input$df_69) == 0) {
      df_filtered  # Return all data if no filter is selected
    } else {
      df_filtered %>%
        filter(Etiquette_DPE %in% input$df_69)  # Filter data by DPE etiquette
    }
  })
  
  # Render the map on the "Map" tab
  output$map <- renderLeaflet({
    leaflet(filtered_data1()) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lon, lat = ~lat,
        popup = ~paste("Etiquette DPE:", Etiquette_DPE, "<br>",
                       "Postal Code:", Code_postal, "<br>",
                       "Reception Date:", Date_réception_DPE),
        clusterOptions = markerClusterOptions()
      )
  })
  

#regression
  
  # Other elements for the first tab
  output$txtout <- renderText({
    paste(input$txt, input$slider, sep = ", ")
  })
  
  output$table <- renderTable({
    head(mtcars, 5)  # Example table data for display
  })
  
  observeEvent(input$plot_regression, {
    output$regressionPlot <- renderPlot({
      # Vérification si les colonnes existent et sont bien numériques
      if (input$x_axis %in% colnames(df_filtered) && input$y_axis %in% colnames(df_filtered)) {
        
        # Convertir les colonnes en numérique et gérer les erreurs de conversion
        df_filtered[[input$x_axis]] <- suppressWarnings(as.numeric(as.character(df_filtered[[input$x_axis]])))
        df_filtered[[input$y_axis]] <- suppressWarnings(as.numeric(as.character(df_filtered[[input$y_axis]])))
        
        # Filtrer les valeurs non numériques ou manquantes
        df_valid <- df_filtered %>%
          filter(!is.na(df_filtered[[input$x_axis]]) & !is.na(df_filtered[[input$y_axis]]))
        
        if (nrow(df_valid) > 1) {
          ggplot(df_valid, aes_string(x = input$x_axis, y = input$y_axis)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Ligne de régression linéaire
            labs(
              title = paste("Regression Plot:", input$y_axis, "vs", input$x_axis),
              x = input$x_axis,
              y = input$y_axis
            ) +
            theme_minimal()
        } else {
          # Afficher un message si pas assez de données après filtrage
          plot.new()
          text(0.5, 0.5, "Pas assez de données pour la régression", cex = 2)
        }
      } else {
        # Afficher un message si la sélection des colonnes est invalide
        plot.new()
        text(0.5, 0.5, "Sélection invalide des axes X ou Y", cex = 2)
      }
    })
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
)}

