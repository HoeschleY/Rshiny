# Install necessary packages if not already installed
required_packages <- c("shiny", "shinythemes", "leaflet", "dplyr", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# Load the filtered CSV file
df_filtered <- read.csv("C:/Users/yanni/BUT/R/S2/CSV/df_filtered.csv")

# Rename the column "Code_postal_.BAN." to "Code_postal"
colnames(df_filtered)[colnames(df_filtered) == "Code_postal_.BAN."] <- "Code_postal"

# Shiny UI with multiple tabs
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    # Title
    "Shiny App with Map and Regression",
    
    # First Tab - Other Content (e.g., form elements)
    tabPanel("Navbar 1",
             sidebarPanel(
               fileInput("file", "File input:"),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Default actionButton:"),
               actionButton("action", "Search"),
               
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 1",
                          h4("Table"),
                          tableOutput("table"),
                          h4("Verbatim text output"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
                 tabPanel("Tab 2", "This panel is intentionally left blank"),
                 tabPanel("Tab 3", "This panel is intentionally left blank")
               )
             )
    ),
    
    # Second Tab - Map Integration
    tabPanel("Map",
             sidebarPanel(
               selectInput("dpe_filter", "Filter by DPE Etiquette:",
                           choices = unique(df_filtered$Etiquette_DPE),
                           selected = NULL,
                           multiple = TRUE)
             ),
             mainPanel(
               leafletOutput("map", height = 600)  # Displaying the map here
             )
    ),
    
    # Third Tab - Regression Plot with Dynamic Axis Selection
    tabPanel("Regression Plot",
             sidebarPanel(
               selectInput("x_axis", "Select X-axis:", 
                           choices = names(df_filtered), 
                           selected = "Surface_habitable_logement"),
               selectInput("y_axis", "Select Y-axis:", 
                           choices = names(df_filtered), 
                           selected = "Coût_chauffage"),
               actionButton("plot_regression", "Plot Regression")
             ),
             mainPanel(
               plotOutput("regressionPlot")
             )
    ),
    
    # Placeholder Tab
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive data based on DPE filter selection for the map
  filtered_data <- reactive({
    if (is.null(input$dpe_filter) || length(input$dpe_filter) == 0) {
      df_filtered  # Return all data if no filter is selected
    } else {
      df_filtered %>%
        filter(Etiquette_DPE %in% input$dpe_filter)  # Filter data by DPE etiquette
    }
  })
  
  # Render the map on the "Map" tab
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lon, lat = ~lat,
        popup = ~paste("Etiquette DPE:", Etiquette_DPE, "<br>",
                       "Postal Code:", Code_postal, "<br>",
                       "Reception Date:", Date_réception_DPE),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Other elements for the first tab
  output$txtout <- renderText({
    paste(input$txt, input$slider, sep = ", ")
  })
  
  output$table <- renderTable({
    head(mtcars, 5)  # Example table data for display
  })
  
  # Reactive expression for regression data based on selected axes
  observeEvent(input$plot_regression, {
    output$regressionPlot <- renderPlot({
      # Check if both x and y axis columns exist
      if(input$x_axis %in% colnames(df_filtered) && input$y_axis %in% colnames(df_filtered)) {
        
        # Plotting using ggplot2
        ggplot(df_filtered, aes_string(x = input$x_axis, y = input$y_axis)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
          labs(
            title = paste("Regression Plot:", input$y_axis, "vs", input$x_axis),
            x = input$x_axis,
            y = input$y_axis
          ) +
          theme_minimal()
      } else {
        # If invalid axes are selected, display a message
        plot.new()
        text(0.5, 0.5, "Invalid X or Y axis selection", cex = 2)
      }
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
