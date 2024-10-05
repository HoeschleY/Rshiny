# Remove all objects from the workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/yanni/BUT/R/S2/CSV")
getwd()

# Install and load necessary libraries
# install.packages("rmarkdown")
# install.packages("shiny")
# install.packages("httr")
# install.packages("jsonlite")

library(rmarkdown)
library(shiny)
library(httr)
library(jsonlite)

#############################################################

# Define base URL
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"

# Read the CSV file
df_adresses <- read.csv("adresses-69.csv", sep = ";", dec = ".")
df_69 = data.frame()

# Get unique postal codes
code = unique(df_adresses$code_postal)

# Loop over each postal code
for(code_postal in code){
  
  # Set query parameters
  params <- list(
    size = 10000,
    select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE",
    q = code_postal,
    q_fields = "Code_postal_(BAN)"
  )
  
  # Encode URL with parameters
  url_encoded <- modify_url(base_url, query = params)
  print(url_encoded)
  
  # Make GET request
  response <- GET(url_encoded)
  
  # Check the status of the response
  if (status_code(response) == 200) {
    print(paste("Success for code:", code_postal))
    
    # Convert raw response content to text and parse JSON
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    
    # Check if the response contains data
    if (!is.null(content$result)) {
      # Append the results to df_69
      df_69 <- rbind(df_69, content$result)
    }
    
    # Print the number of rows fetched
    print(content$total)
    
  } else {
    print(paste("Failed for code:", code_postal, "with status:", status_code(response)))
  }
}

# Display the number of rows in the final dataframe
print(nrow(df_69))

# Calculate the ratio of rows to postal codes
print(nrow(df_69) / length(code))

# View the data (optional)
# View(df_69)

