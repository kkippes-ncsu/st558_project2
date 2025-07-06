#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(httr)
library(tidyverse)

#gives detailed view of breweries based on certain parameters
#only returns max 200 though
brewery_filter <- function(country = NULL, state = NULL, city = NULL, type = NULL, name = NULL, per_page = NULL) {
  
  result <- httr::GET(url = 'https://api.openbrewerydb.org/v1/breweries', 
                      query = list(
                        by_country = country,
                        by_state = state,
                        by_city = city,
                        by_type = type,
                        by_name = name,
                        per_page = per_page
                      ))
  parsed_data <- as_tibble(fromJSON(rawToChar(result$content)))
  
  return(parsed_data)
}

#gives how many breweries there are within certain parameters
brewery_metadata <- function(country = NULL, state = NULL, city = NULL, type = NULL, name = NULL) {
  
  result <- httr::GET(url = 'https://api.openbrewerydb.org/v1/breweries/meta', 
                      query = list(
                        by_country = country,
                        by_state = state,
                        by_city = city,
                        by_type = type,
                        by_name = name
                      ))
  parsed_data <- as_tibble(fromJSON(rawToChar(result$content)))
  
  return(parsed_data)
}

#random selection of breweries - could be used if you want to randomly decide which to go to!

brewery_random <- function(n, country = NULL, state = NULL, city = NULL, type = NULL, name = NULL) {
  
  result <- httr::GET(url = 'https://api.openbrewerydb.org/v1/breweries/random', 
                      query = list(
                        size = n,
                        by_country = country,
                        by_state = state,
                        by_city = city,
                        by_type = type,
                        by_name = name
                      ))
  parsed_data <- as_tibble(fromJSON(rawToChar(result$content)))
  
  return(parsed_data)
}


# Define UI ----
ui <- page_sidebar(
  title = "Brewlytics - Shiny App for Breweries"
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)