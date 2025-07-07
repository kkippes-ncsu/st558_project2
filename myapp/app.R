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
library(ggplot2)
library(jsonlite)
library(DT)



#gives how many breweries there are within certain parameters
brewery_metadata <- function(state = NULL, city = NULL, type = NULL, name = NULL) {
  
  result <- httr::GET(url = 'https://api.openbrewerydb.org/v1/breweries/meta', 
                      query = list(
                        by_state = state,
                        by_city = city,
                        by_type = type,
                        by_name = name
                      ))
  parsed_data <- as_tibble(fromJSON(rawToChar(result$content)))
  
  return(parsed_data)
}

#random selection of breweries - could be used if you want to randomly decide which to go to!

brewery_random <- function(n, state = NULL, city = NULL, type = NULL, name = NULL) {
  
  result <- httr::GET(url = 'https://api.openbrewerydb.org/v1/breweries/random', 
                      query = list(
                        size = n,
                        by_state = state,
                        by_city = city,
                        by_type = type,
                        by_name = name
                      ))
  parsed_data <- as_tibble(fromJSON(rawToChar(result$content)))
  
  return(parsed_data)
}


# Define UI ----
ui <- fluidPage(
  titlePanel('Brewlytics - Shiny App for Breweries'),
  tabsetPanel(
    #About tab
    tabPanel('About',
             h2('Purpose'),
             p('The purpose of this app is to fulfill the requirements of Project 2 in ST 558. This app will explore different breweries in the', em('Open Brewery Database.'),
               'This app will allow its users to filter the brewery data set, download chosen subsets of the data, and view numerical or graphical summaries dependent on the selected parameters.'),
             h2('The Data'),
             p('Ranging all across the world, information like brewery id, name, location, type, phone number, and website can be found in this database. Location information is
               presented from address all the way down to coordinates. The source of the data can be found below:'), a('Open Brewery DB', href = 'https://www.openbrewerydb.org/'),
             h2('Tabs'),
             h4('About'),
             p('The purpose of the about tab is to give a general overview of the app, the data set and where to find the data.'),
             h4('Data Download'),
             p('The purpose of the data download tab is to allow the user to make changes to the API query functions, return and display the data,
               subset this data, and then download and save the data.'),
             h4('Data Exploration'),
             p('The purpose of the data exploration tab is to allow the user to choose different varaiables and combination of variables to view numerical and graphical summaries.'),
             tags$img(height = 300, width = 400, src = 'https://bundobust.com/wp-content/uploads/2023/05/2021-09-14_BundobustBrewery-114-scaled.jpg')
             ),
    #Data download tab
    tabPanel('Data Download',
             sidebarLayout(
               sidebarPanel(
                 # Select state
                 selectInput(inputId = 'state', label = strong('Select State'), 
                             choices = c('', state.name),
                             selected = ''),
                 #type in city
                 textInput(inputId = 'city', label = strong('Find City'),
                           value = ''),
                 #select type
                 selectInput(inputId = 'type', label = strong('Select Type'), 
                             choices = c('', 'micro', 'nano', 'regional','brewpub', 'large','planning','bar','contract','proprietor','closed'),
                             selected = ''),
                 #type in name containing
                 textInput(inputId = 'name', label = strong('Name Contains'),
                           value = ''),
                 #add a download button
                 downloadButton("download_data", "Download Results")),
               #output the table to the main panel
               mainPanel(DT::DTOutput('brewery_table'))
             )),
    #data exploration tab
    tabPanel('Data Exploration')
  )
)

# Define server
server <- function(input, output, session) {
  #gives detailed view of breweries based on certain parameters
  #only returns 50 though
  brewery_filter <- function(state = NULL, city = NULL, type = NULL, name = NULL) {
    #define url
    url = 'https://api.openbrewerydb.org/v1/breweries'
    #define filters
    query <- list(
      by_state = if (!is.null(state) && state != '') state else NULL,
      by_city  = if (!is.null(city) && city != '') city else NULL,
      by_type  = if (!is.null(type) && type != '') type else NULL,
      by_name  = if (!is.null(name) && name != '') name else NULL
    )
    #remove any filters that are null
    query <- query[!sapply(query, is.null)]
    #get my result
    result <- httr::GET(url = url, query = query)
    
    brewery_data <- as_tibble(fromJSON(rawToChar(result$content)))
    
    if (length(brewery_data) == 0 | nrow(brewery_data) == 0) {
      return(tibble(message = 'No results found'))
    }
    
    return(brewery_data)
  }
  
  brewery_data <- reactive({
    brewery_filter(state = input$state,
                   city = input$city,
                   type = input$type,
                   name = input$name)
  })
  output$brewery_table <- DT::renderDataTable({
    req(brewery_data())
    brewery_data()
  })
  
  # Allowing the user to download the data as a csv
  output$download_data <- downloadHandler(
    filename = function() {
      'Brewery_Data.csv'
    },
    content = function(file) {
      readr::write_csv(brewery_data(), file)
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)