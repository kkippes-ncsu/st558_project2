# packages needed
library(shiny)
library(bslib)
library(httr)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(DT)
library(maps)

# Define UI
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
                 radioButtons('selection_type', 'Choose View Type:',
                              choices = c('Filtered', 'Random')),
                 conditionalPanel(
                   condition = 'input.selection_type == "Filtered"',
                   h3('Choose a subset of breweries you want information for!'),
                   #Disclaimer
                   h5('*Please note that the API only allows a maximum of 200 results to be returned'),
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
                   downloadButton('download_filtered_data', 'Download Results')
                 ),
                 conditionalPanel(
                   condition = 'input.selection_type == "Random"',
                   h3('Randomly get your desired number of breweries'),
                   #Disclaimer
                   h5('*Please note that the API only allows a maximum of 50 results to be returned'),
                   sliderInput(inputId = 'n', label = 'Number of Breweries',
                               min = 1, max = 50, value = 1, step = 1),
                   #add a download button
                   downloadButton('download_random_data', 'Download Results')
                 )
              ),
               #output the table to the main panel
               mainPanel(
                 conditionalPanel(
                   condition = 'input.selection_type == "Filtered"',
                   DT::dataTableOutput('filtered_table')
                 ),
                 conditionalPanel(
                   condition = 'input.selection_type == "Random"',
                   DT::dataTableOutput('random_table')
                 )
              )
             )),
    #data exploration tab
    tabPanel('Data Exploration',
             sidebarLayout(
               sidebarPanel(
                 #allow user to subset data for the summaries
                 h4('Choose a subset of breweries you want information for!'),
                 #Disclaimer
                 h5('*Please note that the API only allows a maximum of 200 results to be returned'),
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
                 #allow user to select summary type
                 radioButtons('summary_type', 'Choose Summary Type:',
                                        choices = c('Contingency Table', 'Numerical Summary', 'Graphs'),
                                        selected = 'Contingency Table'
                           ),
                 #condition for contingency table
                conditionalPanel(
                  condition = 'input.summary_type == "Contingency Table"',
                  selectInput(inputId = 'var_1', label = strong('Variable 1'),
                              choices = c('state', 'city', 'brewery_type')),
                  selectInput(inputId = 'var_2', label = strong('Variable 2'),
                              choices = c('state', 'city', 'brewery_type'))
                           ),
                #conditional for numerical summary
                conditionalPanel(
                  condition = 'input.summary_type == "Numerical Summary"',
                  selectInput(inputId = 'var', label = strong('Group By'),
                              choices = c('state', 'city', 'brewery_type'))
                ),
                #condition for graphs
                conditionalPanel(
                  condition = 'input.summary_type == "Graphs"',
                  #Choose plot type
                  selectInput(inputId = 'plot_type', label = strong('Choose Plot Type'),
                              choices = c('Bar Chart', 'Boxplot', 'Location', 'Heatmap')),
                  #add facet option
                  selectInput(inputId = 'facet_option', label = strong('Facet by (Optional):'),
                              choices = c('None', 'state', 'brewery_type', 'city'))
                )
                 
                  ),
               #main panel for data exploration with conditionals
               mainPanel(conditionalPanel(condition = 'input.summary_type == "Contingency Table"',
                                          tableOutput('contingency_table')),
                         conditionalPanel(condition = 'input.summary_type == "Numerical Summary"',
                                          DT::dataTableOutput('numeric_summaries')),
                         conditionalPanel(condition = 'input.summary_type == "Graphs"',
                                          plotOutput('all_plots'))
    )
  )
)))

# Define server
server <- function(input, output, session) {
  #gives detailed view of breweries based on certain parameters
  #only returns 200 though
  brewery_filter <- function(state = NULL, city = NULL, type = NULL, name = NULL) {
    #define url
    url = 'https://api.openbrewerydb.org/v1/breweries'
    #define filters
    query <- list(
      by_state = if (!is.null(state) && state != '') state else NULL,
      by_city  = if (!is.null(city) && city != '') city else NULL,
      by_type  = if (!is.null(type) && type != '') type else NULL,
      by_name  = if (!is.null(name) && name != '') name else NULL,
      per_page = 200
    )
    #remove any filters that are null
    query <- query[!sapply(query, is.null)]
    #get my result
    result <- httr::GET(url = url, query = query)
    #turn data into a data frame
    brewery_data <- as_tibble(fromJSON(rawToChar(result$content)))
    #return no results if parameters chosen don't have matching records
    if (length(brewery_data) == 0 | nrow(brewery_data) == 0) {
      return(tibble(message = 'No results found'))
    }
    
    return(brewery_data)
  }
  #random selection of breweries - could be used if you want to randomly decide which to go to!
  brewery_random <- function(n) {
    url = 'https://api.openbrewerydb.org/v1/breweries/random'
    #define filters
    random_query <- list(
      size = n
    )
    #get my result
    random_result <- httr::GET(url = url, query = random_query)
    #turn data into a data frame
    random_brewery <- as_tibble(fromJSON(rawToChar(random_result$content)))
    
    return(random_brewery)
  }
  #define the filtered data
  brewery_data <- reactive({
    brewery_filter(state = input$state,
                   city = input$city,
                   type = input$type,
                   name = input$name)
  })
  #define the random data
  random_brewery <- reactive({
    brewery_random(n = input$n)
  })
  #show the data in a table
  output$filtered_table <- DT::renderDataTable({
    req(brewery_data())
    brewery_data()
  })
  
  #show the data in a table
  output$random_table <- DT::renderDataTable({
    req(random_brewery())
    random_brewery()
  })
  
  # Allowing the user to download the data as a csv
  #filtered option
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      'Filtered_Brewery_Data.csv'
    },
    content = function(file) {
      readr::write_csv(brewery_data(), file)
    }
  )
  #random option
  output$download_random_data <- downloadHandler(
    filename = function() {
      'Random_Brewery_Data.csv'
    },
    content = function(file) {
      readr::write_csv(random_brewery(), file)
    }
  )
  
  #contingency table
  output$contingency_table <- renderTable(
    table(brewery_data()[[input$var_1]], brewery_data()[[input$var_2]])
  )
  
  #numeric summaries
  output$numeric_summaries <- DT::renderDataTable(
    brewery_data() %>%
      mutate(name_length = nchar(name)) %>%
      group_by(.data[[input$var]]) %>%
      summarise(`Minimum Name Length` = min(name_length, na.rm = TRUE),
                `Average Name Length` = mean(name_length, na.rm = TRUE),
                `Std. Dev. Name Length`= sd(name_length, na.rm = TRUE),
                `Maximum Name Length` = max(name_length, na.rm = TRUE))
  )
  
  #graphical plots
  output$all_plots <- renderPlot( {
    df <- brewery_data() %>%
      filter(state %in% state.name)
    #require a plot type
    req(input$plot_type)
    #bar chart
    if (input$plot_type == 'Bar Chart') {
      g <- ggplot(df, aes(x = brewery_type), fill = state) +
        geom_bar(fill = 'blue') +
        labs(title = 'Brewery Type Bar Chart',
             x = 'Brewery Type',
             y = 'Count') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    }
    #boxplot
    else if (input$plot_type == 'Boxplot') {
      #get name length
      df_name <- df %>%
        filter(!is.na(name), !is.na(state)) %>%
        mutate(name_length = nchar(name))
      
      g <- ggplot(df_name, aes(x = brewery_type, y = name_length)) +
        geom_boxplot(fill = 'orangered') +
        labs(title = 'Boxplot of Name Length By Brewery Type',
            x = 'Type',
            y = 'Name Length (Number of Characters)') +
        theme_minimal() +
        # Slanting the x-axis labels so they will fit on the plot
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    }
    #map locations
    else if (input$plot_type == 'Location') {
      #get the map
      usa <- map_data("state")
      #clean the data
      df_loc <- df %>%
        filter(!is.na(latitude), !is.na(longitude)) %>%
        mutate(state = tolower(state))
      
      g <- ggplot() + 
        geom_polygon(data = usa, aes(x = long, y = lat, group = group),
                          fill = 'white', color = 'black') +
        geom_point(data = df_loc, aes(x = longitude, y = latitude, color = brewery_type),
                   alpha = 0.75, size = 2) +
        labs(title = 'Map of Breweries',
             x = 'Longitude',
             y = 'Latitude') +
        theme_minimal()
      
    }
    #heatmap
    else if (input$plot_type == 'Heatmap') {
      df_heat <- df %>%
        filter(!is.na(state), !is.na(brewery_type)) %>%
        count(state, brewery_type)
      
      g <- ggplot(df_heat, aes(x = brewery_type, y = state, fill = n)) +
        geom_tile(color = 'white') +
        scale_fill_gradient(low = 'lightgreen', high = 'darkgreen') +
        labs(title = 'Heatmap of State and Brewery Type',
             x = 'Brewery Type',
             y = 'State') +
        theme_minimal() +
        theme(axis.text.y = element_text(hjust = 1, size = 10))
    }
    
    #add facet capability 
    if (input$facet_option != 'None') {
      g <- g + facet_wrap(as.formula(paste('~', input$facet_option)))
    }
    g
  }
  )
  
}

#Run the app
shinyApp(ui = ui, server = server)