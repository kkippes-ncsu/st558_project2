# Project 2

Project: Making an app in RShiny

Purpose: This app accesses an API from the Open Brewery Database. It gives details about this API, allows the user to access certain data sets from this API, and then allows the user to subset the data sets and download a CSV file. After this, the user is able to access contingency tables, numerical summaries, and graphs from certain data sets that have been selected.

Packages Required to run App: shiny, bslib, httr, tidyverse, ggplot2, jsonlite, DT, maps

Line to install these packages: install.packages(c('shiny','bslib', 'httr', 'tideyverse', 'ggplot2', 'jsonlite', 'DT', 'maps'))

shiny::runGitHub(repo = 'st558_project2', username = 'kkippes-ncsu', subdir = 'myapp', ref = 'main')