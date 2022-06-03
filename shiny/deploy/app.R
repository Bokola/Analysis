#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# ipk <- function(pkg) {
#   new_pkg <- list_pkg[!(list_pkg %in% .packages(all.available = T))]
#   if (length(new_pkg))
#     install.packages(new_pkg, dependencies = T,
#                      repos = "https://cran.us.r-project.org",
#                      lib = .libPaths()[[1]])
#   sapply(pkg, require, character.only = T)
# }
# list_pkg <- c(
#   "gapminder", "ggforce", "gh", "globals", "openintro", "profvis",
#   "RSQLite", "shiny", "shinycssloaders", "shinyFeedback",
#   "shinythemes", "testthat", "thematic", "tidyverse", "vroom",
#   "waiter", "xml2", "zeallot"
# )
# ipk(list_pkg)


# use library() not install.packages() for shinyapps.io apps --------------

library(gapminder)
library(ggforce)
library(gh)
library(globals)
library(openintro)
library(profvis)
library(RSQLite)
library(shiny)
library(shinycssloaders)
library(shinyFeedback)
library(shinythemes)
library(testthat)
library(thematic )
library(tidyverse)
library(vroom)
library(waiter)
library(xml2)
library(zeallot)

ui <- fluidPage( # layout function that sets up the basic str of page
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"), # for code output
  tableOutput("table")
)
#outputs are added on server with output$id assignments
server <- function(input, output, session){
  output$summary <- renderPrint({
    # print results of an expression
    dataset <- get(input$dataset, "package:datasets") #get by name
    summary(dataset)
  })
  # we assigned dataset object twice
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
  
}
# you create a reactive expression by wrapping block of code in reactive({...})
server <- function(input, output, session){
  # create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  output$summary <- renderPrint({
    # use a reactive expression by calling it like a function
    summary(dataset())
    output$table <- renderTable({
      dataset()
    })
  })
}
shinyApp(ui, server)

