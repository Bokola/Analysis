#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# mine ----
ui<-fluidPage(
  #App title ----
  titlePanel("Hello Shiny!"),
  # sidebar layout with input output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    # main panel for displaying outputs ----
    mainPanel(
      # output: Histogram ----
      plotOutput(outputId = "disPlot"),
      plotOutput(outputId = "disPlot2")
    )
  )
  
)
