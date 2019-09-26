#shiny apps
ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  if('tidyr' %in% list.of.pkgs){
    devtools::install_github("tidyverse/tidyr",force = T)
  }
  if(!'patchwork' %in% .packages(all.available = TRUE)){
    devtools::install_github("thomasp85/patchwork")
  }
  if(!'ReporteRsjars' %in% .packages(all.available = TRUE)){
    devtools::install_github('davidgohel/ReporteRsjars')
  }
  if(!'ReporteRs' %in% .packages(all.available = TRUE)){
    devtools::install_github('davidgohel/ReporteRs')
  }
  
  sapply(pkg, require, character.only = T)
}
list.of.pkgs = c('devtools', 'Rcpp', 'digest', 'backports', 'tidyverse',  'tidyr', 'dplyr', 'ggplot2', 'magrittr','tm','SnowballC','RColorBrewer','patchwork','cowplot','gridExtra','ReporteRs', 'shiny')
ipk(list.of.pkgs)

# shiny app has 3 parts; user interface(ui), server function and a call to the function
# define  UI for app that draws a histogram
ui = fluidPage(
  #App title ----
  titlePanel('Hello Shiny!'),
  #sidebar layout with input and output definitions ----
  sidebarLayout(
    #sidebar panel for inputs ----
    sidebarPanel(
      #inputs: slider for the number of bins----
      sliderInput(inputId = 'bins',
                  label = 'Number of bins:',
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    #main panel for displaying outputs ----
    mainPanel(
      #output: Histogram ----
      plotOutput(outputId = 'distPlot')
    )
  )
)
#server
#define server logic required to draw a histogram ----
server = function(input, output){
  output$distPlot = renderPlot({
    x = faithful$waiting
    bins = seq(min(x), max(x), length.out = input$bins+1)
    hist(x, breaks = bins, col = '#75AADB', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting time')
  })
}
# run the app
shinyApp(ui = ui, server = server)

#lesson 2 - html contents
# shiny function 	HTML5 equivalent 	creates
# p 	<p> 	A paragraph of text
# h1 	<h1> 	A first level header
# h2 	<h2> 	A second level header
# h3 	<h3> 	A third level header
# h4 	<h4> 	A fourth level header
# h5 	<h5> 	A fifth level header
# h6 	<h6> 	A sixth level header
# a 	<a> 	A hyper link
# br 	<br> 	A line break (e.g. a blank line)
# div 	<div> 	A division of text with a uniform style
# span 	<span> 	An in-line division of text with a uniform style
# pre 	<pre> 	Text 'as is' in a fixed width font
# code 	<code> 	A formatted block of code
# img 	<img> 	An image
# strong 	<strong> 	Bold text
# em 	<em> 	Italicized text
# HTML 	  	Directly passes a character string as HTML code

#lesson 3 - widgets

# The standard Shiny widgets are:
#   function 	widget
# actionButton 	      Action Button
# checkboxGroupInput 	A group of check boxes
# checkboxInput 	    A single check box
# dateInput 	        A calendar to aid date selection
# dateRangeInput 	    A pair of calendars for selecting a date range
# fileInput 	        A file upload control wizard
# helpText 	          Help text that can be added to an input form
# numericInput 	      A field to enter numbers
# radioButtons 	      A set of radio buttons
# selectInput 	      A box with choices to select from
# sliderInput 	      A slider bar
# submitButton 	      A submit button
# textInput 	        A field to enter text

# To add a widget to your app, place a widget function in sidebarPanel or mainPanel in your ui object.

# Example app: using HTML tags to layout your app - basic page
server = function(input, output, session){
  
}
ui = basicPage(
  h1("Title without tags$"),
  tags$blockquote("But block qoutes require tags$ because it is less common than h3, h1 or code"),
  h3("h3 is fine without tags and so is code here"),
  code("data.frame(a = 1:10, b = 10:1)")
)

shinyApp(ui = ui, server = server)
# example app: using Bootstrap's grid system to lay app

server = function(input, output, session){
  
}
ui = fluidPage(
  fluidRow(
    column(6, offset = 1,
           h1("Title in one row")
      
    )
    
  ),
  fluidRow(
    column(1,
           actionButton("button", "click")
           ),
    column(6,
           p("Row 2, Column 2 (button is col 1)")
           )
  )
)
shinyApp(server = server, ui = ui)

# Example app: use a pre-existing Bootstrap theme
library(shinythemes)
server = function(input, output, session){
  
}
ui = fluidPage(theme = shinytheme("cosmo"),
               titlePanel("Use an existing theme"),
               
               sidebarLayout(
                 
                 sidebarPanel(
                   h3("Note the button is black. This is different from the previous app."),
                   actionButton("button", "A button")
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Plot"),
                     tabPanel("Summary"),
                     tabPanel("Table")
                   )
                 )
               )
            )
shinyApp(ui = ui, server = server)
# Example app: using inline CSS in the HTML head
#Note that the actual style code is within an HTML function - this prevents Shiny from treating the text as "regular" text and escaping the strings.

server = function(input, output, session){
  
}
ui = basicPage(
  # this is your web page header information
  tags$head(
    #here you include your inline styles
    tags$style(HTML("
                         body {
                            background-color: cornflowerblue;
                            color: #6B1413;
                         
                         }
                         "))
  ),
  h3("CSS using the HTML tag"),
  p("Some important text")
  
)
shinyApp(ui = ui, server = server)

