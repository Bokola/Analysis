#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

ipk <- function(pkg) {
  new.pkgs <- list.of.pkgs[list.of.pkgs !(installed.packages()[,"Package"])]
  if(length(new.pkgs)) install.packages(new.pkg, repos = 'https://cran.us.r-project.org', dependencies = T)
  sapply(pkg, require, character.only = T)
}
list.of.pkgs <- c("shiny","tidyverse","magrttr")
ipk(list.of.pkgs)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

