#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

ipk<-function(pkg) {
  new.pkg<-list.of.pkgs[!(list.of.pkgs %in% installed.packages()[,"Package"])]
  if(length(new.pkg)) install.packages(new.pkg,repos = "https://cran.us.r-project",dependencies = T)
  sapply(pkg,require,character.only=T)
}
list.of.pkgs<-c("ggplot2","shiny","dplyr")
ipk(list.of.pkgs)

# Define UI for app that draws a histogram ----
ui<-fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebarpanel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins", label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    
   
    ),
    # Main panel for dispalying outputs ---
    mainPanel(
      # Output: Histogram ----
      plotOutput("distPlot")
     
    )
  )
)
# Define server logic required to draw a histogram ----
server<- function(input,output) {
  # Hist of the faithful geyser data
  # with requested nummber of bins
  # This expression that generates a hist is wrapped in a call
  # to renderPlot to indicate that:
  #
  #1. it is reactive and therefore should be automatically 
  # re-excecuted when the inputs (inputs$bins) change
  #
  #2. Its output type is a plot
  
  output$distPlot <- renderPlot ({
    x <- faithful$waiting
    bins <- seq(min(x),max(x),length.out = input$bins+1)
    
    hist(x,breaks=bins,col="#75AADB",border="white",xlab="Waiting time before next eruption in minutes",main="Histogram of waiting time")
  })
  }

shinyApp(ui = ui, server = server)

