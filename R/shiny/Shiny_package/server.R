#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# server logic to draw a histogram ----
server<-function(input,output) {
  output$disPlot<-renderPlot({
    x<-faithful$waiting
    bins<-seq(min(x),max(x),length.out = input$bins +1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  output$disPlot2<-renderPlot({
    x = rep(1:10)
    y = rep(10:1)
    qplot(x,y)
    
  })
  
}
