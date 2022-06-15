library(shiny)
# Extend the app from the previous exercise to allow the user
# to set the value of the multiplier, y, so that the app
# yields the value of x * y. The final result should look like this:
ui <- fluidPage(
  sliderInput("x", label = 'if x is', min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 60, value = 5),
  "then x times y is", # raw text can go within the fluidpage
  textOutput("product")
)
server <- function(input, output, session){
  output$product <- renderText({
    input$x * input$y
  })
}
shinyApp(ui, server)
