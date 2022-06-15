library(shiny)
# Replace the UI and server components of your app from the previous
# exercise with the UI and server components below, run the app,
# and describe the app's functionality. Then reduce the duplication
# in the app by using a reactive expression.
ui <- fluidPage(
  sliderInput("x", label = 'if x is', min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 60, value = 5),
  "then x * y is", textOutput("product"),
  "and, (x*y) + 5 is", textOutput("product_plus5"),
  "and (x*y) + 10 is", textOutput("product_plus10")
)
server <- function(input, output, session){
  output$product <- renderText({
    product <- input$x * input$y
    product
  })
  output$product_plus5 <- renderText({
    product_plus5 <- input$x * input$y + 5
    product_plus5
  })
  output$product_plus10 <- renderText({
    product_plus10 <- input$x * input$y + 10
    product_plus10
  })

}
shinyApp(ui, server)

# reducing duplicated code with reactive({...})
server <- function(input, output, session){
  product <- reactive({
    input$x * input$y
  })
  output$product <- renderText({
    product()
  })
  output$product_plus5 <- renderText({
    product() + 5
  })
  output$product_plus10 <- renderText({
    product() + 10
  })
}
shinyApp(ui, server)
