#shiny app components
#</ui and server>
cat('Building UI and Server ....!\n\n')

#ui
ui = fluidPage(
  titlePanel("BC Liquor Prices"),
  sidebarLayout(
    sidebarPanel('our inputs will go here'),
    mainPanel('our results will go here')
  )
)


# Add inputs to the ui
#inputs are what gives a user a way to interact with a shiny app.
# include 
#1. actionButton()
#2 checkboxInput()
#3 checkboxGroupInput()
#4 dateInput()
#5 colourpicker::colourInput()
#6 dateRangeInput()
#7 fileInput()
#8 numericInput()
#9 passwordInput()
#10 radioButtons()
#11 selectInput()
#12 sliderInput()
#13 textInput()
#14 textAreaInput()

ui_1 = fluidPage(
  titlePanel('BC Liquor Prices'),
  sidebarLayout(
    sidebarPanel(
      # input for price
      sliderInput('priceInput', 'Price', min = 0, max = 100, value = c(25, 40), pre = '$'), 
      #input for type 
      radioButtons('typeInput', 'Product Type', choices = c('BEER', 'REFRESHMENT', 'SPIRITS', 'WINE'),
                   selected = 'WINE'),
      # select box input for country
      selectInput('countryInput', 'Country', choices = c('CANADA', 'FRANCE', 'ITALY'))
      
    ),
    mainPanel('Our results will go here')
  )
)

#Adding a placeholder for outputs
#After creating all inputs we have to add elements to the UI to display the outputs.
# Outputs can be any R generated objects such as plots, tables, text
#We are still bulding the UI, so at this point we can only add placeholders for the outputs that will determine where an output
#will be and what its ID is, but it won't actually show anything.

ui_2 = fluidPage(
  titlePanel('BC Liquor Prices'),
  sidebarLayout(
    sidebarPanel(
      # input for price
      sliderInput('priceInput', 'Price', min = 0, max = 100, value = c(25, 40), pre = '$'), 
      #input for type 
      radioButtons('typeInput', 'Product Type', choices = c('beer', 'refreshment', 'spirits', 'wine'),
                   selected = 'wine'),
      # select box input for country
      selectInput('countryInput', 'Country', choices = c('canada', 'france', 'italy'))
      
    ),
    mainPanel(
      plotOutput('coolplot'), #placeholder for plot output
      br(), br(),
      tableOutput('results') #placeholder for table output
      
    )
  )
)



# implementing server logic to create outputs

# remember we created 2 output placeholders: coolplot(a plot) and results(a table)
#we now build an output and tell shiny what kind of table or plot to display
#3 rules to build an output in china:
#a). save the output object into the `output` list(remember every server function has an `output` argument)
#b). Build the object with a `render*` function, where `*` is the type of output
#c). Access input values using tht `input` list(every server function has an `input` argument)

#Building the plot output

server = function(input, output){
  output$coolplot <- renderPlot({
    filtered = bcl %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             country == input$countryInput
             )
    ggplot(filtered, aes(alcohol_content)) +
      geom_histogram()
  })
}

#Building the table output

server = function(input, output){
  output$coolplot <- renderPlot({
    filtered = bcl %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             country == input$countryInput
      )
    ggplot(filtered, aes(alcohol_content)) +
      geom_histogram()
  })
  output$results <- renderTable({
    filtered = bcl %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             country == input$countryInput
      )
    filtered
  })
  observe({ print(input$priceInput) })
  priceDiff = reactive({ # using reactive({})
    diff(input$priceInput)
  
  })
  observe({ print(priceDiff()) })
}

#shinyApp(ui = ui_2, server = server)

#creating and accessing reactive variables
# we access reactive input vars using observe({})
#You caan also create your own reactive variables using the reactive({}) function. 
#if you want to access a reative variable defined with `reactive({})`, you must add parentheses after the variable #name, as if it is a function e.g `observe({ print(priceDiff()) })`

## uisng reactive variables to reduce code duplication
# Notice we use the filtering code twice, we can then define a reacive variable that will hold the filtered dataseet.
# in order to access the value of a reactive variable, you must follow the name with a parenthesis
server = function(input, output){
  filtered = reactive({
    bcl %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             type == input$typeInput,
             country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    ggplot(filtered(), aes(alcohol_content)) +
      geom_histogram()
  })
  output$results <- renderTable({
    filtered()
  })
  observe({ print(input$priceInput) })
  priceDiff = reactive({ # using reactive({})
    diff(input$priceInput)
    
  })
  observe({ print(priceDiff()) })
}

shinyApp(ui = ui_2, server = server)

#using uiOutput() to create UI elements dynamically
#one of the outputs you can add to the UI is `uiOutput()`.
#this is an output used to render more UI
#it is usually used to create inputs(or any other UI) from the server,or in other words you can create inputs dynamically

