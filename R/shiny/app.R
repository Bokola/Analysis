ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  if('d3treeR' %in% list.of.pkgs){
    remotes::install_github("d3treeR/d3treeR")
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
  
  if(!'INLA' %in% .packages(all.available = TRUE)){
    install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
  }
  
  sapply(pkg, require, character.only = T)
}

list.of.pkgs = c("gapminder", "ggforce", "openintro", "shiny", "shinyFeedback", 
                 "shinythemes", "tidyverse", "vroom", "waiter", "ggplot2", "tidyverse",
                 "RColorBrewer", "INLA", "SpatialEpi", "spdep", "rnaturalearth", "flexdashboard", "DT", "dygraphs", "wbstats", "shiny", "xts")
ipk(list.of.pkgs)

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}

shinyApp(ui = ui, server =server)

server = function(input, output, session){
  dataset = reactive({
    get(input$dataset, "package:datasets")
    
    output$summary = renderPrint({
      summary(dataset())
    })
    
    output$table = renderTable({
      dataset()
      
    })
  })
}

shinyApp(ui = ui, server =server)

ui_1 = fluidPage(
  textInput("name", "What is your name"),
  textOutput("name")
)

server_1 = function(input, output, session){
  output$name = renderText({
    paste0("Hello ", input$name)
  })
}
shinyApp(ui=ui_1, server = server_1)

library(ggplot2)
datasets <- data(package = "ggplot2")$results[c(2, 4, 10), "Item"]

ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summmry <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset()[[3]], dataset()[[4]])
  })
}
shinyApp(ui = ui, server =server)

ui = fluidPage(
  sliderInput("date", label = "When should we deliver", min = as.Date("2020-06-10"), max = as.Date("2021-05-31"), value = as.Date("2020-06-11"), timeFormat = "%F")
)
server = function(input, output, session){
  
}
shinyApp(ui = ui, server = server)


# Inputs ---------------------------------------------------------------------

# Free texts

ui = fluidPage(
  textInput("username", "What is your username?"),
  passwordInput("password", "What is your password?"),
  textAreaInput("bio", "Tell me about yourself", rows = 3)
)

server = function(input, output, session){
  
}

shinyApp(ui = ui, server = server)

# Numeric inputs

ui = fluidPage(
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10,20), min = 0, max = 100)
  
)
shinyApp(ui = ui, server = server)

# Dates
ui = fluidPage(
  dateInput("dob", "When were you born"),
  dateRangeInput("holiday", "When do you want to go on vacation next?")
)
shinyApp(ui,server)
