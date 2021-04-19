options(shiny.maxRequestSize = 400 * 1024^2)
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
  if(!"thematic" %in% .packages(all.available = TRUE)) {
    remotes::install_github("rstudio/thematic")
  }
  
  if(!'INLA' %in% .packages(all.available = TRUE)){
    install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
  }
  
  sapply(pkg, require, character.only = T)
}

list.of.pkgs = c("gapminder", "ggforce", "openintro", "shiny", "shinyFeedback", 
                 "shinythemes", "tidyverse", "vroom", "waiter", "ggplot2", "tidyverse",
                 "RColorBrewer", "INLA", #"SpatialEpi", "spdep",
                 "rnaturalearth", "flexdashboard", 
                 "DT", "dygraphs", "wbstats", "shiny", "xts", "magrittr", "cowplot", "Cairo", "cairoDevice", 
                 "thematic", "shinyFeedback")
ipk(list.of.pkgs)

home = ifelse(Sys.info()["sysname"] == "Linux", Sys.getenv("home"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\", "/",.)

home = file.path(home, "Analysis", "R", "shiny")
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

# Limited choices - selectInput & radioButtons
animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")
ui = fluidPage(
  selectInput("state", "What's your favourite state", choices = state.name),
  radioButtons("animal", "What's your favourite animal", animals)
)
shinyApp(ui, server)

ui = fluidPage(
  radioButtons("rb", "Choose one:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry", "happy", "sad"))
)
shinyApp(ui, server)

ui = fluidPage(
  checkboxGroupInput("animal", "What animals do you like?", animals)
) # multiple choices
shinyApp(ui, server)

ui = fluidPage(
  checkboxInput("cleanup", "Clean up?", value = TRUE),
  checkboxInput("shutdown", "Shutdown")
)
shinyApp(ui, server)

# File uploads

ui = fluidPage(
  fileInput("upload", NULL)
)
shinyApp(ui, server)
# Action buttons

ui = fluidPage(
  actionButton("click", "click me!"),
  actionButton("drink", "Drink me", icon = icon("cocktail"))
)
shinyApp(ui, server)

# outputs -----------------------------------------------------------------

# text

ui = fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)
server = function(input, output, session){
  output$text = renderText({
    "Hello friend!"
  })
  output$code = renderPrint({
    summary(1:10)
  })
}
shinyApp(ui, server)
# Tables

ui = fluidPage(
  tableOutput("statistic"),
  dataTableOutput("dynamic")
)
server = function(input, output, session){
  output$statistic = renderTable({
    head(mtcars)
  })
  output$dynamic = renderDataTable(mtcars, options = list(pageLength = 10))
}
shinyApp(ui, server)
# plots

ui = fluidPage(
  plotOutput("plot", width = "400px")
)

server = function(input, output, session){
  output$plot = renderPlot(plot(1:5), res = 96)
}
shinyApp(ui, server)


# Layouts -----------------------------------------------------------------

fluidPage(
  titlePanel(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# #### --------------------------------------------------------------------


# page with sidebar
 ui = fluidPage(
  titlePanel(
   " # tiltle of your app"
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
    ),
    mainPanel(
      # outputs
    )
  )
)

# simple app for central liit theorem -------------------------------------

ui = fluidPage(
  titlePanel(
    "Central limit theorem"
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)
server = function(input, output, session){
  output$hist = renderPlot({
    means = replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}
shinyApp(ui, server)

# Multi-row
# fluidPage(
#   fluidRow(
#     column(4, 
#            ...
#     ),
#     column(8, 
#            ...
#     )
#   ),
#   fluidRow(
#     column(6, 
#            ...
#     ),
#     column(6, 
#            ...
#     )
#   )
# )


# Themes ------------------------------------------------------------------

theme_demo = function(theme){
  fluidPage(
    theme = shinythemes::shinytheme(theme),
    sidebarLayout(
      sidebarPanel(
        textInput("txt", "Text input:", "text here"),
        sliderInput("slider", "Slider input:", 1, 100, 30)
      ),
      mainPanel(
        h1("Header 1"),
        h2("Header 2"),
        p("some text")
      )
    )
  )
}
theme_demo("darkly")
theme_demo("flatly")
theme_demo("sandstone")
theme_demo("united")
shinyApp(ui = theme_demo("united"), server)

library(ggplot2)

histogram <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  
  ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

# Basic reactivity --------------------------------------------------------

ui = fluidPage(
  fluidRow(
    column(4,
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "µ", value = 0, step = 0.1),
           numericInput("sd1", label= "σ", value = 0.5, min=0.1, step = 0.1)),
    column(4, 
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "µ", value = 0, step = 0.1),
           numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Histogram",
           numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
           sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)

server = function(input, output, session){
  output$hist = renderPlot({
    x1 = rnorm(input$n1, input$mean1, input$sd1)
    x2 = rnorm(input$n2, input$mean2, input$sd2)
    histogram(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$test = renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    t_test(x1, x2)
  })
}
shinyApp(ui, server)
  
# Reactivity

server = function(input, output, session){
  x1 = reactive(rnorm(input$n1, input$mean1, input$sd1))
  x2 = reactive(rnorm(input$n2, input$mean2, input$sd2))
  
  output$hist = renderPlot({histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)},res = 96)
  
  output$ttest = renderText(t_test(x1(), x2()))
}
shinyApp(ui, server)

# Timed invalidation
# We can increase the frequency of updates with a new function `reactiveTimer()`

server = function(input, output, session){
  timer = reactiveTimer(500)
  
  x1 = reactive({
    timer()
    
    rpois(input$n, input$lambda1)
  })
  
  x2 = reactive({
    timer()
    rpois(input$n, input$lambda2)
  })
  
  output$hist = renderPlot({histogram(x1(), x2(), binwidth = 1, xlim = c(0,40))},res = 96)
  
  # output$ttest = renderText(t_test(x1(), x2()))
}
shinyApp(ui, server)

# On click

ui = fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
           ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  x1 <- reactive({
    input$simulate
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    input$simulate
    rpois(input$n, input$lambda2)
  })
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}
shinyApp(ui, server)
# using eventReactive() to avoid creating more dependencies
# `eventReactive()` has two arguments: the first argument specifies what to take a dependency on, and the second argument specifies what to compute

server = function(input, output, session){
  x1 = eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  
  x2 = eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  
  output$hist = renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0,40))
  }, res = 96)
}
shinyApp(ui, server)

# Observers

# much like eventReactive() with `eventExpr` and `handlerExpr` arguments
# gives you an important debugging tool when first learning shiny

server = function(input, output, session) {
  text = reactive(paste0("Hello ", input$name, "!"))
  
  output$greeting = renderText(text())
  observeEvent(input$name, {
    
    message("Greeting performed")
  })
}

# Chapter 5 Case study: ER injuries

# Data

source(file.path(home, "data.R"))

injuries = vroom::vroom(file.path(home, "data", "injuries.tsv.gz"))
names(injuries)
# `weight` is statistical weight giving the estimated number of people who would suffer this injury if this dataset was scaled to the entire population of the US.

population = vroom::vroom(file.path(home, "data", "population.tsv"))
products = vroom::vroom(file.path(home, "data", "products.tsv"))

# Exploration

injuries %>% count(prod_code, sort = T)
selected = injuries %>% filter(prod_code == 1842) 
selected %>% count(body_part, wt = weight, sort = T)
selected %>% count(location, wt = weight, sort = TRUE)
summary = selected %>%
  count(age, sex, wt = weight)
summary

summary %>% 
  ggplot(aes(age, n, color = sex)) +
  geom_line() +
  labs( y = "Estimated number of injuries")
# rate
summary = selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n/population * 1e4)
summary

summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")
# sample narratives

selected %>%
  sample_n(10) %>%
  pull(narrative)
# Prototype
# when building a complex app, start as simple as possible - to confirm the basic mechanics work before you start doing something more complicated

ui = fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", setNames(products$prod_code, products$title))
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

# `setNames()` in `selectInput()` `choices` shows the product name in the UI and returns the product code to the server.

server = function(input, output, session) {
  selected = reactive({
    injuries %>% filter(prod_code == input$code)
    })
  
  output$diag = renderTable({
    selected() %>%
      count(diag, wt = weight, sort = TRUE)
  })
  
  output$body_part = renderTable({
    selected() %>%
      count(body_part, wt = weight, sort = TRUE)
  })
  
  output$location = renderTable({
    selected() %>%
      count(location, wt = weight, sort = TRUE)
  })
  
  summary = reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex = renderPlot({
    summary() %>%
      ggplot(aes(age,n, color = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries") +
      theme_minimal_hgrid(12, rel_small = 1)
    
  }, res = 96)
}
shinyApp(ui, server)

# polish tables

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))
names(injuries)
# a function to lump

server = function(input, output, session) {
  selected = reactive({
    injuries %>% filter(prod_code == input$code)
  })
  
  count_top = function(df, var, n = 5) {
    df %>%
      mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
      group_by({{ var }}) %>%
      summarise(n = as.integer(sum(weight)))
  }
  
  # I made one other change to improve the aesthetics of the app: I forced all tables to take up the maximum width (i.e. fill the column that they appear in). This makes the output more aesthetically pleasing because it reduces the amount of incidental variation.
  
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary = reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex = renderPlot({
    summary() %>%
      ggplot(aes(age,n, color = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries") +
      theme_minimal_hgrid(12, rel_small = 1)
    
  }, res = 96)
}
shinyApp(ui, server)



ui = fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", setNames(products$prod_code, products$title))
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  # Rate vs count
  fluidRow(
    column(8,
           selectInput("code", "Product", choices = setNames(products$prod_code, products$title),
                       width = "100%"
      )
           
    ),
    column(12, selectInput("y", "Y axis", c("rate", "count")))
    # Narrative
    
  ),
  fluidRow(
    column(2,actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server = function(input, output, session) {
  selected = reactive({
    injuries %>% filter(prod_code == input$code)
  })
  
  count_top = function(df, var, n = 5) {
    df %>%
      mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
      group_by({{ var }}) %>%
      summarise(n = as.integer(sum(weight)))
  }
  
  # I made one other change to improve the aesthetics of the app: I forced all tables to take up the maximum width (i.e. fill the column that they appear in). This makes the output more aesthetically pleasing because it reduces the amount of incidental variation.
  
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary = reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  # 
  output$age_sex = renderPlot({
    if(input$y == "count") {
    summary() %>%
      ggplot(aes(age,n, color = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries") +
      theme_minimal_hgrid(12, rel_small = 1)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_minimal_hgrid(12, rel_small = 1)
    }
    
  }, res = 96)
  
  output$narrative = renderText({
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
  
  
}
shinyApp(ui, server)

# 7 Graphics
# A plot can respond to four different mouse events: click, dbClick, hover and brush(a rectangular selection tool). To turn these events into a Shiny input, you supply a string to the corresponding plotOutput() argument, e.g. `plotOutput("plot", click = "plot_click")`

ui = basicPage(
  plotOutput("plot", click = "plot_click"),
  verbatimTextOutput("info")
)

server = function(input, output) {
  output$plot = renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  }, res = 96)
  
  output$info = renderPlot({
    req(input$plot_click)
    x = round(input$plot_click$x, 2)
    y = round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
}
shinyApp(ui, server)

# Clicking

# Most iportant components are `x`, `y` which give the location of the event in data coordinates. Use the `nearPoints()` helper, which finds data points near the event, taking care of a bunch of fiddly details

ui = fluidPage(
  plotOutput("plot", click = "click"),
  tableOutput("data")
)

server = function(input, output, session) {
  output$plot = renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  }, res = 96)
  
  output$data = renderTable({
    nearPoints(mtcars, input$click, xvar = "wt", yvar = "mpg")
  })
}
# Here we give `nearPoints()` four arguments: the data frame that underlines the plot, the input event, and the names of the variables on the axes. If you use ggplot2, you only need to provide the first two arguments since `xvar` and `yvar` can be automatically imputed from the plot data structure. ggplot2 is used in subsequent plots

shinyApp(ui, server)

ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  tableOutput("data")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + 
      geom_point() +
      theme_minimal_hgrid(12, rel_small = 1)
  }, res = 96)
  
  output$data <- renderTable({
    nearPoints(mtcars, input$plot_click)
  })
}
shinyApp(ui, server)
# Another way to use `nearPoints()` is with `allRows = TRUE` and `addDist = TRUE`. 

# other point events
# The same approach works equally well with `click`, `dblClick`, and `hover`: just change the name of the argument.

# Brushing

# A brush is a rectangular selection defined by four edges. In Shiny, using a brush is straightforward once you've mastered `click` and `nearPoints()`. You just switch to `brush` argument and the `brushedPoints()` helper

ui = fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
)

server = function(input, output, session) {
  output$plot = renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() + theme_minimal_hgrid(12, rel_small = 1)
  }, res = 96)
  
  output$data = renderTable({
    brushedPoints(mtcars, input$plot_brush)
  })
}
shinyApp(ui, server)
# using `brushOpts()`
ui = fluidPage(
  plotOutput("plot", brush =brushOpts("plot_brush", fill = "green", direction = "x")),
  tableOutput("data")
)

server = function(input, output, session) {
  output$plot = renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() + theme_minimal_hgrid(12, rel_small = 1)
  }, res = 96)
  
  output$data = renderTable({
    brushedPoints(mtcars, input$plot_brush)
  })
}
shinyApp(ui, server)

# Modifying the plot
# using `reactiveVal()`

df = data.frame(x = rnorm(100), y = rnorm(100))
ui = fluidPage(
  plotOutput("plot", click = "plot_click")
)

server = function(input, output, session) {
  dist = reactiveVal(rep(1, nrow(df)))
  observeEvent(input$plot_click,
               dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)
)
  output$plot = renderPlot({
    df$dist = dist()
    ggplot(df, aes(x, y, size = dist)) +
      geom_point() +
      scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL) +
      theme_minimal_hgrid(12, rel_small = 1)
  })
}
shinyApp(ui, server)

# using brush

ui = fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
)

server = function(input, output, session) {
  selected = reactiveVal(rep(TRUE, nrow(mtcars)))
  
  observeEvent(input$plot_brush,{
    brushed = brushedPoints(mtcars, input$plot_brush, allRows = TRUE)$selceted_
    selected(ifelse(brushed, !selected(), selected()))
  })
  
  output$plot = renderPlot({
    mtcars$sel = selected()
    
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point(aes(colour = sel)) +
      scale_color_discrete(limits = c("TRUE", "FALSE"))
  }, res = 96)
}
shinyApp(ui, server)
# More on plotly package: https://plotly-r.com/

# Theming

library(thematic)
thematic_on(bg = "#222222", fg = "white", accent = "#0CE3AC")

library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_smooth()
# More on: https://rstudio.github.io/thematic/articles/Shiny.html

# Dynamic height and width

# You can make plot size reactive in response to user actions. To do this, supply zero-argument functions to the `width` and `height` arguments.
library(shiny)
ui = fluidPage(
  sliderInput("height", "height", min = 100, max = 500, value = 250),
  sliderInput("width", "width", min = 100, max = 500, value = 250),
  sliderInput("n", "n", min = 10, max = 100, value = 25),
  plotOutput("plot", width = 250, height = 250)
)

server = function(input, output, session) {
  output$plot = renderPlot(
    width = function() input$width,
    height = function() input$height,
    res = 96,
    {
      plot(rnorm(input$n), rnorm(input$n))
    }
  )
}
shinyApp(ui, server)

# Cached plots

# use `renderCachedPlot()` instead of `renderPlot()`. useful for complex plotting.
# you have to supply a `cacheKeyExpr` that uniquely identifies each plot

ui = fluidPage(
  selectInput("x", "X", choices = sort(names(diamonds)), selected = "carat"),
  selectInput("y", "Y", choices = sort(names(diamonds)), selected = "price"),
  plotOutput("diamonds")
)

server = function(input, output, session) {
  output$diamonds = renderCachedPlot({
    ggplot(diamonds, aes(.data[[input$x]], .data[[input$y]])) +
      geom_point()
  },
  cacheKeyExpr = list(input$x, input$y))
}
shinyApp(ui, server)

# more on https://shiny.rstudio.com/articles/plot-caching.html

# 8 User feedback
# Explores techniques like validation, notification, progress bars and confirmation dialogues or the ability to undo an action
#  uses `shinyFeedback` library

# a) Validating an input
# first add `shinyFeedback::useShinyFeedback()` in ui then call one of the feedback functions in your server function.i.e `feedback()`, `feedbackWarning()`, `feedbackDanger()` and `feedbackSuccess()`

ui = fluidPage(
  shinyFeedback::useShinyFeedback(),
  numericInput("n", "n", value = 10),
  textOutput("half")
)

server = function(input, output, session) {
  observeEvent(input$n,
               shinyFeedback::feedbackWarning(
                 "n",
                 input$n %% 2 != 0,
                 "Please select an even number"
      )
    )
  output$half = renderText(input$n / 2)
}
shinyApp(ui, server)
# use req() to avoid evaluation when condition not met

server = function(input, output, session) {
  half = reactive({
    even = input$n %% 2 == 0
    shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
    req(even)
    input$n / 2
  })
  output$half = renderText(half())
}
shinyApp(ui, server)

# req() and validation

ui = fluidPage(
  shinyFeedback::useShinyFeedback(),
  textInput("dataset", "Dataset name"),
  tableOutput("data")
)

server = function(input, output, session) {
  data = reactive({
    req(input$dataset)
    
    exists = exists(input$dataset, "package:datasets")
    shinyFeedback::feedbackDanger("dataset", !exists, "Unknown dataset")
    req(exists, cancelOutput = TRUE)
    get(input$dataset, "package:datasets")
  })
  output$data = renderTable({
    head(data())
  })
}
shinyApp(ui, server)

# validate an output with `validate()`
ui = fluidPage(
  numericInput("x", "x", value = 0),
  selectInput("trans", "transformation", choices = c("square", "log", "square-root")),
  textOutput("out")
)

server = function(input, output, session) {
  output$out = renderText({
    if (input$x < 0 && input$trans %in% c("log", "square-root")) {
      validate("x can not be negative for this transformation")
    }
    switch(input$trans,
           square = input$x ^ 2,
           "square-root" = sqrt(input$x),
           log = log(input$x)
      
    )
  })
}
shinyApp(ui, server)

# Notifications
# use notifications if there is no problem and you just want to let the user know what is happening

ui = fluidPage(
  actionButton("goodnight", "Good night")
 )

server = function(input, output, session) {
  observeEvent(input$goodnight,{
    showNotification("So long", type = "message")
    
    Sys.sleep(1)
    
    showNotification("Farewell")
    
    Sys.sleep(1)
    
    showNotification("Auf Wiedersehen", type = "error")
    
    Sys.sleep(1)
    
    showNotification("Adieu", type = "warning")
    
  })
}
shinyApp(ui, server)

# Reoving on completion
# using `duration = NULL` and `closeButton = FALSE` keeps notification visible until the task completes
# store the `id` returned by  showNotification()`, and then pas this value to `removeNotification()`. use `on.exit()` which ensures the notification ie removed regardless of how the task completes

server <- function(input, output, session) {
  data <- reactive({
    id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    read.csv(input$file$datapath)
  })
}
# More on: https://adv-r.hadley.nz/functions.html#on-exit

# progress updates

# multiple calls to `showNotification()` creates multiple notifications. You can instead update a single notification by capturing the `id` from the first call and using it in subsequent calls

ui = fluidPage(
  tableOutput("data")
)

server = function(input, output, session) {
  notify = function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
  }
  
  data = reactive({
    id = notify("Reading data...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    
    notify("Reticulating splines...", id = id)
    Sys.sleep(1)
    
    notify("Herding llmas...", id = id)
    Sys.sleep(1)
    
    notify("Orthogonolizing matrices...", id = id)
    Sys.sleep(1)
    
    mtcars
    
  })
  output$data = renderTable(head(data()))
}
shinyApp(ui, server)

# Progress bars

# Best for long running tasks

ui = fluidPage(
  numericInput("steps", "How many steps", 10),
  actionButton("go", "go"),
  textOutput("result")
)

server = function(input, output, session) {
  data = reactive({
    req(input$go)
    
    progress = Progress$new(max = input$steps)
    on.exit(progress$close())
    
    progress$set(message = "computing rando number")
    for (i in seq_len(input$steps)) {
      Sys.sleep(0.5)
      progress$inc(1)
    }
    
    runif(1)
  })
  output$result = renderText(round(data(), 2))
}
shinyApp(ui, server)

# Waiter

#  we add `use_waitress()` in the ui

ui = fluidPage(
  waiter::use_waitress(),
  numericInput("steps", "How many steps?", 10),
  actionButton("go", "go"),
  textOutput("result")
)

# then replace Progress with Waitress

server = function(input, output, session) {
  data = reactive({
    req(input$go)
    
    waitress = waiter::Waitress$new(max = input$steps)
    on.exit(waitress$close())
    
    for (i in seq_len(input$steps)) {
      Sys.sleep(0.5)
      waitress$inc(1)
    }
    runif(1)
  })
  output$result = renderText(round(data(), 2))
}
shinyApp(ui, server)

# spinners

# use when not sure how long an operation will take to asure the user that something is happening
# use `Waiter` in place of `Waitress`

ui = fluidPage(
  waiter::use_waiter(),
  actionButton("go", "go"),
  textOutput("result")
)

server = function(input, output, session) {
  data = reactive({
    req(input$go)
    
    waiter = waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    Sys.sleep(sample(5,1))
    runif(1)
  })
  output$result = renderText(round(data(), 2))
}
shinyApp(ui, server)

# like `Waitress` you can also use `Waiter` for specific outputs

ui = fluidPage(
  waiter::use_waiter(),
  actionButton("go", "go"),
  plotOutput("plot")
)

server = function(input, output, session) {
  data = reactive({
    req(input$go)
    waiter::Waiter$new(id = "plot")$show()
    
    Sys.sleep(3)
    data.frame(x = runif(50), y = runif(50))
  })
  output$plot = renderPlot(plot(data()), res = 96)
}
shinyApp(ui, server)

# using shinycssloaders
# it's a real deal

library(shinycssloaders)

ui = fluidPage(
  actionButton("go", "go"),
  withSpinner(plotOutput("plot"))
)

server = function(input, output, session){
  data = reactive({
    req(input$go)
    Sys.sleep(3)
    data.frame(x = runif(50), y = runif(50))
  })
  output$plot = renderPlot(plot(data()), res = 96)
}
shinyApp(ui, server)

# Confiring and undoing

# Explicit confirmation
# uses a dialogue box with `modalDialog()`

modal_confirm = modalDialog(
  "Are you sure you want to continue?",
  title = "Deleting files",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Delete", class = "btn btn-danger")
  )
)

ui = fluidPage(
  actionButton("delete", "Delete all files?")
)

server = function(input, output, session) {
  observeEvent(input$delete, {
    showModal(modal_confirm)
  })
  
  observeEvent(input$ok, {
    showNotification("Files deleted")
    removeModal()
  })
  
  observeEvent(input$cancel,
               removeModal()
               )
}

shinyApp(ui, server)

#  undoing an action

ui <- fluidPage(
  textAreaInput("message", 
                label = NULL, 
                placeholder = "What's happening?",
                rows = 3
  ),
  actionButton("tweet", "Tweet")
)

runLater = function(action, seconds = 3) {
  observeEvent(
    invalidateLater(seconds * 1000), action,
    ignoreInit = TRUE,
    once = TRUE,
    ignoreNULL = FALSE,
    autoDestroy = FALSE
  )
}

server = function(input, output, session) {
  waiting = NULL
  last_message = NULL
  
  observeEvent(input$tweet, {
    notification = glue::glue("Tweeted '{input$message}'")
    last_message <<- input$message
    updateTextAreaInput(session, "message", value = "")
    
    showNotification(
      notification,
      action = actionButton("undo", "Undo?"),
      duration = NULL,
      closeButton = FALSE,
      id = "tweeted",
      type = "warning"
    )
    
    waiting<<- runLater({
      cat("Actually sending tweet...\n")
      removeNotification("tweeted")
    })
  })
  observeEvent(input$undo, {
    waiting$destroy()
    showNotification("Tweet retracted", id = "tweeted")
    updateTextAreaInput(session, "message", value = last_message)
  })
}

shinyApp(ui, server)

# 9. Uploads and downloads

# uploads

ui = fluidPage(
  fileInput("file", NULL, accept = c(".docx", ".tsv", ".csv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
)

server = function(input, output, session) {
  data = reactive({
    req(input$file)
    
    ext = tools::file_ext(input$file$name)
    
    switch(ext,
           docx  = officer::read_docx(),
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$data$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .docx or .csv or .tsv")
      
    )
  })
  output$head = renderTable({
    head(data(), input$n)
  })
}
shinyApp(ui, server)

# Downloads
#  a prototype looks like this
# `filename` is a function with no arguments that returns a file name (as a string)
# `content` should be a function with one argument, `file`, which is a path to save the file.
ui = fluidPage(
  downloadButton("download1"),
  downloadLink("download2")
)

output$download = downloadHandler(
  filename = function() {
    paste0(input$dataset, ".csv")
    
  },
  content = function(file) {
    write.csv(data(), file)
  }
)

# Downloading data

home = ifelse(Sys.info()[["sysname"]] == "Linux", Sys.getenv("HOME"), Sys.getenv("USERPROFILE")) %>% gsub("\\\\", "/",.)

temp = list.files(file.path(home, "Downloads"),pattern="*.csv")
# dat = temp %>%
#   sub("\\..*", "\\1", .) %>%
#   gsub("-", "\\.",.) %>% tolower(.)
list2env(
  lapply(setNames(temp, tolower(make.names(gsub("\\..*", "", temp)))), function(x){
    x =read.csv(file.path(home, "Downloads", x))
    names(x) <- tolower(names(x))
    
    return(x)}),
  
  envir = .GlobalEnv)

ui = fluidPage(
  selectInput("dataset", "Pick a dataset", ls(.GlobalEnv)),
  tableOutput("preview"),
  downloadHandler("download", "Download .tsv")
)

server = function(input, output, session) {
  data = reactive({
    out = get(input$dataset, envir = .GlobalEnv)
    
    if (!is.data.frame(out)) {
      validate(paste0("'", input$dataset, "' is not a data frame"))
    }
    out
  })
  
  output$preview = renderTable({
    head(data())
  })
  
  output$download = downloadHandler(
    filename = function() {
      paste0(input$dataset, ".tsv")
    },
    
    content = function(file) {
      vroom::vroom_write(data(), file = file.path(home, "Downloads"))
    }
  )
}
shinyApp(ui, server)



