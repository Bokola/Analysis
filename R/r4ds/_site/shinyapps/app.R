

options(scipen = 999) # off scintific notation
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
list.of.pkgs = c('sf', 'viridis', 'geoR', 'cholera', 'rgdal', "leaflet", "mapview","DT", "tmap", "ggplot2", "tidyverse",
                 "RColorBrewer", "INLA", "SpatialEpi", "spdep", "rnaturalearth", "flexdashboard", "DT", "dygraphs", "wbstats", "shiny")
ipk(list.of.pkgs)

home = ifelse(Sys.info()["sysname"] == "Linux", Sys.getenv("home"), Sys.getenv("USERPROFILE"))
home = gsub("\\\\", "/", home)


app_dir = path.expand(file.path(home, "Analysis", "R", "r4ds", "shinyapps"))

setwd(app_dir)


# ui ----------------------------------------------------------------------


ui = fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Made with", a(
        "shiny", href = "http://shiny.rstudio.com"
      ), "." ),
      img(
        src = "Shiny.png", width = "70px", height = "70px"
        
      )
    ),
    mainPanel("main panel for outputs")
  )
)

#server()

server = function(input, output){}
getwd()
#shinyApp()

shinyApp(ui = ui, server = server)