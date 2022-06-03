.libPaths()
ipk <- function(pkg) {
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if (length(new.pkg))
    install.packages(new.pkg,
                     repos = "https://cran.us.r-project.org",
                     dependencies = T,
                     lib = .libPaths()[[1]])
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("tidyverse", "magrittr", "rsconnect")
ipk(list.pkg)

# authorize rconnect to access shinyapps.io
browseURL("https://docs.rstudio.com/shinyapps.io/getting-started.html")
rsconnect::setAccountInfo(name = 'bokola',
                          token = 'F8DD64A33717C54BA5B5AD1664C89CAC',
                          secret = 'r5gBSUltMB+oCfIqbd0kWRZ0ORLksjtH3y3HdX0X')
# deploy
home.dir <- ifelse(.Platform$OS.type == "windows", Sys.getenv("USERPROFILE"),
         Sys.getenv("HOME")) %>% gsub("\\\\", "/",.)
app.dir <- file.path(home.dir, "Analysis", "shiny", "deploy")
rsconnect::deployApp(app.dir)
