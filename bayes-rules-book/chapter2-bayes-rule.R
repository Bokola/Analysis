
# packages ----------------------------------------------------------------


ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("bayesrules", "tidyverse", "janitor")
ipk(list.pkg)

# ipk = function(pkg) {
#   new.pkg = list.pkg[!(list.pkg %in% .packages(all.available = TRUE))]
#   if (length(new.pkg))
#     install.packages(new.pkg, dependencies = TRUE, repos = 'https://cran.us.r-project.org')
#   ddpcr::quiet(sapply(pkg, require, character.only = TRUE))
# }
# list.pkg = c('magrittr',
#              'tidyverse',
#              'rvest',
#              'knitr',
#              'httr',
#              'jsonlite')
# 
# ipk(list.pkg)