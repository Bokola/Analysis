rm(list = ls(all = T))
#setwd("C:\\Users\\bokola\\Google Drive\\Data")
ipk <- function(pkg){
  new.pkg <- list.of.pkg[!(list.of.pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
  
}
list.of.pkg <- c("plyr", "tidyverse", "repr", "magrittr","DBI", "RMySQL", "readxl")
ipk(list.of.pkg)
options(repr.plot.width = 4, repr.plot.height = 3.5)

#paths
hom.dir = ifelse(Sys.info()["sysname"] == "Windows", Sys.getenv("USERPROFILE"), Sys.getenv("HOME"))
project.path = path.expand(file.path(hom.dir
                                     ,"Analysis"
                                     ,"tiba")) %>% gsub("\\\\", "/", .)

data.path = path.expand(file.path(project.path
                                  ,"Data"))
scripts.path = path.expand(file.path(project.path
                                     ,"src"
                                     ,"r"))
report.path = path.expand(file.path(project.path
                                    ,"cache"
                                    ,"doc"))
if(!file.exists(project.path)){
  if(dir.create(project.path, recursive = T))
    stop("The project directory \"",
         project.path, "\"has been created!\nPlease fill it with relevant files and folders!")
  else
    stop("The project directory\"",
         project.path,
         "\"could not be created!")
}
if(!file.exists(data.path)){
  if(dir.create(data.path, recursive = T))
    stop("The project directory \"",
         data.path, "\"has been created!\nPlease fill it with relevant files and folders!")
  else
    stop("The project directory\"",
         data.path,
         "\"could not be created!")
}
if(!file.exists(report.path)){
  if(dir.create(report.path, recursive = T))
    stop("The project directory \"",
         report.path, "\"has been created!\nPlease fill it with relevant files and folders!")
  else
    stop("The project directory\"",
         report.path,
         "\"could not be created!")
}
if(!file.exists(scripts.path)){
  if(dir.create(scripts.path, recursive = T))
    stop("The project directory \"",
         scripts.path, "\"has been created!\nPlease fill it with relevant files and folders!")
  else
    stop("The project directory\"",
         scripts.path,
         "\"could not be created!")
}
