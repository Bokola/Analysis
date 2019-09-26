#shiny apps
rm(list = ls(all = T))


cat('setting project directories', '\n')
library(dplyr)

home_dir =ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))

project_dir = path.expand(file.path(home_dir
                                    ,'Analysis'
                                    ,'R'
                                    ,'shiny'
)) %>% gsub( '\\\\', '/',.)

data_dir = path.expand(file.path(project_dir, 'data')) %>%
  gsub("\\\\", '/',.)

src_dir = path.expand(file.path(project_dir
                                ,'src'
                                ,'r'
)) %>% gsub( '\\\\', '/',.)

cache_dir = path.expand(file.path(project_dir
                                  ,'cache'
                                  ,'doc'
)) %>% gsub( '\\\\', '/',.)

if(!file.exists(project_dir)){
  if(dir.create(project_dir, recursive = T))
    stop('The directory \'', project_dir, '\'has been created! \nPlease fill it with necessary files')
  else
    stop('The file directory\'', project_dir, '\'could not be created')
}

if(!file.exists(src_dir)) {
  if(dir.create(src_dir, recursive = T))
    stop("The file directory \"", src_dir, 
         "\"has been created! \nPleaase fill it with required files")
  else
    stop("The Directory \"", src_dir, "\"could not be created")
  
}

if(!file.exists(data_dir)){
  if(dir.create(data_dir, recursive = T))
    stop("The directory\"", data_dir, 
         "\"has been created!\nPlease fill it with required files")
  else
    stop("The directory", data_dir, 'could not be created')
}

if(!file.exists(cache_dir)){
  if(dir.create(cache_dir, recursive = T))
    stop("The directory\"", cache_dir, 
         "\"has been created!\nPlease fill it with required files")
  else
    stop("The directory", cache_dir, 'could not be created')
}


# Run scripts

run.app <- function(files) {
  run.app.internal <- function(file) {
    if(!file.exists(file)) {
      stop("The file \"", file, "\" does not exist!\n")
    }
    source(file, max.deparse.length =Inf, echo=FALSE)
  }
  invisible(sapply(files, run.app.internal))
}
allfiles <- sort(list.files(src_dir))
files <- allfiles[!allfiles %in% c("00_main.R","04_data_recoding.R", "05_reports.R", "shiny_intro.R")]
files <- files[order(files)]
files <- file.path(src_dir, files)
invisible(run.app(files))

files <- allfiles[allfiles %in% c("04_data_recoding.R", "05_reports.R")] %>%
  sort() %>%
  file.path(src_dir, .)
invisible(run.app(files)) #run files


#shinyApp(ui = ui, server = server)

