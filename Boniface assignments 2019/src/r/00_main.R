#	 
#	<copyright file="Diabetes and hypertension summaries.R" company="bokola">
#	 
#		Copyright (c) 2019 All rights reserved
#	 
#		This source is owned and managed by KEMRI Wellcome Trust.
#		Permission must be sought from the above mentioned entity for use, distribution or modification
#		If you are not a KEMRI employee, any violation of the above can constitute malice and criminal intent
#		All other rights reserved.
#	 
#	</copyright>
#	 
#	<author>Basil Okola</author>
#	<email>okolabasilowiti@gmail.com</email>
#	<date>2019-08-07</date>
#	<summary>
#	 
#		Point of entry and exit
#	 
#	</summaries and plots>
#	 

# Clear memory

rm(list = ls(all = T))

# Display message:

cat("----------------------------------------\n\n")
cat("Welcome ", Sys.getenv("USERNAME"), "!\n\n", sep = "")
cat("The date today is", format(Sys.Date(), "%d %B %Y."), "\n")-
  cat("Reporting session started at ", format(Sys.time(), "%I:%M %p"), "\n\n", sep = "")

# Set project path and load packages

cat("Installing packages", '\n')

ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  # if('tidyr' %in% list.of.pkgs){
  #   devtools::install_github("tidyverse/tidyr",force = T)
  # }
  # if(!'patchwork' %in% .packages(all.available = TRUE)){
  #   devtools::install_github("thomasp85/patchwork")
  # }
  # if(!'ReporteRsjars' %in% .packages(all.available = TRUE)){
  #   devtools::install_github('davidgohel/ReporteRsjars')
  # }
  # if(!'ReporteRs' %in% .packages(all.available = TRUE)){
  #   devtools::install_github('davidgohel/ReporteRs')
  # }
  # 
  sapply(pkg, require, character.only = T)
}
list.of.pkgs = c('officer', 'plyr', 'tidyr', 'dplyr', 'ggplot2','magrittr', 'ReporteRs', 'janitor')
ipk(list.of.pkgs)

cat('Setting options \n\n')

options(scipen=999)
theme_set(theme_minimal())
options(repr.plot.width = 6, repr.plot.height = 6) # plot dim

cat('setting project directories', '\n')

home_dir =ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))

project_dir = path.expand(file.path(home_dir
                                    ,'Analysis'
                                    ,'Boniface assignments 2019'
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
files <- allfiles[!allfiles %in% c("00_main.R","04_data_recoding.R", "05_reports.R")]
files <- files[order(files)]
files <- file.path(src_dir, files)
invisible(run.app(files))

files <- allfiles[allfiles %in% c("04_data_recoding.R", "05_reports.R")] %>%
  sort() %>%
  file.path(src_dir, .)
invisible(run.app(files)) #run files
