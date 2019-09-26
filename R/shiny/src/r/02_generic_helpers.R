#generic helpers
cat('Welcome ', Sys.getenv('USERNAME'),  '!\n\n',sep = '')
cat('setting directories and loading packages...\n\n')

ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  
  
  if(!'ReporteRsjars' %in% .packages(all.available = TRUE)){
    devtools::install_github('davidgohel/ReporteRsjars')
  }
  if(!'ReporteRs' %in% .packages(all.available = TRUE)){
    devtools::install_github('davidgohel/ReporteRs')
  }
  
  sapply(pkg, require, character.only = T)
}

list.of.pkgs = c('devtools', 'Rcpp', 'digest', 'backports', 'tidyverse',  'tidyr', 'dplyr', 'ggplot2', 'magrittr','tm','SnowballC','RColorBrewer','cowplot','gridExtra','ReporteRs', 'shiny','janitor')
ipk(list.of.pkgs)


cat('Setting options \n\n')

options(scipen=999)
theme_set(theme_minimal())
options(repr.plot.width = 6, repr.plot.height = 6) # plot dim