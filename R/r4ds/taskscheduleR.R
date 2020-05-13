
# Task scheduler ----------------------------------------------------------
browseURL('https://cran.rstudio.com/web/packages/taskscheduleR/vignettes/taskscheduleR.html')

ipk = function(pkg){
  new.pkg = list.pkg[!(list.pkg %in% installed.packages()[, "Package"])]
  if("practicalgg" %in% list.pkg){
    remotes::install_github("wilkelab/practicalgg")
  }
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
list.pkg = c('taskscheduleR', 'tidyverse') 
#ggforce for geom_arc_bar()
#cowplot for theme_map()
ipk(list.pkg)

home = ifelse(Sys.info()['sysname'] == 'Linux', Sys.getenv('HOME'), Sys.getenv('USERPROFILE')) %>%
  gsub('\\\\', '/',.)

home = path.expand(file.path(home,'Analysis', 'hsuApps'))

# run daily at 4.00pm -----------------------------------------------------

script = file.path(home, 'dataEntryTrends 2019', 'r', '00_main.R')
taskscheduler_create(taskname = "Trends", rscript = script, schedule = "DAILY", starttime = "16:30", startdate = format(Sys.Date(), "%d/%m/%Y"))
tasks = taskscheduler_ls()
taskscheduler_delete(taskname = "Trends")

