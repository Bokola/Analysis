#	 
#	<copyright file="week_32.R" company="bokola">
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
#	</summary>
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
  if('tidyr' %in% list.of.pkgs){
    devtools::install_github("tidyverse/tidyr",force = T)
  }
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
list.of.pkgs = c('janitor', 'viridis','repr',  'tidyverse',  'tidyr', 'dplyr', 'ggplot2')
ipk(list.of.pkgs)

cat('Setting options \n\n')

options(scipen=999)
theme_set(theme_minimal())
options(repr.plot.width = 6, repr.plot.height = 6) # plot dim

cat('setting project directories', '\n')

home_dir =ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))

project_dir = path.expand(file.path(home_dir
                                    ,'Analysis'
                                    ,'tidytuesday'
)) %>% gsub( '\\\\', '/',.)
data_dir = path.expand(file.path(project_dir
                                 ,'data'
                                 ,'2019'
                                 ,'2019-08-13')) %>% gsub( '\\\\', '/',.)

# load data
emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
 emperors = emperors %>%
   mutate(., age_at_death = round((difftime(as.Date(death), as.Date(birth), 'days'))/360)) %>%
   mutate(., age_at_death = ifelse(age_at_death < 0, NA_integer_, age_at_death))  %>%
   mutate(., century_of_death =as.numeric(sapply(strsplit(as.character(death), '-'), function(x) x[[1]][1]))) %>%
   mutate(., age_start_reign = round((difftime(as.Date(reign_start), as.Date(birth), 'days'))/360)) %>%
   mutate(., age_start_reign = ifelse(age_start_reign < 0, NA_integer_, age_start_reign)) %>%
   mutate(., age_end_reign = round((difftime(as.Date(reign_end), as.Date(birth), 'days'))/360)) %>%
   mutate(., age_end_reign = ifelse(age_end_reign < 0, NA_integer_, age_end_reign))

#plots

ggplot(emperors) +
  geom_segment(aes(x = birth, xend = death, y = reorder(name, reign_start), yend = reorder(name, reign_start), color = dynasty), alpha = .8, size = 2.5, lineend = "round") +
  geom_text(aes(x = death, y = name, label = ifelse(emperors$cause == "Natural Causes", NA, cause)), hjust = -.5, size = 2.5, alpha = .8) +
  geom_segment(aes(x = reign_start, xend = reign_end, y = name, yend = name), color = "black", size = 1.5, lineend = "round") +
  geom_point(aes(x = death, y = name), shape = ifelse(emperors$cause == "Natural Causes", NA, 20), size = 1, color = "white") +
  geom_point(aes(x = death, y = name), shape = ifelse(emperors$cause == "Natural Causes", NA, 1), size = .6, color = "black") +
  geom_point(aes(x = death, y = name), shape = ifelse(emperors$cause == "Natural Causes", NA, 1), size = 1, color = "white") +
  annotate("rect", xmin = as.Date("01-01-0010", "%d-%m-%Y"), xmax = as.Date("01-01-0155", "%d-%m-%Y"), ymin = 1.5, ymax = 2.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
  annotate("text", x = as.Date("01-01-0060", "%d-%m-%Y"), y = 2, label = "Error in data set", size = 2, hjust = -1, family = "Courier", color = "red") +
  annotate("rect", xmin = as.Date("01-01-0270", "%d-%m-%Y"), xmax = as.Date("01-01-0400", "%d-%m-%Y"), ymin = 43.5, ymax = 44.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
  annotate("text", x = as.Date("01-01-0330", "%d-%m-%Y"), y = 44, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
  annotate("rect", xmin = as.Date("01-01-0275", "%d-%m-%Y"), xmax = as.Date("01-01-0405", "%d-%m-%Y"), ymin = 46.5, ymax = 48.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
  annotate("text", x = as.Date("01-01-0335", "%d-%m-%Y"), y = 47.5, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
  annotate("rect", xmin = as.Date("01-01-0300", "%d-%m-%Y"), xmax = as.Date("01-01-0425", "%d-%m-%Y"), ymin = 52.5, ymax = 53.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
  annotate("text", x = as.Date("01-01-0355", "%d-%m-%Y"), y = 53, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
  annotate("rect", xmin = as.Date("01-01-0345", "%d-%m-%Y"), xmax = as.Date("01-01-0475", "%d-%m-%Y"), ymin = 60.5, ymax = 61.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
  annotate("text", x = as.Date("01-01-0405", "%d-%m-%Y"), y = 61, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
  labs(x = "Year", y = "", title = "#tidytuesday: Roman Emperors", subtitle = "Lifespan (colored bars), duration of reign (black bars), cause of death (unless natural cause) and their dynasty", color = "Dynasty", caption = "@MyriamCTraub") +
  expand_limits(x = as.Date("500-01-01")) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank()) +
  ggsave(path.expand(file.path(project_dir,'plots',"Emperors.png")), width = 12, height = 10)

 ggplot(emperors) +
   geom_segment(aes(x = age_start_reign, xend = age_end_reign, y = reorder(name, age_start_reign), yend = reorder(name, age_start_reign), color = dynasty), alpha = .8, size = 2.5, lineend = "round") +
   geom_text(aes(x = age_end_reign, y = name, label = ifelse(emperors$cause == "Natural Causes", NA, cause)), hjust = -.5, size = 2.5, alpha = .8) +
   geom_segment(aes(x = age_start_reign, xend = age_at_death, y = name, yend = name), color = "black", size = 1.5, lineend = "round") +
   geom_point(aes(x = age_end_reign, y = name), shape = ifelse(emperors$cause == "Natural Causes", NA, 20), size = 1, color = "white") +
   geom_point(aes(x = age_end_reign, y = name), shape = ifelse(emperors$cause == "Natural Causes", NA, 1), size = .6, color = "black") +  
   geom_point(aes(x = age_end_reign, y = name), shape = ifelse(emperors$cause == "Natural Causes", NA, 1), size = 1, color = "white") +
   annotate("rect", xmin = as.Date("01-01-0010", "%d-%m-%Y"), xmax = as.Date("01-01-0155", "%d-%m-%Y"), ymin = 1.5, ymax = 2.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
   annotate("text", x = as.Date("01-01-0060", "%d-%m-%Y"), y = 2, label = "Error in data set", size = 2, hjust = -1, family = "Courier", color = "red") +
   annotate("rect", xmin = as.Date("01-01-0270", "%d-%m-%Y"), xmax = as.Date("01-01-0400", "%d-%m-%Y"), ymin = 43.5, ymax = 44.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
   annotate("text", x = as.Date("01-01-0330", "%d-%m-%Y"), y = 44, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
   annotate("rect", xmin = as.Date("01-01-0275", "%d-%m-%Y"), xmax = as.Date("01-01-0405", "%d-%m-%Y"), ymin = 46.5, ymax = 48.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
   annotate("text", x = as.Date("01-01-0335", "%d-%m-%Y"), y = 47.5, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
   annotate("rect", xmin = as.Date("01-01-0300", "%d-%m-%Y"), xmax = as.Date("01-01-0425", "%d-%m-%Y"), ymin = 52.5, ymax = 53.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
   annotate("text", x = as.Date("01-01-0355", "%d-%m-%Y"), y = 53, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
   annotate("rect", xmin = as.Date("01-01-0345", "%d-%m-%Y"), xmax = as.Date("01-01-0475", "%d-%m-%Y"), ymin = 60.5, ymax = 61.5, alpha= .01, color = "red", size = .3, linetype = "dotted") +
   annotate("text", x = as.Date("01-01-0405", "%d-%m-%Y"), y = 61, label = "Missing data", size = 2, hjust = -1, family = "Courier", color = "red") +
   labs(x = "Year", y = "", title = "#tidytuesday: Roman Emperors", subtitle = "Lifespan (colored bars), duration of reign (black bars), cause of age_end_reign (unless natural cause) and their dynasty", color = "Dynasty", caption = "@okola_owiti") +
   expand_limits(x = as.Date("500-01-01")) +
   scale_color_viridis(discrete = TRUE, option = "D") +
   theme(legend.position = "bottom",
         panel.grid.major.y = element_blank()) +
   ggsave(path.expand(file.path(project_dir,'plots',"Emperors.png")), width = 12, height = 10)





