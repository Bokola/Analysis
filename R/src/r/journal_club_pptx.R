#	 
#	<copyright file="journal_pptx.R" company="bokola">
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
list.of.pkgs = c('ReporteRs',  'tidyr', 'dplyr', 'ggplot2','magrittr')
ipk(list.of.pkgs)

cat('Setting options \n\n')

options("ReporteRs-default-font" = "Garamond")
options(scipen=999)
theme_set(theme_minimal())
options(repr.plot.width = 6, repr.plot.height = 6) # plot dim

cat('setting project directories', '\n')

home_dir =ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))

project_dir = path.expand(file.path(home_dir
                                    ,'Analysis'
                                    ,'R'
)) %>% gsub( '\\\\', '/',.)

data_dir = path.expand(file.path(home_dir, "Analysis", 'journal papers', 'data')) %>%
  gsub("\\\\", '/',.)

src_dir = path.expand(file.path(project_dir
                                 ,'src'
                                 ,'r'
                                 )) %>% gsub( '\\\\', '/',.)
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

#place holder variables

doc = pptx()
slide.layouts(doc) # available layouts

layouts = slide.layouts(doc) # all available layout

for(i in layouts) {
  par(mar = c(0.5, 0.5,2,0.5), cex = 0.7)
  slide.layouts(doc, i)
  title(main = paste0("'", i, "'"))
  if(interactive()) readline(prompt = "Show next slide layout")
}  

#slide 1 :  Title slide
doc = pptx()
doc = addSlide(doc, "Title Slide") %>%
  addTitle(.,'How do low-birthweight neonates fare 2 years after discharge from a low-technology neonatal care unit in a rural district hospital in Burundi?', font.) %>%
  addSubtitle(., 'A Journal Club Paper Presentation') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
addPageNumber(., "1")

#slide 2 : Outline

doc = doc %>%
  addSlide(., "Title and Content") %>%
  addTitle(., "Outline of Presentation") %>%
  addParagraph(., value = c('Background', 'Methods', 'Results', 'My take'),
               par.properties = parProperties(list.style = 'ordered', level = 1)) %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "2")

#slide 3: Backgroud

doc = doc %>%
  addSlide(., "Title and Content") %>%
  addTitle(., 'Backgroud') %>%
  addParagraph(., value = c('Neonatal deaths account for about 40% of all under-five mortalities', 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest', 'Majority of these deathes occur in the VLBW (< 1500 g)', 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)'), par.properties = parProperties(list.style = 'unordered', level = 1)) %>%
  # addParagraph(., 'Neonatal deaths account for about 40% of all under-five mortalities') %>%
  # addParagraph(., 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest') %>%
  # addParagraph(., 'Majority of these deathes occur in the VLBW (< 1500 g)') %>%
  # addParagraph(., 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "3")

#slide 4: Methods

doc = doc %>%
  addSlide(., "Two Content") %>%
  addTitle(., 'Methods') %>%
  addParagraph(., value = c('H/hold survey of neonates discharged from EmONC in 2012', 'Location is Rural Bujumbura, Burundi', 'Survey is by 10 TQS screening tool from UNICEF', 'Analysis reports summaries, comparison of outcomes by categories (t-tests and likes)'), par.properties = parProperties(list.style = 'unordered', level = 1)) %>%
  addImage(., file.path(data_dir, 'tqs.png')) %>%
  # addParagraph(., 'Neonatal deaths account for about 40% of all under-five mortalities') %>%
  # addParagraph(., 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest') %>%
  # addParagraph(., 'Majority of these deathes occur in the VLBW (< 1500 g)') %>%
  # addParagraph(., 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "4")

#slide 5: Results
doc = doc %>%
  addSlide(., "Two Content") %>%
  addTitle(., 'Results') %>%
  addParagraph(., value = c('Results are on 107 neonates'), par.properties = parProperties(list.style = 'unordered', level = 1)) %>%
  addImage(., file.path(data_dir, 'sample.png')) %>%
  #addImage(., file.path(data_dir, 'baseline_xtics.png')) %>%
  # addParagraph(., 'Neonatal deaths account for about 40% of all under-five mortalities') %>%
  # addParagraph(., 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest') %>%
  # addParagraph(., 'Majority of these deathes occur in the VLBW (< 1500 g)') %>%
  # addParagraph(., 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "5")

doc = doc %>%
  addSlide(., "Two Content") %>%
  addTitle(., 'Results') %>%
  addParagraph(., value = c('LBWs accounted for 48% of study sample'), par.properties = parProperties(list.style = 'unordered', level = 1)) %>%
  #addImage(., file.path(data_dir, 'sample.png')) %>%
  addImage(., file.path(data_dir, 'baseline_xtics.png')) %>%
  # addParagraph(., 'Neonatal deaths account for about 40% of all under-five mortalities') %>%
  # addParagraph(., 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest') %>%
  # addParagraph(., 'Majority of these deathes occur in the VLBW (< 1500 g)') %>%
  # addParagraph(., 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "6")

doc = doc %>%
  addSlide(., "Two Content") %>%
  addTitle(., 'Results') %>%
  addParagraph(., value = c('Interllectual impairement more pronounced among VLBWs'), par.properties = parProperties(list.style = 'unordered', level = 1)) %>%
  #addImage(., file.path(data_dir, 'sample.png')) %>%
  addImage(., file.path(data_dir, 'imparements.png')) %>%
  # addParagraph(., 'Neonatal deaths account for about 40% of all under-five mortalities') %>%
  # addParagraph(., 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest') %>%
  # addParagraph(., 'Majority of these deathes occur in the VLBW (< 1500 g)') %>%
  # addParagraph(., 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "7")

doc = doc %>%
  addSlide(., "Title and Content") %>%
  addTitle(., 'My take') %>%
  addParagraph(., value = c('A good step towards monitoring post-discharge outcomes in neonates', 'Simple communication of findings'), par.properties = parProperties(list.style = 'unordered', level = 1)) %>%
  #addImage(., file.path(data_dir, 'sample.png')) %>%
  #addImage(., file.path(data_dir, 'imparements.png')) %>%
  # addParagraph(., 'Neonatal deaths account for about 40% of all under-five mortalities') %>%
  # addParagraph(., 'Leading cause of neonatal deaths is prematurity (< 37 weeks of gest') %>%
  # addParagraph(., 'Majority of these deathes occur in the VLBW (< 1500 g)') %>%
  # addParagraph(., 'There is growing need to monitor progress of discharged LBW neonates to meet SDG 3(3.2)') %>%
  addDate(.) %>%
  addFooter(., 'Kemri-Wellcome Trust|Basil Okola') %>%
  addPageNumber(., "8")






writeDoc(doc, file.path(data_dir, "Journal_pptx.pptx"))  
  
  