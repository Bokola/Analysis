# link to gt tutorial
# browseURL('https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/')
# study schedule program
# packages
ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("tidyverse",
              "magrittr",
              "janitor",
              # "gt",
              "officer",
              "flextable",
              "paletteer"
)
ipk(list.pkg)
# directories

home <- ifelse(Sys.info()["sysname"] == "Linux",
              Sys.getenv("Home"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\",  "/",.)
data_dir <- file.path(home, "Analysis", "study-scheduler")



study_schedule <- tibble::tibble(
  Mon = vector("character", 3),
  Tue = vector("character", 3),
  Wed = vector("character", 3),
  Thu = vector("character", 3),
  Fri = vector("character", 3)
)
core_courses <- c("Bayesian\nData Analysis", "LDA", "AMT")
core_courses_1 <- c("Bayesian\nData Analysis", "LDA")
side_study <- c( "SAS", "shiny\nwebDev")
# weights for Sep - Oct 2021
core_weight = c(0.35, 0.35, 0.3)
core_weight_1 = c(0.49, 0.51)
side_weight <- c(0.51, 0.49)
  # sample
set.seed(1234)
tmp_core <- sample(core_courses, size = 5, prob = core_weight, replace = T)
tmp <- c(tmp_core)
study_schedule[1,] = rbind(tmp)
study_schedule[2, ] = rbind(sample(
  core_courses_1,
  size = 5,
  prob = core_weight_1,
  replace = T
))
study_schedule[3, ] = rbind(sample(
  side_study,
  size = 5,
  prob = side_weight,
  replace = T
))
study_schedule$Time <- c('02:00 - 04:00', '04:00 - 06:00', '08:00 - 10:00')
study_schedule <- study_schedule %>% 
  select(Time, everything())
study_schedule_ft <- flextable(study_schedule) %>%
  bold(part = "header", bold = T) %>% 
  bold(j = 1, bold = T) %>%
  set_caption(
    paste0("For ", format(Sys.Date(), "%B, %Y")),
    style = "Table Caption",
    autonum = run_autonum(seq_id = "tab", bkm = "study")
  )
  
doc <- read_docx()
doc <- doc %>%
  body_add_par("Study timetable", style = "heading 1")
doc <- doc %>%
  body_add_flextable(study_schedule_ft) 
print(doc,
     target =  file.path(data_dir,
                paste0('study-schedule-',
                       format(Sys.Date(), "%B-%Y"),".docx")))





