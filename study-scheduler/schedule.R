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
              "gt",
              "officer",
              "flextable",
              "paletteer"
)
ipk(list.pkg)
# directories
rm(list = ls(all.names = T))
home = ifelse(Sys.info()["sysname"] == "Linux",
              Sys.getenv("Home"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\",  "/",.)
data_dir = file.path(home, "Analysis", "study-scheduler")



# function
schedule <- function(core_courses, side_study, core_weight, side_weight){
  study_schedule <- tibble::tibble(Mon = vector("character", 3),
                                   Tue = vector("character", 3),
                                   Wed = vector("character", 3),
                                   Thu = vector("character", 3),
                                   Fri = vector("character", 3))
  
  # sample
  set.seed(1234)
  for(i in seq_along(study_schedule)) {
    
    tmp_core <- sample(core_courses, size = 2, prob = core_weight, replace = T)
    tmp_side <- sample(side_study, size = 1, prob = side_weight, replace = F)
    tmp <- c(tmp_core, tmp_side)
    study_schedule[[i]] <- tmp
  }
  # tmp_core <- sample(core_courses, size = 4, prob = core_weight, replace = F)
  # tmp_side <- sample(side_study, size = 2, prob = side_weight, replace = F)
  # tmp <- c(tmp_core, tmp_side)
  
  study_schedule <- as.data.frame(study_schedule)
  # show range of colors in a given scale
  scales::show_col(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"))
  
  paletteer::paletteer_d("ggsci::green_material", n = 6) %>%
    as.character() %>%
    scales::show_col()
  
  # add rownames
  row.names(study_schedule) <- c(
                                 "0300 - 0500",
                                 "2200 - 2400",
                                 "0100 - 0300")
  study_schedule <- study_schedule %>% tibble::rownames_to_column()
  names(study_schedule)[1]  <- "Time"
  
  
  # make times bold
  
  study_schedule <- study_schedule %>%
    mutate(Time =  paste0("<span style='font-size:16px; font-weight:bold;'>",
                          Time,
                          "</span>"),
           Time = map(Time, ~gt::html(as.character(.x)))
    )
  # bold column names
  col_for_list <- list(
    Time = md("**Time**"),
    Mon = md("**Mon**"),
    Tue = md("**Tue**"),
    Wed = md("**Wed**"),
    Thu = md("**Thu**"),
    Fri = md("**Fri**")
  )
  
  # use gt functions to format the table
  out <- study_schedule %>% gt() %>% cols_label(.list = col_for_list) %>%
    
    # We use tab_style() to change style of cells
    # cell_borders() provides the formatting
    # locations tells it where
    # add a border to left of the Mon column
    tab_style(
      style = list(
        cell_borders(
          sides = "left",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_body(
          columns = vars(Mon)
        )
      )
    ) %>%
    
    # We use tab_style() to change style of cells
    # cell_borders() provides the formatting
    # locations tells it where
    # Add black borders to the bottom of all the column labels
    
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_column_labels(
          columns = gt::everything()
        )
      )
    ) %>%
    
    # add headers and footnotes
    
    tab_source_note("@okola_owiti | data: 2021 - 2022 MSc Statistics courses ") %>%
    tab_header(
      title = md("**2021 - 2022 Study Schedule**"),
      subtitle = format(Sys.Date(), "%b %Y")
      
    )
  return(out)

}


# arguments

core_courses <- c("Bayesian\nData Analysis", "LDA", "AMT")
side_study <- c( "SAS", "shiny\nwebDev")
# weights for Sep - Oct 2021
core_weight = rep(1/length(core_courses), 3)
side_weight <- c(0.45, 0.55)


# Sep & Oct 2021
core_sep <- c("CPS", "Bayesian\nData Analysis", 
              "Computer\nIntensive Methods", "LDA")
side_sep <- c("Bayes\nrules book", "Base SAS", "Advanced\nR", "SQL",
              "Designing Data\nIntensive applications")
side_sep_wt <- c(0.25, 0.25, 0.2, 0.15, 0.15)

core_sep_wt <- c(0.3, 0.25, 0.2, 0.25)

# OCT & Nov 2021
core_nov <- c("CPS", "Bayesian\nData Analysis", 
          "Computer\nIntensive Methods", "LDA", "AMT", "capita\nselecta")
side_nov <- c("Bayes\nrules book", "Advanced\nSAS", "Advanced\nR", "SQL","Designing Data\nIntensive applications")
side_nov_wt <- c(0.25, 0.25, 0.2, 0.15, 0.15)

core_nov_wt <- c(0.2, 0.2, 0.15, 0.2, 0.15, 0.1)

# call the function

Jul_schedule <- schedule(core_courses = core_courses, side_study = side_study, core_weight = core_weight,
                         side_weight = side_weight)

# save the gt object
gtsave(data = Jul_schedule, filename = file.path(data_dir, paste0(
  "study schedule_", format(Sys.Date(), "%b-%Y"), ".tex"
)))
gtsave(
  data = Jul_schedule,
  filename = file.path(data_dir, paste0(
    "study schedule_", format(Sys.Date(), "%b-%Y"), ".html"
  )),
  inline_css = T
)
