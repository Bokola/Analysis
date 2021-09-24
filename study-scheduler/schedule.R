# link to gt tutorial
browseURL('https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/')
# study schedule program

# directories

home = ifelse(Sys.info()["sysname"] == "Linux",
              Sys.getenv("Home"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\",  "/",.)
data_dir = file.path(home, "Analysis", "study-scheduler")

# packages
ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("tidyverse",
              "janitor",
              "gt",
              "officer",
              "paletteer")
ipk(list.pkg)

study_schedule <- tibble::tibble(Mon = vector("character", 6),
                                 Tue = vector("character", 6),
                                 Wed = vector("character", 6),
                                 Thu = vector("character", 6),
                                 Fri = vector("character", 6))
core_courses <- c("CPS", "Bayesian\nData Analysis", "Capita selecta\nof Computational Biology",
                  "Computer\nIntensive Methods", "LDA", "Topics in Advanced\nModelling Techniques")
side_study <- c("Advanced R", "base SAS", "Advanced SAS", "SQL", "bayes rules book", "data engineering")
# weights for Sep - Oct 2021
core_weight = c(0.16, 0.15, 0.12, 0.12, 0.3, 0.12)
side_weight <- c(0.2, 0.25, 0.15, 0.2, 0.2, 0)


# Sep & Oct 2021
core_sep <- c("CPS", "Bayesian\nData Analysis", 
              "Computer\nIntensive Methods", "LDA")
side_sep <- c("Bayes\nrules book", "Base SAS", "Advanced\nR", "SQL")
side_sep_wt <- c(0.3, 0.35, 0.2, 0.15)

core_sep_wt <- c(0.3, 0.25, 0.2, 0.25)
# sample
set.seed(1234)
for(i in seq_along(study_schedule)) {
  
  tmp_core <- sample(core_sep, size = 4, prob = core_sep_wt, replace = F)
  tmp_side <- sample(side_sep, size = 2, prob = side_sep_wt, replace = F)
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
row.names(study_schedule) <- c("0330 - 0600", "0800 - 10000", "1015 - 1215",
                               "1315 - 1515", "1530 - 1730", "1900 - 2100")
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
    subtitle = "Sep - Oct 2021"
    
  )
# save the gt object
gtsave(data = out, filename = file.path(data_dir, "out.tex"))
gtsave(data = out, filename = file.path(data_dir, "out.html"), inline_css = T)
