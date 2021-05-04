ipk = function(pkg) {
  new = libs[!(libs %in% .packages(all.available = TRUE))]
  if (length(new))
    (
      install.packages(new, dependencies = TRUE,
                       repos = 'https://cran.us.r-project.org')
    )
  ddpcr::quiet(sapply(pkg, require, character.only = TRUE))
}
libs = c("tidyverse", "magrittr", "distill")
ipk(libs)

home = ifelse(Sys.info()["sysname"] == "Windows",
              Sys.getenv("USERPROFILE"),
              Sys.getenv("HOME"))
home = home %>% gsub("\\\\", "/", .)

data_dir = file.path(
  home,
  "Google Drive (basil.okola@student.uhasselt.be)",
  "MSc. Stats Hasselt",
  "y1 sem2",
  "Multivariate and hierarchical data",
  "sample size calculation"
)
data = readr::read_csv(file.path(data_dir, "G8.pilot.data.csv"))
site_dir = file.path(home, "Distill websites", "_posts")
site_dir2 = file.path(home, "Distill websites")
# set dir to distill website location
setwd(site_dir2)
library(distill)
# distill::import_post('https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html')
create_post("Web scraping with R")
