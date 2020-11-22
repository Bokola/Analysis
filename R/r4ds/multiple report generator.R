
# generate individual reports ---------------------------------------------


home = ifelse(Sys.info()[["sysname"]] == "Linux", Sys.getenv("HOME"), Sys.getenv("USERPROFILE"))
home = gsub("\\\\", "/",home)

data_dir = file.path(home, "Google Drive (basil.okola@student.uhasselt.be)", "MSc. Stats Hasselt", 
                     "y1 sem1", "Linear models")
fig_dir = file.path(home, "Google Drive (basil.okola@student.uhasselt.be)", "MSc. Stats Hasselt", 
                    "y1 sem1", "Linear models")


# packages ----------------------------------------------------------------

ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages({sapply(pkg, require, character.only = TRUE)})
}

list.of.pkgs = c("tidyverse", "magrittr", "knitr", "rmarkdown", "bookdown",
                 "skimr", "cowplot", "plyr", "data.table", "gt", "yardstick", "officer", 
                 "flextable", "kableExtra", "patchwork", "car")
ipk(list.of.pkgs)


student = c("Meseret Assefa", "Endale Alemayu", "Kedir Adem", "Basil Okola")

for(a in student) {
  rmarkdown::render(input = file.path(data_dir, "lmo_assign.Rmd"),
                    output_format = bookdown::pdf_document2(),
                    output_file = paste0(a, ".docx"),
                    output_dir = file.path(data_dir, "figures"))
}