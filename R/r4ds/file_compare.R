
setwd("C:\\Users\\bokola\\Analysis\\csvdiff_1\\examples")

#1. Load Packages

ipk <- function(pkg) {
  new.pkg <- list.of.pkg[!(list.of.pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
  
}
list.of.pkg <- c("tidyverse", "diffobj", "reprex")
ipk(list.of.pkg)

#2. Load Data

csv1 = read_csv('majestic_million.csv')
csv2 = read_csv('majestic_million_diff.csv')

csv1_header <- as.data.frame(colnames(csv1)) %>% readr::write_csv("headers.csv", append = T)
csv2_header <- as.data.frame(colnames(csv2)) %>% readr::write_csv("headers.csv", append = T)

# simulated data
library(diffobj)
library(tidyverse)

csv_01 <- tibble::tibble(name = "", age = "", sex = "")
csv_02 <- tibble::tibble(names = "", Age = "", SeX = "")

csv01_header <- as.data.frame(colnames(csv_01)) %>% readr::write_csv("headers.csv", append = T)
csv02_header <- as.data.frame(colnames(csv_02)) %>% readr::write_csv("header.csv", append = T)

diffobj::diffCsv(csv01_header, csv02_header)

##
f1 <- tempfile()
f2 <- tempfile()
write.csv(csv1_header, f1, row.names = F)
write.csv(csv2_header, f2, row.names = F)
diffCsv(f1, f2)

#3. Compute differences

diffCsv(csv1_header, csv2_header)
diffCsv(csv1, csv2)


