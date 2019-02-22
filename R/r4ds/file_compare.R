
rm(list=ls(all=T))
setwd("H:\\HSU\\data\\Neonatal")

#1. Load Packages

ipk <- function(pkg) {
  new.pkg <- list.of.pkg[!(list.of.pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
  
}
list.of.pkg <- c("tidyverse", "diffobj", "reprex", "daff")
ipk(list.of.pkg)

#2. Load Data

# temp = list.files(pattern="*.csv")
# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))




temp = list.files(pattern="*.csv")[1]
list2env(
  lapply(setNames(temp, tolower(make.names(gsub("_.*", "", temp)))), function(x){ 
    x =read.csv(x)
    names(x) <- tolower(names(x))
    return(x)}), 
        
  envir = .GlobalEnv)


head(kiambu)
names(kiambu)

rename <- function(df) {
  sapply(names(df), tolower)
}
rename(kiambu)

# rm(list = ls())

# csv1_header <- as.data.frame(colnames(csv1)) %>% readr::write_csv("headers.csv", append = T)
# csv2_header <- as.data.frame(colnames(csv2)) %>% readr::write_csv("headers.csv", append = T)

# simulated data


# csv_01 <- tibble::tibble(name = "", age = "", sex = "")
# csv_02 <- tibble::tibble(names = "", Age = "", SeX = "")
# 
# csv01_header <- as.data.frame(colnames(csv_01)) %>% readr::write_csv("headers.csv", append = T)
# csv02_header <- as.data.frame(colnames(csv_02)) %>% readr::write_csv("header.csv", append = T)



##
f1 <- tempfile()
f2 <- tempfile()
write.csv(csv1_header, f1, row.names = F)
write.csv(csv2_header, f2, row.names = F)
diffCsv(f1, f2)

#3. Compute differences

as.data.frame(diffObj(kiambu, bungoma))

diffCsv(csv1, csv2)


