ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, library, character.only = TRUE))
}

list.of.pkgs = c('tidyverse', 'magrittr', 'nycflights13', 'lubridate')
ipk(list.of.pkgs)



home = ifelse(Sys.info()["sysname"] == 'Linux', Sys.getenv("HOME"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\", "/",.)


# 1. product of 2 numbers adding to 2020 ----------------------------------


data = file.path(home, "Downloads")

dat = read.delim(file.path(data,"dat1.txt")) %>% `colnames<-`(c("x1"))
dat =  dat %>%
  mutate(., x2 = x1)

out = expand.grid(dat$x1, dat$x2) %>% `colnames<-`(c("x1", "x2"))

out1 = out %>%
  mutate(., x3 = rowSums(.)) %>%
  filter(., x3 == 2020) %>% slice(1) %>%
  select(-x3) %>%
  prod(.)
