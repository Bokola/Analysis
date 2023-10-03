ipk <- function(pkg){
  for(i in seq_len(length(pkg))){
    if(!require(pkg[i], character.only = TRUE)) install.packages(
      pkg[i], dep = T#, repos = "https://cran.us.r-project.org"
    )
  }
}


pks <- c("haven", "here", "remotes","dplyr", "purrr", "readxl", "magrittr")
ipk(pks)
fils <- list.files(file.path(here(),  "sightsavers"),pattern = ".xlsx")
fils <- subset(fils, !grepl("~", fils))
dat <- read_xlsx(file.path(here(), "sightsavers", fils), sheet = "Data")

str(dat)
dat <- dat %>% mutate(
  blind_le = ifelse(pvale %in% c(5, 6, 7), 1, 0),
  blind_re = ifelse(pvare %in% c(5, 6, 7), 1, 0)
)

dat <- dat %>% mutate(
  blind = ifelse(blind_le == 1 & blind_re == 1, 1, 0)
)
