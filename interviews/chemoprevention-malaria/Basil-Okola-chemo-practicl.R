ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) install.packages(new.pkg, dep = T, repos = "https://cran.us.r-project.org")
  ddpcr::quiet(sapply(pkg, require, character.only = TRUE))
}

list.pkg <- c(
  "tidyverse", "readxl", "magrittr", "lme4", "geepack",
  "here", "gtsummary"
)
ipk(list.pkg)

data_dir <- file.path(here(), "data")
csv_f <- list.files(data_dir, pattern = ".csv$")
excel <- list.files(data_dir, pattern = ".xlsx$")
sheet <- readxl::excel_sheets(file.path(data_dir, excel))
dat1 <- read_excel(file.path(data_dir, excel), sheet = sheet)
dat2 <- read_csv(file.path(data_dir, csv_f)
                 
    )


dat2_1 <- dat2 %>% mutate(
  across(.cols = c(female,  protestant, region), ~as.character(.))
)
summ <- dat2_1 %>% select (-post_weights) %>% tbl_summary(.,
                                                          by = memberships,
                                                          statistic = list(all_continuous() ~ "{mean} ({sd})")
)

# ## Part 1

# Q1: Design weights are implemented prior to sampling of observations to ensure the resulting sample is as representative of the population as possible; while post-stratification weights are used at analysis to re-adjust the resulting sample within each strata if there is reason to believe it is not representative of the population

## Part 2
# 
# Q1 
# 
# mod1 <- glmer(
#   memberships ~ female+ age + protestant + (1|region) + age*female, data = dat2_1, family = poisson(link = "log")
# )
# names(summary(mod1))
# out1 <- summary(mod1)$coef %>% as.data.frame() 
# knitr::kable(out1, format = "pandoc")
# 
# 
# Q2
# 
# There is a significant effect for age*gender interaction. Protestants tend to be more enlisted than non- protestants, also significant. A significant increasing effect of age on membership.
# 
# Effect for age in males is coefficient of age : 0.0792
# Effect for age on females: 0.0793 - 0.0815 (interaction effect)
# 
# Q3

mod2 <- glmer(
  memberships ~ female+ age + protestant + (female|region) + age*female, data = dat2_1, family = poisson(link = "log")
)

# Q4 
# 
# Overdispersion occurs in families where variance 
# and mean are related like the poisson, binomial. 
# It is when the variance is too big to be estimated 
# by the model as is. One can try using a different 
# distribution or scaling the standard errors using a different dist.


