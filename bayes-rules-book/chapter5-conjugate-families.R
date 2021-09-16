ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("Rcpp", "rstan", "bayesrules", "tidyverse", "janitor")
ipk(list.pkg)


# load data ---------------------------------------------------------------

data("football")
concussion_subjects <- football %>%
  filter(group == "fb_concuss")

concussion_subjects %>% summarize(
  mean(volume)
)

ggplot(concussion_subjects, aes(x = volume)) +
  geom_density()

plot_normal_likelihood(y = concussion_subjects$volume, sigma = 0.5)
plot_normal_normal(mean = 6.5, sd = 0.4, sigma = 0.5,
                   y_bar = 5.735, n = 25)
