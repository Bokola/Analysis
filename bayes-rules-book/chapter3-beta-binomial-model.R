
# packages ----------------------------------------------------------------


ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("Rcpp", "rstan", "bayesrules", "tidyverse", "janitor")
ipk(list.pkg)

# Tuning the Beta prior

# plot the Beta(45, 55) prior
plot_beta(45, 55)
# plot beta-binomial
plot_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)
summarize_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)
# simulating the beta-binomial
set.seed(1234)
michelle_sim <- data.frame(pi = rbeta(10000, 45, 55)) %>%
  mutate(y = rbinom(10000, size = 50, prob = pi))

ggplot(michelle_sim, aes(x = pi, y = y)) +
  geom_point(aes(color = (y==30)), size = 0.1)

# keep only simulated pairs that match our data
michelle_posterior <- michelle_sim %>%
  filter(y == 30)

ggplot(michelle_posterior, aes(x = pi)) +
  geom_density()
