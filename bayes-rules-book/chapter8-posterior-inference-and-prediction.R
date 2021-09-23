browseURL("https://www.bayesrulesbook.com/chapter-8.html")
# packages
ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("Rcpp",
              "rstan",
              "bayesrules",
              "tidyverse",
              "janitor",
              "bayesplot",
              "broom.mixed")
ipk(list.pkg)

# goal is to find proportion of artists that are Gen x

data("moma_sample")
plot_beta_binomial(alpha = 4, beta = 6, y = 14, n = 100)
# 0.025th & 0.975th quantiles of the Beta(18,92) posterior
qbeta(c(0.025, 0.975), 18, 92)
# 0.005th & 0.995th quantiles of the Beta(18,92) posterior
qbeta(c(0.005, 0.995), 18, 92)
# posterior probability that pi < 0.20
post_prob <- pbeta(0.2, 18, 92)
post_prob
# Posterior odds
post_odds <- post_prob / (1 - post_prob)
post_odds
# Prior probability that pi < 0.2
prior_prob <- pbeta(0.20, 4, 6)
prior_prob
# Prior odds
prior_odds <- prior_prob / (1 - prior_prob)
prior_odds
# Bayes factor
BF <- post_odds / prior_odds
BF


# posterior analysis with MCMC ---------------------------------------------

# posterior simulation

# step 1: define the model

art_model <- "
  data {
    int<lower = 0, upper = 100> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(100, pi);
    pi ~ beta(4, 6);
  }
"

# step 2: simulate the posterior

art_sim <- stan(model_code = art_model, data = list(Y = 14),
                chains = 4, iter = 5000*2, seed = 1234)
# parallel trace plots and density plots

mcmc_trace(art_sim, pars = "pi", size = 0.5) + 
  xlab("iterations")
mcmc_dens_overlay(art_sim, pars = "pi")
# autocorrelation plot
mcmc_acf(art_sim, pars = "pi")

# Markov chain diagnostics
rhat(art_sim, pars = "pi")
neff_ratio(art_sim, pars = "pi")

# posterior estimation & hypothesis testing -------------------------------

# The actual Beta(18, 92) posterior
plot_beta(alpha = 18, beta = 92) + 
  lims(x = c(0, 0.35))

# MCMC posterior approximation
mcmc_dens(art_sim, pars = "pi") + 
  lims(x = c(0,0.35))
# As such, we can approximate any feature of the Beta(18, 92) posterior model 
# by the corresponding feature of the Markov chain
# tidy() function in the broom.mixed package provides some handy statistics for
# the combined 20,000 Markov chain values stored in art_sim

tidy(art_sim, conf.int = TRUE, conf.level = 0.95)

# mcmc_area() function in the bayesplot package provides a visual complement

# shade in the middle 95% interval
mcmc_areas(art_sim, pars = "pi", prob = 0.95)

# tidy() function doesn't always provide every summary statistic of interest. 
# For example, it doesn't report the mean or mode of our Markov chain sample values

# store 4 chains in one dataframe

art_chains_df <- as.data.frame(art_sim, pars = "lp__", include = F)
dim(art_chains_df)

# Calculate posterior summaries of pi
art_chains_df %>% 
  summarize(post_mean = mean(pi), 
            post_median = median(pi),
            post_mode = sample_mode(pi),
            lower_95 = quantile(pi, 0.025),
            upper_95 = quantile(pi, 0.975))

# Tabulate pi values that are below 0.20
art_chains_df %>% 
  mutate(exceeds = pi < 0.20) %>% 
  tabyl(exceeds)


# posterior prediction ----------------------------------------------------

# Set the seed
set.seed(1)

# Predict a value of Y' for each pi value in the chain
art_chains_df <- art_chains_df %>% 
  mutate(y_predict = rbinom(length(pi), size = 20, prob = pi))

# Check it out
art_chains_df %>% 
  head(3)

# Plot the 20,000 predictions
ggplot(art_chains_df, aes(x = y_predict)) + 
  stat_count()

# posterior prediction interval
art_chains_df %>% 
  summarize(mean = mean(y_predict),
            lower_80 = quantile(y_predict, 0.1),
            upper_80 = quantile(y_predict, 0.9))