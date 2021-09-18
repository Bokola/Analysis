ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("Rcpp", "rstan", "bayesrules", "tidyverse", "janitor", "bayesplot")
ipk(list.pkg)


# beta-binomial example ---------------------------------------------------



# step 1: define a grid of 6 pi values

grid_data <- data.frame(pi_grid = seq(from = 0, to = 1, length = 6))
# step 2: evaluate the prior and likelihood at each pi
grid_data <- grid_data %>%
  mutate(prior = dbeta(pi_grid, 2, 2), 
         likelihood = dbinom(9, 10, pi_grid))
# step 3: approximate posterior
grid_data <- grid_data %>%
  mutate(unnormalized = likelihood * prior,
         posterior = unnormalized / sum(unnormalized))
# confirm that the posterior approximation sums to 1
grid_data %>% summarize(sum(unnormalized), sum(posterior))

# examine the grid approximated posterior
round(grid_data, 2)

# plot the grid approximated posterior
ggplot(grid_data, aes(x = pi_grid, y = posterior)) + 
  geom_point() +
  geom_segment(aes(x = pi_grid, xend = pi_grid, y = 0, yend = posterior))
# sample N \theta grid values

set.seed(1234)
post_sample = sample_n(grid_data, size = 10000, 
                       weight = posterior, replace = T)

#  a table of 10000 sample values

# As expected, most of our 10,000 sample values of  
# pi were 0.6 or 0.8, few were 0.4, and none were below 0.4 or above 0.8

post_sample %>%
  tabyl(pi_grid) %>%
  adorn_totals("row")

# histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x = pi_grid)) +
  geom_histogram(aes(y = ..density..), color = "white") +
  stat_function(fun = dbeta, args = list(11, 3)) +
  lims(x = c(0, 1))

# Gamma-Poisson example ---------------------------------------------------

plot_gamma_poisson(s = 3, r = 1, sum_y = 10, n = 2, posterior = F)

# Step 1: Define a grid of 501 lambda values
grid_data   <- data.frame(lambda_grid = seq(from = 0, to = 15, length = 501))

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data <- grid_data %>% 
  mutate(prior = dgamma(lambda_grid, 3, 1),
         likelihood = dpois(2, lambda_grid) * dpois(8, lambda_grid))

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = likelihood * prior,
         posterior = unnormalized / sum(unnormalized))

# Set the seed
set.seed(84735)

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)
# Histogram of the grid simulation with posterior pdf 
ggplot(post_sample, aes(x = lambda_grid)) + 
  geom_histogram(aes(y = ..density..), color = "white") + 
  stat_function(fun = dgamma, args = list(13, 3)) + 
  lims(x = c(0, 15))

# Markov Chain Monte Carlo via rstan --------------------------------------

# Beta-Binomial example

# step 1: Define the model

bb_model <- "

data {
int<lower = 0, upper = 10> Y;
}

parameters {
real<lower = 0, upper = 1> pi;
}

model {
Y ~ binomial(10, pi);
pi ~ beta(2, 2);
}
"

# step 2: Simulate the posterior

bb_sim <- stan(model_code = bb_model, data = list(Y = 9),
               chains = 4, iter = 5000*2, seed = 1234)
# extract first 4 values of pi
as.array(bb_sim, pars = "pi") %>%
  head(4)

mcmc_trace(bb_sim, pars = "pi", size = 0.1)

# Histogram of the Markov chain values
mcmc_hist(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + 
  ylab("density")

# a Gamma-Poisson example

# STEP 1: DEFINE the model
gp_model <- "
  data {
    int<lower = 0> Y[2];
  }
  parameters {
    real<lower = 0> lambda;
  }
  model {
    Y ~ poisson(lambda);
    lambda ~ gamma(3, 1);
  }
"

# STEP 2: SIMULATE the posterior
gp_sim <- stan(model_code = gp_model, data = list(Y = c(2,8)), 
               chains = 4, iter = 5000*2, seed = 1234)
# Trace plots of the 4 Markov chains
mcmc_trace(gp_sim, pars = "lambda", size = 0.1)

# Histogram of the Markov chain values
mcmc_hist(gp_sim, pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(gp_sim, pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("density")
