
# installations -----------------------------------------------------------


install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
# we recommend running this in a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
cmdstanr::install_cmdstan()
devtools::install_github("rmcelreath/rethinking")
p_grid <- seq(0, 1, length.out = 1000)
# prior
prob_p <- rep(1, 1000)
# likelihood
prob_data <- dbinom(6, size = 9, prob = p_grid)
# posterior
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
sapply(list(p_grid, prob_p, prob_data, posterior), plot)
# sample from posterior pedictive distribution
s <- sample(p_grid, 1e4, prob = posterior, replace = TRUE)
w <- dbinom(1e4, 9, prob = s)
plot(w/sum(w))


# chap3: Linear regression ------------------------------------------------

# data generating mechanism: H ---> W
alpha <- 0; beta <- 0.5; sigma <- 5; n <- 1000
# generate heights
h <- runif(n, 130, 170)
mu <- alpha + beta * h
# generate weight
w <- rnorm(n, mu, sigma)
