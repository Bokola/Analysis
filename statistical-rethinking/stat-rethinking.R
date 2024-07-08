p_grid <- seq(0, 1, length.out = 1000)
# prior
prob_p <- rep(1, 1000)
# likelihood
prob_data <- dbinom(6, size = 9, prob = p_grid)
# posterior
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
sapply(list(p_grid, prob_p, prob_data, posterior), plot)
