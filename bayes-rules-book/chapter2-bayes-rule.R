
# packages ----------------------------------------------------------------


ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("Rcpp", "rstan", "bayesrules", "tidyverse", "janitor")
ipk(list.pkg)

# ipk = function(pkg) {
#   new.pkg = list.pkg[!(list.pkg %in% .packages(all.available = TRUE))]
#   if (length(new.pkg))
#     install.packages(new.pkg, dependencies = TRUE, repos = 'https://cran.us.r-project.org')
#   ddpcr::quiet(sapply(pkg, require, character.only = TRUE))
# }
# list.pkg = c('magrittr',
#              'tidyverse',
#              'rvest',
#              'knitr',
#              'httr',
#              'jsonlite')
# 
# ipk(list.pkg)

data(fake_news)


# posterior simulation ----------------------------------------------------

set.seed(1234)
# define possible articles
article <- data.frame(type = c("real", "fake"))
# define the prior model
prior <- c(0.6, 0.4)
# do 10000 simulations
article_sim <- sample_n(article,
                        size = 10000,
                        weight = prior,
                        replace = TRUE)
article_sim %>% tabyl(type) %>%
  adorn_totals("row")
# simulate exclamation point usage
article_sim <- article_sim %>%
  mutate(data_model = case_when(type == "fake" ~ 0.2667,
                                type == "real" ~ 0.0222))
glimpse(article_sim)

# group_by specifies exclamation point simulation to be done
# separately for each of the 10000 articles

# define whether there are exclamation points
data <- c("no", "yes")
# simulate the exclamation point usage

article_sim <- article_sim %>% group_by(1:n()) %>%
  mutate(usage = sample(data, size = 1, prob = c(1 - data_model, data_model)))

ggplot(article_sim, aes(x = type, fill = usage)) +
  geom_bar(position = "fill")
# Our 10,000 simulated articles now reflect the prior model of fake news,
# as well as the likelihood of exclamation point usage among fake vs real news

article_sim %>% filter(usage == "yes") %>%
  tabyl(type) %>%
  adorn_totals("row")

# posterior simulation
# define possible win probabilities
chess <- data.frame(pi = c(0.2, 0.5, 0.8))
# define the prior model
prior <- c(0.10, 0.25, 0.65)
# simulate 10,000 values for pi from data
chess_sim <- sample_n(chess, size = 10000, weight = prior, replace = T)
# simulate 6 games from the 10000 prior values of pi using rbinom()
chess_sim <- chess_sim %>% mutate(y = rbinom(10000, size = 6, prob = pi))
# the combined 10,000 simulated values closely approximate the prior model f(pi)
chess_sim %>%
  tabyl(pi) %>%
  adorn_totals("row")
# plot y by pi
ggplot(chess_sim, aes(x = y)) +
  stat_count(aes(y = ..prop..)) +
  facet_wrap(~ pi)
# Finally, let's focus on the simulated outcomes that
# match the observed data that Kasparov won one game

win_one <- chess_sim %>%
  filter(y ==1)
# summarize the posterior approximation
win_one %>%
  tabyl(pi) %>%
  adorn_totals("row")
ggplot(win_one, aes(x = pi)) +
  geom_bar()
