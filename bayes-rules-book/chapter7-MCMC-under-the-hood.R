browseURL("https://www.bayesrulesbook.com/chapter-7.html")
# packages
ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <- c("Rcpp", "rstan", "bayesrules", "tidyverse", "janitor", "bayesplot")
ipk(list.pkg)

set.seed(1234)
mc_tour <- data.frame(mu = rnorm(5000, mean = 4, sd = 0.6))
ggplot(mc_tour, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 15) + 
  stat_function(fun = dnorm, args = list(4, 0.6), color = "blue")


# understanding Metropolis-Hastings ---------------------------------------


# R simulation to understand scenario 2 when the ratio of unnormalized
# pdfs < 1
# suppose we're utilizing a Uniform proposal model with half-width w = 1
current <- 3
set.seed(8)
proposal <- runif(1, min = current - 1, max = current + 1)
proposal
proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
proposal_plaus
current_plaus <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
current_plaus
# probability of accepting and subsequently moving to proposed location
alpha <- min(1, proposal_plaus/ current_plaus)
alpha
# to make the final determination, we set up a weighted coin which accepts the 
# proposal with a probability alpha = 0.05 and rejects it by 1-alpha = 0.95
next_stop <- sample(c(proposal, current), size = 1, prob = c(alpha, 1 - alpha))
next_stop

# bundle it to a function
one_mh_iteration <- function(w, current){
  # STEP 1: Propose the next chain location
  proposal <- runif(1, min = current - w, max = current + w)
  
  # STEP 2: Decide whether or not to go there
  proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
  current_plaus  <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
  alpha <- min(1, proposal_plaus / current_plaus)
  next_stop <- sample(c(proposal, current), 
                      size = 1, prob = c(alpha, 1-alpha))
  
  # Return the results
  return(data.frame(proposal, alpha, next_stop))
}

set.seed(8)
one_mh_iteration(w = 1, current = 3)

# implementing Metropolis-Hastings ----------------------------------------

# the mh_tour() function below constructs a Metropolis-Hastings tour of 
# any given length N utilizing a Uniform proposal model with any given
# half-width w

mh_tour <-function(N, w){
  # 1. Start the chain at location 3
  current <- 3
  # 2. initialize the simulation
  mu <- rep(0, N)
  # 3. simulate N Markov chain stops
  for(i in 1:N){
    # simulate one iteration
    sim <- one_mh_iteration(w = w, current = current)
    # record next location
    mu[i] <- sim$next_stop
    # reset the current location
    current <- sim$next_stop
  }
  # 4. return the chain locations
  return(data.frame(iteration = c(1:N), mu))
}
set.seed(1234)
mh_simulation_1 <- mh_tour(N = 5000, w = 1)

# trace plot
ggplot(mh_simulation_1, aes(x = iteration, y = mu)) +
  geom_line()

ggplot(mh_simulation_1, aes(x = mu)) +
  geom_histogram(aes(y = ..density..), color = "white", bins = 20) +
  stat_function(fun = dnorm, args = list(4, 0.6), color = "blue")

# tuning the Metropolis-Hastings algorithm --------------------------------
# w too small, neighbourhood tight together, proposals are always accepted
set.seed(84735)
mh_simulation_2 <- mh_tour(N = 5000, w = 0.01)
ggplot(mh_simulation_2, aes(x = iteration, y = mu)) + 
  geom_line() + 
  lims(y = c(1.6, 6.4))
# w too large, proposals can be far flung - proposals always rejected, resulting
# in a tour which gets stuck at the same location for multiple iterations in a row
set.seed(7)
mh_simulation_3 <- mh_tour(N = 5000, w = 100)
ggplot(mh_simulation_3, aes(x = iteration, y = mu)) + 
  geom_line() + 
  lims(y = c(1.6,6.4))

# a beta-binomial example -------------------------------------------------

# uses independent sampling algorithm

one_iteration <- function(a, b, current){
  # step 1: propose the next chain location
  proposal <- rbeta(1, a, b)
  # step 2: decide whether or not to go there
  proposal_plaus <- dbeta(proposal, 2, 3) * dbinom(1, 2, proposal) / 
    dbeta(proposal, a, b)
  current_plaus  <- dbeta(current, 2, 3) * dbinom(1, 2, current) / 
    dbeta(current, a, b)
  alpha <- min(1, proposal_plaus / current_plaus)
  next_stop <- sample(c(proposal, current), size = 1, prob = c(alpha, 1 - alpha))
  
  return(data.frame(proposal, alpha, next_stop))
}

# Subsequently, we write a betabin_tour() function which constructs an N-length Markov chain tour for any
# Beta(a,b)  proposal model, utilizing one_iteration() to determine each stop:
betabin_tour <- function(N, a, b){
  # 1. Start the chain at location 0.5
  current <- 0.5
  
  # 2. Initialize the simulation
  pi <- rep(0, N)
  
  # 3. Simulate N Markov chain stops
  for(i in 1:N){    
    # Simulate one iteration
    sim <- one_iteration(a = a, b = b, current = current)
    
    # Record next location
    pi[i] <- sim$next_stop
    
    # Reset the current location
    current <- sim$next_stop
  }
  
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), pi))
}

set.seed(1234)
betabin_sim <- betabin_tour(N = 5000, a = 1, b = 1)

# Plot the results
ggplot(betabin_sim, aes(x = iteration, y = pi)) + 
  geom_line()
ggplot(betabin_sim, aes(x = pi)) + 
  geom_histogram(aes(y = ..density..), color = "white") + 
  stat_function(fun = dbeta, args = list(3, 4), color = "blue")
