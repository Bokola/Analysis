# browseURL("https://noamross.github.io/gams-in-r-course/chapter1")
# install.packages("devtools")
# devtools::install_github("Bokola/glmsummary")
library(glmsummary)
pks <- c("mgcv", "gamair")
install_load_packages(pks)
?gam
# GAMs are quadratically penalized GLMs
mycle <- MASS::mcycle
plot(mycle)
lm_mod <- lm(accel ~ times, data = mycle)
# plot regression terms
termplot(lm_mod, partial.resid = TRUE, se = TRUE)
gam_mod <- gam(accel ~ s(times), data = mycle)
plot(gam_mod, residuals = TRUE, pch = 1)
# GAM is made of basis functions that together make the smooth
# temrs in the model
coef(gam_mod)
# a fit is influenced by smoothing param - sp, smoothing method - REML advised
# & number of basis functions - k
# setting sp very high e.g., sp = 1000 fits a linear relationship
# linear terms are useful for categorical variables
# for continuous terms always use splines s()

# multivariate GAM --------------------------------------------------------
data("mpg", package = "gamair")
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price),
                method = "REML", data = mpg)
plot(mod_city)
mod_city_2 <- gam(
  city.mpg ~ s(weight) + s(length) + s(price)
  + fuel + drive + style, data = mpg, method = "REML"
)
plot(mod_city_2, all.terms = TRUE)
# by drive
mod_city3 <- gam(
  city.mpg ~ s(weight, by = drive) +
    s(length, by = drive) + 
    s(price, by = drive) + drive,
  method = "REML",
  data = mpg
)
plot(mod_city3)
