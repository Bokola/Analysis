browseURL("https://jtr13.github.io/cc19/time-series-modeling-with-arima-in-r.html")
# ARIMA(p,d,q): 
# Auto-regressive components, AR(p) implies use of past values in the regression
# Moving average (MA(q)) which accounts for error of the model
# differencing variable d used to remove trend & convert a
# non-stationary time series to a stationary one

# ARIMA modelling steps:

# 1. visualize the time series
# 2. stationarize the time series
# 3. plot ACF/PACF and final optimal parameters
# 4. build the ARIMA model
# 5. make predictions

if(!require(glmsummary))  devtools::install_github("Bokola/glmsummary")
pks <- c("tidyquant", "tidyverse")
install_load_packages(pks)

# #1. visualize -----------------------------------------------------------


jnj <- tq_get("JNJ", get = "stock.prices", from = "1997-01-01") %>%
  tq_transmute(
    mutate_fun = to.period, period = "months"
  )
# closing prices of stock
ggplot(jnj, aes(date, close)) + geom_line()
# stationarize using log difference
plot(diff(log(jnj$close)), type = "l", main = "log returns plot")

# #2. stationarize --------------------------------------------------------

library(tseries)

# stationary = means/vars/covars constant with time
# to estimate params a ts must be stationary
# k is the lag order
adf.test(diff(log(jnj$close)), alternative = "stationary", k = 0)


# #3. ACF/PACF ------------------------------------------------------------

# ACF = Auto-correlation function - gives us 
# values of any auto-correlation with its
# lagged function
# ACF determines order moving average (MA) coeffs
# PACF = partial auto-correlation function.
# finds correlation of the residuals with the next lag value
# PACF determines no. of AR coeffs

# ACF/PACF  determines the order of our params in ARIMA model

acf(diff(log(jnj$close)))
# MA(1) - curve drops significantly after first lag
pacf(diff(log(jnj$close)))
# AR(3) - difficult to read, assume 3

# model as (3, 0, 1) (p,d,q)


# #4. build ARIMA model ---------------------------------------------------

library(forecast)
(fit <- arima(diff(log(jnj$close)), c(3,0,1)))
# fit is examined by AIC
# iterate with the params till you get lowest AIC

# automated ARIMA
fitARIMA <- auto.arima(diff(log(jnj$close)), trace = TRUE)


# #5. make predictions ----------------------------------------------------


# first let's see how the model fitted the data
plot(as.ts(diff(log(jnj$close))))
lines(fitted(fitARIMA), col = "red")

# predict next 5 months at 99% confidence level
futueval <- forecast(fitARIMA, h = 5, level = c(99)) 
plot(forecast(futueval))

futueval$mean

