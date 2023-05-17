###3
set.seed(123)
n <- 1000
delta <- 0.01
sigma_w <- 1

# create matrices to store results
x_list <- matrix(0, nrow = n, ncol = 6)
beta_hat_list <- numeric(6)
x_hat_list <- matrix(0, nrow = n, ncol = 6)

for (i in 1:6) {
  w <- rnorm(n, mean = 0, sd = sigma_w)
  x <- numeric(n)
  for (t in 2:n) {
    x[t] <- x[t-1] + delta + w[t]
  }
  
  t <- 1:n
  fit <- lm(x ~ t)
  beta_hat <- coef(fit)[2]
  x_hat <- beta_hat * t
  
  x_list[,i] <- x
  beta_hat_list[i] <- beta_hat
  x_hat_list[,i] <- x_hat
}

mu <- delta * t

# plot the results
par(mfrow = c(2, 3))
for (i in 1:6) {
  plot(t, x_list[,i], type = 'l', ylim = range(c(x_list, mu, x_hat_list)),
       xlab = 't', ylab = 'x', main = paste('Iteration', i))
  lines(t, mu, col = 'blue')
  lines(t, x_hat_list[,i], col = 'red')
  legend('bottomright', c('x', 'mu', 'x_hat'),
         col = c('black', 'blue', 'red'), lty = 1)
}

##i
library(forecast)

set.seed(123)
n <- 1000
delta <- 0.01
sigma_w <- 1

# generate random walk with drift
w <- rnorm(n, mean = 0, sd = sigma_w)
x <- numeric(n)
for (t in 2:n) {
  x[t] <- x[t-1] + delta + w[t]
}

# plot ACF and PACF
par(mfrow = c(1, 2))
acf(x)
pacf(x)

# fit ARIMA model
fit <- auto.arima(x, ic = 'aic', trace = TRUE)
summary(fit)

##Perform an ADF test with 8 lags including both intercept and trend, and report the p- value.#Convince yourself that the series is stationary by following through the full ADF procedure, if
#necessary.
library(urca)

set.seed(123)
n <- 1000
delta <- 0.01
sigma_w <- 1

# generate random walk with drift
w <- rnorm(n, mean = 0, sd = sigma_w)
x <- numeric(n)
for (t in 2:n) {
  x[t] <- x[t-1] + delta + w[t]
}

# ADF test with 8 lags
adf <- ur.df(x, lags = 8, type = "drift")
summary(adf)

###Test whether we have included enough lags in part (i) by considering only the first 20 lags of the
#residuals. Do you believe we have included enough lags in (i)?
# Fit ARIMA model to the time series
arima_model <- arima(x, order = c(1,0,0))

# Extract residuals from the model
residuals <- arima_model$residuals

# Compute ACF and PACF of residuals up to lag 20
acf_res <- acf(residuals, lag.max = 20, plot = FALSE)
pacf_res <- pacf(residuals, lag.max = 20, plot = FALSE)

# Plot ACF and PACF of residuals
par(mfrow = c(2,1))
plot(acf_res, main = "ACF of Residuals")
plot(pacf_res, main = "PACF of Residuals")

##################
#based on the ADF test results, which indicated that the time series is stationary, it suggests that the model we fitted with an autoregressive order of 1 and no differencing or moving average components was able to capture the autocorrelation in the data.

#To be sure, we can examine the ACF and PACF plots of the residuals up to lag 20 to see if there are any significant lags that were not accounted for by the model. If we see significant lags outside of the confidence intervals in the ACF or PACF plots, it suggests that we may need to include additional lags in the model to better capture the autocorrelation in the data.
###Now we would like to find a good ARMA model for your stocks returns. Do this manually
#to start with, and then use the auto.arima function to compare your best model to the best
#automatically generated model. How many AR terms are included in the ARMA model calculated
#using auto.arima?

# ACF and PACF of stock returns
acf(stock_returns)
pacf(stock_returns)
