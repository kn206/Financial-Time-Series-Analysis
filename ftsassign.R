library(readxl)
cbk <- read_excel('Kenya cbk Indicative Exchange Rates.xlsx')
View(cbk)

cbkdata<-as.ts(cbk)
is.ts(cbkdata)
View(cbkdata)

# Create a time series plot for each exchange rate
ts.plot(subset(cbkdata, select = `US DOLLAR`),
        main = "Daily Average Exchange Rates",
        ylab = "Exchange Rate (KES)",
        xlab = "Date")

ts.plot(subset(cbkdata, select = EURO),
        main = "Daily Average Exchange Rates",
        ylab = "Exchange Rate (KES)",
        xlab = "Date")

ts.plot(subset(cbkdata, select = YEN),
        main = "Daily Average Exchange Rates",
        ylab = "Exchange Rate (KES)",
        xlab = "Date")

ts.plot(subset(cbkdata, select = RAND),
        main = "Daily Average Exchange Rates",
        ylab = "Exchange Rate (KES)",
        xlab = "Date")

ts.plot(subset(cbkdata, select = `STG POUND`),
        main = "Daily Average Exchange Rates",
        ylab = "Exchange Rate (KES)",
        xlab = "Date")

# Create a time series plot for all exchange rates
ts.plot(subset(cbkdata, select = c(`US DOLLAR`, `STG POUND`, EURO, YEN, RAND)),
        main = "Daily Average Exchange Rates",
        ylab = "Exchange Rate (KES)",
        xlab = "Date",
        col = 1:5)
length(cbkdata$EURO)
# Calculate simple returns for each exchange rate
ret_usd <- diff(subset(cbkdata, select = `US DOLLAR`)) / subset(cbkdata, select = `US DOLLAR`)[-nrow(cbkdata)]
ret_ukp <- diff(subset(cbkdata, select = `STG POUND`)) / subset(cbkdata, select = `STG POUND`)[-nrow(cbkdata)]
ret_eur <- diff(subset(cbkdata, select = EURO)) / subset(cbkdata, select = EURO)[-nrow(cbkdata)]
ret_jpy <- diff(subset(cbkdata, select = YEN)) / subset(cbkdata, select = YEN)[-nrow(cbkdata)]
ret_zar <- diff(subset(cbkdata, select = RAND)) / subset(cbkdata, select = RAND)[-nrow(cbkdata)]

# Convert returns to percentages
ret_usd_pct <- ret_usd * 100
ret_ukp_pct <- ret_ukp * 100
ret_eur_pct <- ret_eur * 100
ret_jpy_pct <- ret_jpy * 100
ret_zar_pct <- ret_zar * 100


# Compute summary statistics for each exchange rate's simple returns in percentages
library("fBasics")
stats <- data.frame(
  `US DOLLAR` = c(mean(ret_usd_pct), sd(ret_usd_pct), skewness(ret_usd_pct), kurtosis(ret_usd_pct, type = "excess"), min(ret_usd_pct), max(ret_usd_pct)),
  `STG POUND` = c(mean(ret_ukp_pct), sd(ret_ukp_pct), skewness(ret_ukp_pct), kurtosis(ret_ukp_pct, type = "excess"), min(ret_ukp_pct), max(ret_ukp_pct)),
  EURO = c(mean(ret_eur_pct), sd(ret_eur_pct), skewness(ret_eur_pct), kurtosis(ret_eur_pct, type = "excess"), min(ret_eur_pct), max(ret_eur_pct)),
  YEN = c(mean(ret_jpy_pct), sd(ret_jpy_pct), skewness(ret_jpy_pct), kurtosis(ret_jpy_pct, type = "excess"), min(ret_jpy_pct), max(ret_jpy_pct)),
  RAND = c(mean(ret_zar_pct), sd(ret_zar_pct), skewness(ret_zar_pct), kurtosis(ret_zar_pct, type = "excess"), min(ret_zar_pct), max(ret_zar_pct))
)

# Label the rows with the respective statistic names
row.names(stats) <- c("Mean", "Standard Deviation", "Skewness", "Excess Kurtosis", "Minimum", "Maximum")

# Print the summary statistics in a table
print(stats)

# Transform the simple returns into log returns
log_ret_usd <- log(1 + ret_usd_pct)
log_ret_ukp <- log(1 + ret_ukp_pct)
log_ret_eur <- log(1 + ret_eur_pct)
log_ret_jpy <- log(1 + ret_jpy_pct)
log_ret_zar <- log(1 + ret_zar_pct)

# Compute summary statistics for each exchange rate's log returns
log_stats <- data.frame(
  USD = c(mean(log_ret_usd), var(log_ret_usd), skewness(log_ret_usd), kurtosis(log_ret_usd, type = "excess"), min(log_ret_usd), max(log_ret_usd)),
  UKP = c(mean(log_ret_ukp), var(log_ret_ukp), skewness(log_ret_ukp), kurtosis(log_ret_ukp, type = "excess"), min(log_ret_ukp), max(log_ret_ukp)),
  EURO = c(mean(log_ret_eur), var(log_ret_eur), skewness(log_ret_eur), kurtosis(log_ret_eur, type = "excess"), min(log_ret_eur), max(log_ret_eur)),
  YEN = c(mean(log_ret_jpy), var(log_ret_jpy), skewness(log_ret_jpy), kurtosis(log_ret_jpy, type = "excess"), min(log_ret_jpy), max(log_ret_jpy)),
  RAND = c(mean(log_ret_zar), var(log_ret_zar), skewness(log_ret_zar), kurtosis(log_ret_zar, type = "excess"), min(log_ret_zar), max(log_ret_zar))
)

# Label the rows with the respective statistic names
row.names(log_stats) <- c("Mean", "Variance", "Skewness", "Excess Kurtosis", "Minimum", "Maximum")

# Print the summary statistics in a table
print(log_stats)

# Perform a two-sided t-test for each exchange rate's log returns
t_test_usd <- t.test(log_ret_usd, mu = 0)
t_test_ukp <- t.test(log_ret_ukp, mu = 0)
t_test_eur <- t.test(log_ret_eur, mu = 0)
t_test_jpy <- t.test(log_ret_jpy, mu = 0)
t_test_zar <- t.test(log_ret_zar, mu = 0)

# Extract the p-values from each t-test
p_values <- c(t_test_usd$p.value, t_test_ukp$p.value, t_test_eur$p.value, t_test_jpy$p.value, t_test_zar$p.value)

# Print the p-values
print(p_values)

# Plot the histograms of daily simple returns
par(mfrow=c(2,5))
for(i in 1:5) {
  hist(simple_returns[,i], main = names(simple_returns)[i], xlab = "Simple Returns", breaks = 30, col = "blue")
  x <- seq(min(simple_returns[,i]), max(simple_returns[,i]), length = 100)
  y <- dnorm(x, mean = mean(simple_returns[,i]), sd = sd(simple_returns[,i]))
  lines(x, y*length(simple_returns[,i])*diff(hist(simple_returns[,i], plot = FALSE)$breaks)[1], col = "red", lwd = 2)
}

# Plot the histograms of daily log returns
par(mfrow=c(2,5))
for(i in 1:5) {
  hist(log_returns[,i], main = names(log_returns)[i], xlab = "Log Returns", breaks = 30, col = "blue")
  x <- seq(min(log_returns[,i]), max(log_returns[,i]), length = 100)
  y <- dnorm(x, mean = mean(log_returns[,i]), sd = sd(log_returns[,i]))
  lines(x, y*length(log_returns[,i])*diff(hist(log_returns[,i], plot = FALSE)$breaks)[1], col = "red", lwd = 2)
}

##generate random variable
y <- rnorm(4643, mean = mean(cbkdata$`US DOLLAR`), sd = sqrt(var(cbkdata$`US DOLLAR`)))

plot(density(y), main = "Density Plot of Simulated Returns vs USD/KES Exchange Rate Log Returns")
lines(density(usd), col = "red")
legend("topright", legend = c("Simulated Returns", "USD/KES Exchange Rate Log Returns"), 
       col = c("black", "red"), lty = 1)

library(moments)

# Compute the Jarque-Bera test statistic and p-value
jbtest <- jarque.bera.test(usd)
jbstat <- jbtest$jb[1]
jbpval <- jbtest$p.value[1]

# Print the test results
cat("Jarque-Bera test results:\n")
cat("Test statistic: ", jbstat, "\n")
cat("p-value: ", jbpval, "\n")

# Check if null hypothesis is rejected at 5% significance level
if (jbpval < 0.05) {
  cat("Null hypothesis rejected. Log returns are not normally distributed.\n")
} else {
  cat("Null hypothesis not rejected. Log returns may be normally distributed.\n")
}
