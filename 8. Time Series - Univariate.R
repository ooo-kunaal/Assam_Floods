library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)

# 1. Load data and create time series
df <- read.csv("final_data.csv")
df_ts <- ts(df$Flood, start = c(2000), frequency = 1)  # annual frequency

# 2. Plot original time series
ggtsdisplay(df_ts,smooth = T)
# strong autocorrelation with PACF cut-off at lag 1 => supports ARIMA(0, 1, q) models

# 3. Select best ARIMA model
auto_arima_model <- auto.arima(df_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(auto_arima_model) 
# ARIMA(0,1, 3)
# Xt = yt - yt-1 (difference once)
# Xt = -0.25*et-1 + 0.71*et-2 -0.56*et-3 + et
# where, et~ N(0, 6484)
# RMSE = 73.8, AIC = 286.75 (better)
checkresiduals(auto_arima_model)
# Normality: Reasonably bell-shaped.
# Autocorrelation: Ljung-Box p = 0.35 → no significant autocorrelation.

# 4. ETS Model
ets_model <- ets(df_ts)
summary(ets_model)
checkresiduals(ets_model)
# smoothing parameter alpha = 0.6599 (model gives more weight to recent observations which is expected for non-stationary processes)
# RMSE = 95.34, AIC = 314.34
# Residuals: More erratic.
# Ljung-Box p = 0.13 → acceptable but weaker autocorrelation structure.

# 5a. Actual vs fitted - scatter for ARIMA
plot(as.numeric(auto_arima_model$fitted), df$Flood, xlab='Fitted values',ylab='Actual values',main='Actual vs Fitted for ARIMA(0,1,3)')
abline(0,1, col='navy',lty=2)
# Close fit to 45° line, though slight deviation at high flood counts.

# 5b. Actual vs fitted - over time for ARIMA
plot(df_ts, xlab='Years',ylab='Flood Count',main='Actual vs Fitted for ARIMA(0,1,3) (over time)')
points(df_ts, cex=0.8, lwd=2)
lines(auto_arima_model$fitted, col='darkorange', lwd=2, lty=2)
points(auto_arima_model$fitted, cex=0.8, lwd=2, col='darkorange',)
legend('bottomright',c('Actual','Predicted'),col=c('black','darkorange'), lty=c(1,2),lwd=2, bty='n')
# Captures rising trend post-2013 quite well, despite small underestimation in peaks

# 6a. Actual vs fitted - scatter for ETS
plot(as.numeric(ets_model$fitted), df$Flood, xlab='Fitted values',ylab='Actual values',main='Actual vs Fitted for ETS')
abline(0,1, col='navy',lty=2)
# Slightly more scatter around 45° line.

# 6b. Actual vs fitted - over time for ETS
plot(df_ts, xlab='Years',ylab='Flood Count',main='Actual vs Fitted for ETS (over time)')
points(df_ts, cex=0.8, lwd=2)
lines(ets_model$fitted, col='darkorange', lwd=2, lty=2)
points(ets_model$fitted, cex=0.8, lwd=2, col='darkorange',)
legend('bottomright',c('Actual','Predicted'),col=c('black','darkorange'), lty=c(1,2),lwd=2, bty='n')
# Tracks broad trend but lags behind ARIMA in capturing year-to-year variability.

