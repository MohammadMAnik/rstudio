rm(list = ls())

# Nile
Nile

plot(Nile)

Nile.diff1 <- diff(Nile, differences = 1)
plot(Nile.diff1)

Nile.diff2 <- diff(Nile, differences = 2)
plot(Nile.diff2)

# acf
acf(Nile.diff2, lag.max = 20)
acf(Nile.diff2, lag.max = 20, plot = FALSE)

# pacf
pacf(Nile.diff2, lag.max = 20)
pacf(Nile.diff2, lag.max = 20, plot = FALSE)

install.packages('tseries')
library(tseries)

install.packages('forecast')
library(forecast)

# auto.arima
auto.arima(Nile)

# 
Nile.arima <- arima(Nile, order = c(1,1,1))
Nile.arima

#
Nile.forecast <- forecast(Nile.arima, h = 10)
Nile.forecast

plot(Nile.forecast)















