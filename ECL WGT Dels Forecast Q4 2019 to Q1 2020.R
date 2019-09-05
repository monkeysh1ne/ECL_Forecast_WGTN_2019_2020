#########################################
# This file loads weekly totals of deliveries (WE Sunday)
# for ECL Ngaio.
# Source: 'S:/WCP/FLEET/Darrell/Reports/data/ALL_WCP/Forecasting/Weekly Dashboard Volumes.xlsx'
# Author: Darrell Mayson
# Date Last Modified: 4/9/19
#########################################

# Clear all vars in workspace
rm(list=ls())


# Load the forecasting package
library(fpp2)

# Load interactive plotting package
library(plotly)

# Load data into dataframe
data <- read.csv('WGT Deliveries 0717_to_0719.csv')


# Declare this as time series data
Y <- ts(data[, 2], start = c(2017,1), end = c(2019,6), frequency = 104)

##################################################
# Preliminary analysis
##################################################
# Time plot
autoplot(Y) +
        ggtitle("Time Plot: Wellington(Ngaio) Deliveries 2017 - 2018 per Week") +
        ylab("# Deliveries")

# Data has a slight upward trend trend.
# Take first difference of data to remove trend.
DY <- diff(Y)
# Time plot of difference data.
autoplot(DY) +
        ggtitle("Time Plot: Change in Wellington(Ngaio) Deliveries 2017 - 2019 per Week") +
        ylab("# Deliveries")
# Series appears trend-stationary, use to investigate seasonality.
ggseasonplot(DY) +
        ggtitle("Seasonal Plot: Change in Weekly Deliveries, Wellington(Ngaio) 2017 - 2019")

# Look at another seasonal plot, the subseries plot. Only possible when observations in whole year multiples.
# ggsubseriesplot(DY)

####################################################
# This series (Y) has trend and seasonality.
# To remove the trend we take the first difference.
# The first difference series still has seasonality.
#
# Forecast with various methods.
####################################################

####################################################
# Use a benchmark method to forecast.
# Use seasonal naive method as our benchmark.
# y_t = y_{t-s} + e_t
####################################################

# Following produces NaN results en mass.  Cannot apply to current data.
# fit <- snaive(DY)
# print(summary(fit))
# checkresiduals(fit)

fit_ets <- ets(Y) # Residuals = 0.1508
print(summary(fit_ets))
checkresiduals(fit_ets)

###############################
# Fit an ARIMA model.
###############################
fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)

###############################
# Forecast with ETS model.
###############################
fcast <- forecast(fit_arima, h = 26)
autoplot(fcast)
print(summary(fcast))