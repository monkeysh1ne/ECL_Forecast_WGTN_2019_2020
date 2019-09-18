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
library(forecast)

# Load interactive plotting package
library(plotly)

# Load data into dataframe
data <- read.csv('WGT Deliveries 0716_to_0619.csv', stringsAsFactors = FALSE)

# Plot data before timeseries conversion
plot(WGTN_DEL ~ WE_DATE, data=data)


# Declare this as time series data
tsData <- ts(data[, 2], start = c(2016,7), end = c(2019,6), frequency = 52)

# Subset the timeseries (Jan 2019 to Jun 2019)
Z <- window(tsData, start=c(2016, 9), end=c(2017, 2))
# Plot series
plot(tsData)

##################################################
# Preliminary analysis
##################################################
# Time plot
autoplot(tsData) +
        ggtitle("Time Plot: Wellington(Ngaio) Deliveries 2016 - 2019 per Week") +
        ylab("# Deliveries")

# Data has a slight upward trend trend.
# Take first difference of data to remove trend.
diff_tsData <- diff(tsData)
# Time plot of difference data.
autoplot(diff_tsData) +
        ggtitle("Time Plot: Change in Wellington(Ngaio) Deliveries 2016 - 2019 per Week") +
        ylab("# Deliveries")
# Series appears trend-stationary, use to investigate seasonality.
ggseasonplot(diff_tsData) +
        ggtitle("Seasonal Plot: Change in Weekly Deliveries, Wellington(Ngaio) 2016 - 2019")

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
fit <- snaive(diff_tsData)
print(summary(fit))
checkresiduals(fit)

fit_ets <- ets(tsData) # Residuals = 0.1508
print(summary(fit_ets))
checkresiduals(fit_ets)

plot(stlf(tsData, lambda=0))

###############################
# Fit an ARIMA model.
###############################
fit_arima <- auto.arima(tsData, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)

###############################
# Forecast with ETS model.
###############################
fcast <- forecast(fit_arima, h = 6)
autoplot(fcast)
print(summary(fcast))

###############################
# Produce interactive line chart using plotly
###############################

