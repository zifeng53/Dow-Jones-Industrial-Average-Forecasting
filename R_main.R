install.packages("AICcmodavg")

library(AICcmodavg)
library(fpp2)
library(forecast)
library(tseries)
library(ggplot2)
library(graphics)
library(MLmetrics)
library(stat)
library(stat4)
library(portes)
library(lmtest)
library("writexl")
library("urca")

setwd("~/StockPricePrediction")
file.exists('C:\\Users\\User\\OneDrive\\Desktop\\Dow Jones Industrial Average Historical Data.csv')
df <- read.csv(file = 'C:\\Users\\User\\OneDrive\\Desktop\\Dow Jones Industrial Average Historical Data.csv', header=TRUE, stringsAsFactors = FALSE)
head(df)

is.ts(df)
closePrice <- df[[2]]
head(closePrice)
Y <- c(closePrice)
Y <- ts(Y, frequency = 12, start = 1990)
Y
plot(Y, type="o", col="Blue", main="Closing Price", xlab="Year", ylab="Price")
acf(Y, main="ACF", lag.max =  100)
# PACF
# Is the degree of association between two variables while adjusting the effect of one or more additional variables
pacf(Y, main="PACF")
pacf(Y, main="PACF", lag.max =  100)
# have trend and seasonality effect, non stationary
acf(diff(Y), lag.max =  100, main = "Diff ACF") # residuals not WN
pacf(diff(Y), lag.max =  100, main = "Diff PACF")
## Boxplot to see the seasonal effect
boxplot(Y~cycle(Y))

# Additive Decomposition
components <- decompose(Y, type="additive")
plot(components)
cbind(components$x, components$trend, components$seasonal, components$random)

components <- decompose ((diff(Y)), type="additive")
plot(components)
cbind(components$x, components$trend, components$seasonal, components$random)

# Stationary Test
# Augmented Dickey-Fuller Test 
# ADF test belongs to a category of tests called Unit Root Test,
adf.test(Y, alternative = c("stationary","explosive"),
         k = trunc((length(Y)-1)^(1/3)))
adf.test(diff(Y))
#adf.test(Y, k=2)

# Test on White Noise
wn <- rnorm(Y)
tseries::adf.test(wn)
# p-value = 0.0247
# Null hypothesis is rejected. It is stationary
tseries::adf.test(wn,k=0)
# p-value = 0.01

# Test on White Noise with Trend
intercept <- 1
wnt <- wn + 1 :Y+intercept
tseries::adf.test(wnt)
# p-value = 0.01452
# Null hypothesis is rejected. It is stationary

# Test on Random Walk
rw <- cumsum(rnorm(Y))
tseries::adf.test(rw)
# p-value = 0.91
# Null hypothesis is not rejected as the p-value is greater than 0.05
# Non stationary

tseries::adf.test(rw,k=0)
# p-value = 0.98
# Null hypothesis is not rejected as the p-value is greater than 0.05
# Non stationary

# Philips Perron Test
pp.test(Y)
# Truncation lag parameter = 5, p-value = 0.01
# Stationary
pp.test(diff(Y))
# Truncation lag parameter = 5, p-value = 0.01
# Stationary

# KPSS Test (Null hypo = stationary)
tseries::kpss.test(Y)
# p-value = 0.1 > 0.05 = Stationary, cant reject H0
tseries::kpss.test(diff(Y))
# p-value = 0.1 > 0.05 = Stationary, cant reject H0

# Test on white noise with a trend
tseries::kpss.test(wnt, null="Trend")
# p-value = 0.1 > 0.05, cant reject Null hypo of
# stationarity around a trend is not rejected

# Test on white noise with a trend but default of stationary with no trend
tseries::kpss.test(wnt, null = "Level")
# p-value = 0.1 > 0.05, cant reject null hypo of
# stationarity aroudn a level cannot reject


hw_additive <- hw(Y, seasonal = "additive")
fitted(hw_additive)
plot.ts(Y, main="Smoothed Timeseries of Close Price", col="blue")
lines(fitted(hw_additive),col="red")
hw_additive$model
hw_forecast <- forecast(hw_additive,12)
hw_forecast
plot(hw_forecast)
states <- hw_additive$model$states[,1:3]
states
colnames(states) <- cbind("Level","Trend", "Seasonality")
plot(states,col="blue",main="Decomposition of Time Series")
plot(residuals(hw_additive))
acf(residuals(hw_additive))
acf(residuals(hw_forecast))
summary(hw_forecast)