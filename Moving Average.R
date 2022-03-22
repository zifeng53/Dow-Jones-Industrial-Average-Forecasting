library(forecast)
library(fpp2)
library(tseries)
library(ggplot2)
library(graphics)
library(MLmetrics)
library(stats)
library(stats4)
df <- read.csv(file = 'C:\\Users\\nn\\Desktop\\Data.csv')
head(df)
is.ts(df)
Close <- df[[3]]
Date <- df[[2]]
head(Close)
Y <- c(Close)
Y <- ts(Y,Date)
plot(Y,type="o",col="Blue",main="DJIA",xlab="Period",ylab="Closing Price")

fit <-tslm(Y~trend)
Forecast <- forecast(fit,h=30)
Forecast
plot(Forecast)



sma3 <- ma(Y,3,centre =FALSE)
sma3fit <- forecast(sma3,h=12)
sma3fit
plot(sma3fit, main="Forecast using 3-months moving average",xlab="Period",ylab="Price")


sma6 <- ma(Y,6,centre =FALSE)
sma6fit <- forecast(sma6,h=12)
sma6fit
plot(sma6fit, main="Forecast using 6-months moving average",xlab="Period",ylab="Price")