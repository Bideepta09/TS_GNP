rm(list=ls())
library(astsa)
d=gnp;d
plot(d)
plot(gnp, main="US GNP Time Series", ylab="GNP (billions of dollars)", xlab="Year")
gnp_ts=ts(gnp, start = c(1947, 1), frequency = 4) #converted into ts object
plot(gnp_ts)

decomposed_gnp=decompose(gnp_ts)
decomposed_gnp
print(summary(decomposed_gnp))
par(mfrow=c(2,2))
plot(gnp_ts,ylab="observed")
plot(decomposed_gnp$trend,ylab="trend")
plot(decomposed_gnp$seasonal,ylab="seasonal")
plot(decomposed_gnp$random,ylab="random")
par(mfrow=c(1,1))
#detrended series
plot(gnp-decomposed_gnp$trend-decomposed_gnp$seasonal,ylab="Time series value",main="Detrended series")

library(tseries)
adf_test=adf.test(gnp_ts)
print(adf_test)
gnp_diff=diff(gnp_ts);gnp_diff
plot(gnp_diff,main="Differenced time series")
print(adf.test(gnp_diff))
par(mfrow = c(1,1))
acf(gnp_diff, main = "ACF of Differenced GNP")
pacf(gnp_diff, main = "PACF of Differenced GNP")
library(forecast)
arima_model_1=arima(gnp_ts,order=c(0,1,1))
summary(arima_model_1)
arima_model_2=auto.arima(gnp_ts)
summary(arima_model_2)