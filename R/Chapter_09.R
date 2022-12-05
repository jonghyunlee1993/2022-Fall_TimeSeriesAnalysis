rm(list=ls())

setwd("~/Workspace/2022-Fall_TimeSeriesAnalysis/data/")
par(family="AppleGothic")

# 9.1
library(astsa)
z = scan("eg8_7.txt")
par(mfrow=c(1,1))
ts.plot(z, ylab="z", main="Simulated AR(1) process")
acf2(z, max.lag=24, main="AR(1) 과정의 ACF & PACF")
sarima.for(z, 25, 1, 0, 0)

# 9.5
z = scan("eg9_5.txt")
par(mfrow=c(1,1))
ts.plot(z, ylab="z", main="그림 9-2 모의 실험 자료")
acf2(z, max.lag=24, main="그림 9-2 SACF & SPACF")

par(mfrow=c(1,1))
ts.plot(diff(z), main="그림 9-3 1차 차분된 모의 실험 자료")
abline(h=0)
acf2(diff(z), max.lag=24, main="그림 9-3 SACF & SPACF")

fit = arima(z, order=c(0, 1, 1))
par(mfrow=c(1,1))
ts.plot(resid(fit), main="그림 9-4 ARIMA(0, 1, 1) 적합 후의 잔차")
abline(h=0)

acf2(resid(fit), main="그림 9-4 잔차의 SACF & SPACF")
sarima.for(z, 25, 0, 1, 1)




