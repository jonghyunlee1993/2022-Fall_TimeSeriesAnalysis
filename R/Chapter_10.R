rm(list=ls())

setwd("~/Workspace/2022-Fall_TimeSeriesAnalysis/data/")
par(family="AppleGothic")

# 10.5
library(astsa)
tour = scan("tourist.txt")
ts.plot(tour, ylab="tourist", main="그림 10-7 국내 입국 관광객 자료의 시계열 그림")

ltour = log(tour)
ts.plot(ltour, ylab="log tourist", main="그림 10-8 로그변환된 자료의 시계열 그림")

d12tour = diff(ltour, lag=12)
ts.plot(d12tour, ylab="seasonal diff", main="그림 10-9 로그 변환과 계절 차분된 자료의 시계열 그림")
acf2(d12tour, max.lag=36, main="그림 10-9 SACF & SPACF")

d1tour = diff(ltour, lag=1)
ts.plot(d1tour, ylab="1st diff", main="그림 10-10 로그 변환과 1차 차분된 자료의 시계열 그림")
abline(h=0)
acf2(d1tour, max.lag=36, main="그림 10-10 SACF & SPACF")

d1_12tour = diff(d12tour)
ts.plot(d1_12tour, ylab="diff", main="그림 10-11 로그 변환과 계절 및 1차 차분된 자료의 시계열 그림")
abline(h=0)
acf2(d1_12tour, max.lag=36, main="그림 10-11 SACF & SPACF")

fit1 = arima(ltour, order=c(0, 1, 1), seasonal=list(order=c(0, 1, 1), period=12))
fit1

ts.plot(resid(fit1), main="그림 10-12 잔차의 시계열 그림")
abline(h=0)

acf2(resid(fit1), main="그림 10-13 RSACF & RSPACF")
Box.test(resid(fit1), lag=6, type="Ljung", fitdf=0)

fit2 = arima(ltour, order=c(0,1,1), seasonal=list(order=c(1,1,0), period=12))
fit2

ts.plot(resid(fit2), main="잔차의 시계열 그림")
abline(h=0)

acf2(resid(fit1), main="RSACF & RSPACF")
