rm(list=ls())

setwd("~/Workspace/2022-Fall_TimeSeriesAnalysis/")
par(family="AppleGothic")

library(forecast)
library(lubridate)

# Example 4.1
z = scan("data/food.txt")
t = 1:length(z)
food = ts(z, start=c(1981, 1),frequency=12)
fit = lm(food ~ t)

anova(fit)
trend = fitted(fit)
ts.plot(food, trend, col=1:2, lty=1:2, ylab="food", xlab="time",
        main="그림 4-1 원시계열과 분해법에 의한 추세성분")
legend("topleft", lty=1:2, col=1:2, c("원시계열", "추세성분"))
adjtrend = food / trend

y = factor(cycle(adjtrend))
fit1 = auto.arima(adjtrend, max.p=2, xreg=model.matrix(~ 0 + y)[, -12],
                  seasonal=F, max.d=0, max.q=0)
fit1

seasonal = fit1$fitted
pred = trend * seasonal
irregular = food / pred
ts.plot(seasonal, main="그림 4-2 분해법에 의한 계절성분")
ts.plot(irregular, main="그림 4-3 분해법에 의한 불규칙성분")

acf(irregular, main="불규칙 성분의 ACF")
ts.plot(food, pred, lty=1:2, ylab="food", col=c("blue", "red"),
        main="그림 4-4 원시계열과 예측값")
legend("topleft", lty=1:2, col=c("blue", "red"),
       c("원시계열", "예측값"))

date = ymd("810101") + months(1:length(food) - 1)
table4 = data.frame(date, food, trend, seasonal, irregular)
table4

# Example 4.2
z = scan("data/mindex.txt")
mindex = ts(z, start=c(1986, 1), frequency=12)
m3 = ma(mindex, 3)
m7 = ma(mindex, 7)

plot(mindex, lty=1, main="그림 4-5 중간재 출하지수와 이동평균 m=3")
lines(m3, lwd=1, col="red", lty=2)
legend("topleft", lty=1:2, col=c(1, "red"), c("index", "MA3"))

plot(mindex, lty=1, main="그림 4-6 중간재 출하지수와 이동평균 m=7")
lines(m7, lwd=1, col="blue", lty=2)
legend("topleft", lty=1:2, col=c(1, "red"), c("index", "MA7"))

# Example 4.3
z = scan("data/food.txt")
food = ts(z, start=c(1981, 1), frequency=12)

m = decompose(food, type=c("additive"))
trend = trendcycle(m)
seasonal = seasonal(m)
irregular = remainder(m)
adjseasonal = food - seasonal

ts.plot(food, trend, ylab="food", lty=1:2, col=c("blue", "red"),
        main="그림 4-7 원시계열과 추세, 순환 성분")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "추세, 순환"))

ts.plot(food, seasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="그림 4-8 원시계열과 계졀 성분")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절성분"))

ts.plot(food, irregular, ylab="food", lty=1:2, col=c("blue", "red"),
        main="그림 4-9 원시계열과 불규칙 성분")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "불규칙성분"))

ts.plot(food, adjseasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="그림 4-10 원시계열과 계절 조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))

food.stl = stl(food, "periodic")
food.sa = seasadj(food.stl)
ts.plot(food, food.sa, ylab="food", lty=1:2, col=c("blue", "red"),
        main="Figure 4-10 원시계열과 계졀조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))


# Exercise 4.4 - 1
z = scan("data/build.txt")
build = ts(z, start=c(1981, 1), frequency=12)

m = decompose(build, type=c("additive"))
trend = trendcycle(m)
seasonal = seasonal(m)
irregular = remainder(m)
adjseasonal = build - seasonal

ts.plot(build, adjseasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="원시계열과 계절 조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))


# Exercise 4.4 - 2
z = scan("data/export.txt")
export = ts(z, start=c(1981, 1), frequency=12)

m = decompose(export, type=c("additive"))
trend = trendcycle(m)
seasonal = seasonal(m)
irregular = remainder(m)
adjseasonal = export - seasonal

ts.plot(export, adjseasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="원시계열과 계절 조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))


# Exercise 4.4 - 3
z = scan("data/usapass.txt")
usapass = ts(z, start=c(1981, 1), frequency=12)

m = decompose(usapass, type=c("additive"))
trend = trendcycle(m)
seasonal = seasonal(m)
irregular = remainder(m)
adjseasonal = usapass - seasonal

ts.plot(usapass, adjseasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="원시계열과 계절 조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))


# Exercise 4.4 - 4
z = scan("data/depart.txt")
depart = ts(z, start=c(1981, 1), frequency=12)

m = decompose(depart, type=c("additive"))
trend = trendcycle(m)
seasonal = seasonal(m)
irregular = remainder(m)
adjseasonal = depart - seasonal

ts.plot(depart, adjseasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="원시계열과 계절 조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))


# Exercise 4.4 - 5
z = scan("data/koreapass.txt")
koreapass = ts(z, start=c(1981, 1), frequency=12)

m = decompose(koreapass, type=c("additive"))
trend = trendcycle(m)
seasonal = seasonal(m)
irregular = remainder(m)
adjseasonal = koreapass - seasonal

ts.plot(koreapass, adjseasonal, ylab="food", lty=1:2, col=c("blue", "red"),
        main="원시계열과 계절 조정")
legend("topleft", lty=1:2, col=c("blue", "red"), c("원시계열", "계절조정"))

