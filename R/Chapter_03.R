rm(list=ls())
setwd("~/Workspace/2022-Fall_TimeSeriesAnalysis/")
par(family="AppleGothic")

# Example 3.1
library(forecast)
z = scan("data/mindex.txt")
mindex = ts(z, start=c(1986, 1), frequency=12)

w = c(seq(0.1, 0.8, 0.1), seq(0.81, 0.99, 0.01))
sse = sapply(w, function(x)
  return(sum(ses(mindex, alpha=x, h=6)$residuals^2)))
w1 = w[-c(1:6)]
sse1 = sse[-c(1:6)]

plot(w1, sse1, type="o", xlab="weight", ylab="sse",
     main="Figure3-2 1 시차 후 예측오차의 제곱합")
w[which.min(sse)]

fit1 = ses(mindex, alpha=0.9, h=6)
acf(resid(fit1), lag=12)
t.test(resid(fit1), mu=0)

plot(fit1, xlab="year", ylab="minindex",
     main="Figure 3-1 중간재 출하지수와 단순지수평활값 alpha=0.9",
     lty=1, col="black")
lines(fitted(fit1), col="red", lty=2)
legend("topright", legend=c("Mindex", "alpha=0.9"),
       lty=1:2, col=c("black", "red"))

plot(fit1$residuals, ylab="residual",
     main="그림 3-4 예측오차의 시계열 그림: alpha=0.9")
abline(h=0)

fit2 = ses(mindex, alpha=0.2, h=6)
t.test(resid(fit2), mu=0)
acf(resid(fit2), lag.max=12)
plot(fit2, xlab="year", ylab="mindex",
     main="그림 3-3 중간재 출하지수와 단순지수평활값 alpha=0.2",
     lty=1, col="black")
lines(fitted(fit2), col="red", lty=2)
legend("topright", legend=c("Mindex", "alpha=0.2"),
       lty=1:2, col=c("black", "red"))

plot(fit2$residuals, ylab="residual",
     main="그림 3-5 예측오차의 시계열 그림: alpha=0.2")
abline(h=0)

# 두 모형의 비교
round(rbind(accuracy(fit1), accuracy(fit2)), digit=3)

fit3 = ses(mindex, h=6)
fit3$model
plot(fit3, xlab="year", ylab="mindex",
     main="중간재 출하지수와 단순지수평활값 alpha estimated",
     lty=1, col="black")
lines(fit3$fitted, col="red", lty=2)
legend("topright", legend=c("Mindex", "estimated_alpha"),
       lty=1:2, col=c("black", "red"))

plot(fit3$residuals, ylab="residual",
     main="예측오차의 시계열그림: 추정된 alpha")
abline(h=0)

# Example 3.2
library(forecast)
z = scan("data/stock.txt")
stock = ts(z, start=c(1984, 1), frequency=12)

# 1모수 이중지수평활
fit4 = holt(stock, alpha=0.6, beta=0.6, h=6)
fit4$model

plot(fit4, ylab="Index", xlab="year",
     lty=1, col="black",
     main="그림 3-6 중간재 출하지수와 이중지수평활값: alpha=beta=0.6")
lines(fitted(fit4), col="red", lty=2)
legend("topleft", lty=1:2, col=c("black", "red"), c("Index", "Double"))

plot(resid(fit4), main="그림 3-7 예측오차의 시계열 그림")
abline(h=0)

acf(resid(fit4))
t.test(resid(fit4), mu=0)


# 2모수 이중지수평활 alpha, beta 추정
fit5 = holt(stock, h=6)
fit5$model
plot(fit5, ylab="Index", xlab="year", lty=1, col="black",
     main="중간재 출하지수와 이중지수평활값: alpha, beta estimated")
lines(fitted(fit5), col="red", lty=2)
legend("topright", lty=1:2, col=c("black", "red"), c("Index", "Double"))

plot(resid(fit5), main="예측오차의 시계열그림: alpha, beta estimated")
abline(h=0)
acf(resid(fit5))
t.test(resid(fit5), mu=0)

## Example 3.3
library(forecast)
library(astsa)
z = scan("data/koreapass.txt")
pass = ts(z, start=c(1981, 1), frequency=12)

fit6 = hw(pass, seasonal="additive", h=12)
fit6$model

plot(fit6, ylab="passenger", xlab="year", lty=1, col="blue",
     main="그림 3-8 가법모형")
lines(fit6$fitted, col="red", lty=2)
legend("topleft", lty=1:2, col=c("blue", "red"), c("Pass", "Additive"))

ts.plot(resid(fit6), ylab="residual",
        main="그림 3-10 가법모형의 예측 오차")
abline(h=0)

acf(resid(fit6), main="Residual ACF")
t.test(resid(fit6), mu=0)

# holt winters multiplication model
fit7 = hw(pass, seasonal="multiplicative", h=12)
fit7$model

plot(fit7$fitted, ylab="passenger", xlab="year", lty=1, col="blue",
     main="그림 3-9 승법모형")
lines(fit7$fitted, col="red", lty=2)
legend("topleft", lty=1:2, col=c("blue", "red"), c("Pass", "Multiplicative"))

ts.plot(resid(fit7), ylab="residual",
        main="Figure 3-11 승법모형의 예측오차")
abline(h=0)

acf(resid(fit7), main="Residual ACF")
t.test(resid(fit7), mu=0)
