rm(list=ls())
setwd("~/Workspace/2022-Fall_TimeSeriesAnalysis/data")

library(astsa)
library(portes)
library(lmtest)

par(family="AppleGothic")

# 8.6
gas = scan("gas.txt", what=list(0, 0))
gas = data.frame(gas)
names(gas) = c("rate", "co2")

time = 1:nrow(gas)
rate = ts(gas$rate)
co2 = ts(gas$co2)
ts.plot(rate, ylab="gas rate", main="그림 8-1 가스 공급 비율")
acf2(rate, max.lag=24, main="그림 8-1 가스 공급 비율")

fit1 = arima(rate, order=c(3, 0, 0))
summary(fit1)
coeftest(fit1)

fit2 = arima(rate, order=c(3, 0, 0), include.mean=FALSE)
summary(fit2)

ts.plot(resid(fit2), main="그림 8-3 잔차 시계열 그림")
abline(h=0)
acf2(resid(fit2), max.lag=24, main="그림 8-4 잔차 SACF와 SPACF")
qqnorm(resid(fit2), main="그림 8-5 잔차의 정규성 검정")
qqline(resid(fit2), col="red")
# LjungBox(fit2, lags=seq(6, 24, 6))
sarima(rate, 3, 0, 0)
sarima.for(rate, 12, 3, 0, 0)

# 8.7
z = scan("eg8_7.txt")
z.ts = ts(z)
ts.plot(z.ts, ylab="z", main="그림 8-6 모의 실험 자료")
acf2(z.ts, max.lag=24, main="그림 8-6 ACF & PACF")
# LjungBox(z,ts, order=c(1, 0, 0))
fit = arima(z.ts, order=c(1, 0, 0))
summary(fit)
coeftest(fit)

ts.plot(resid(fit), main="그림 8-7 잔차", ylab="residual")
abline(h=0)

acf2(resid(fit), maxlen=24, main="그림 8-8 잔차 SACF와 SPACF")
qqnorm(resid(fit), main="그림 8-9 잔차의 정규성 검정")
qqline(resid(fit), col="red")
# LjungBox(z,ts, lag=seq(6, 24, 6))
sarima.for(z.ts, 25, 1, 0, 0)
sarima(z.ts, 2, 0, 0)
sarima(z.ts, 1, 0, 1)

# 8.8
library(fUnitRoots)
z = scan("elecstock.txt")
stock = ts(z)
ts.plot(stock, ylab="stock", main="그림 8-10 주가 지수의 시계열 그림")
acf2(stock, max.lag=24, main="그림 8-10 주가 지수의 ACF & PACF")
# LjungBox(stock, lags=seq(6, 24, 6))
adfTest(stock, lags=0, type="c")
adfTest(stock, lags=1, type="c")
adfTest(stock, lags=2, type="c")

dstock = diff(stock, lag=1)
ts.plot(dstock, ylab="diff(stock)", main="그림 8-11 차분된 주가 지수")
abline(h=0)
acf2(dstock, maxlag=24)
# LjungBox(dstock, lags=seq(6, 24, 6))
fit = arima(stock, order=c(1, 0, 0), method="CSS")
fit

acf2(resid(fit))
fit1 = arima(stock, order=c(0, 1, 0))
summary(fit1)
acf2(resid(fit1))

# 8.9
library(ggplot2)
library(lubridate)

z = sacn("female.txt")
female = ts(z)
ts.plot(female, ylab="female", main="그림 8-9 여성 근로자")
date = ymd("821201") + months(1:length(female) - 1)
female_df = data.frame(female, date)
acf2(female, max.lag=24, main="그림 8-9 여성 근로자의 ACF & PACF")

# LjungBox(female, lags=seq(6, 24, 6))
adfTest(female, lags=0, type="ct")
adfTest(female, lags=1, type="ct")
adfTest(female, lags=2, type="ct")

fit1 = lm(female ~ date, date=df)
coefficients(fit1)
residual = residuals(fit1)
resdf = data.frame(date, residual)
ggplot(aes(x=date, y=residual), data=resdf) + geom_line()
dfemale = diff(female, lag=1)

ts.plot(dfemale, ylab="dff(female)", main="그림 8-10 차분된 여성 근로자")
abline(h=0)

acf2(dfemale, maxlag=24, main="그림 8-10 ACF & PACF")
fit2 = arima(female, order=c(0, 1, 0), method="ML")
fit2
# LjungBox(fit2, lags=seq(6, 24, 6))
