rm(list=ls())

setwd("Workspace/2022-Fall_TimeSeriesAnalysis/")
par(family="AppleGothic")

# Figure 7-1
z = scan("data/depart.txt")
dept = ts(z, start=c(1984, 1), frequency=12)
ldept=log(dept)
dif_1 = diff(ldept, lag=1)
dif_12 = diff(ldept, lag=12)
dif_112 = diff(dif_1, lag=12)

ts.plot(dept, ylab="depart", main="그림 7-1 백화점 월별 매출액")

ts.plot(ldept, ylab="ln depart", main="그림 7-2 로그 매출액")

ts.plot(dif_1, ylab="diff 1", main="그림 7-5 1차 차분된 로그 매출액")
abline(h=0)

ts.plot(dif_12, ylab="diff 12", main="계절 차분된 로그 매출액")

ts.plot(dif_112, ylab="diff 1 & 12", main="그림 7-6 계절 차분된 로그 매출액")
abline(h=0)

# Figure 7-3
z = scan("data/interest.txt")
interest = ts(z, start=c(1982, 4), frequency=12)
ts.plot(interest, ylab="interest", main="그림 7-3 이자율")
abline(v=1992)

# Figure 7-4
set.seed(123456)
z = ts(cumsum(rnorm(100,.01, 1)))
difz = diff(z, lag=1)
par(mfrow=c(1, 2))

ts.plot(z, ylab="z", main="Random walk")
ts.plot(difz, ylab="diff z", main="1차 차분된 Rnadom walk")

# Figure 7.7~7.10
set.seed(16732)
par(mfrow=c(1, 2))
z = arima.sim(n=300, list(order=c(1,1,1), ar=0.8, ma=-0.5), rand.gen=rnorm)

# Figure 7.7
ts.plot(z, ylab="z", main="그림 7-7 ARMA(1, 1, 1) 과정의 시계열 그림")
acf(z, maxlag=24, main="그림 7-7 ARMA(1, 1, 1) 과정의 ACF")

# Figure 7.7
diff_z = diff(z, lag=1)
ts.plot(diff_z, ylab="z", main="그림 7-8 ARMA(1, 1, 1) 과정의 1차 차분한 후의 시계열 그림")
acf(diff_z, maxlag=24, main="그림 7-8 ARMA(1, 1, 1) 과정의 1차 차분한 후의 ACF")


# or 
t = 1:300
z = rep(0, 302)
a1 = rnorm(1)
for (i in 1:300){
  a = rnorm(1)
  z[i+2] = 1.8 * z[i+1] - 0.8 * z[i] + a - 0.5 * a1
  a1 = a
}
z = z[3:302]
ts.plot(z, ylab="z", main=expression(ARIMA(1,1,1)~~~~~phi==0.8~~theta==0.5))


# Exercise
library(fUnitRoots)
par(mfrow=c(1, 1))
stationary_z_1 = arima.sim(n=100, list(order=c(1,0,0), ar=0.8))
ts.plot(stationary_z_1, main="정상 시계열 1, phi 0.8")
adfTest(stationary_z_1)

stationary_z_2 = arima.sim(n=100, list(order=c(2,0,0), ar=c(0.2, 0.2)))
ts.plot(stationary_z_2, main="정상 시계열 2, phi 0.2, 0.2")
adfTest(stationary_z_2)

nonstationary_z_1 = arima.sim(model=list(order=c(0, 1, 0)), n=100)
ts.plot(nonstationary_z_1, main="비정상 시계열 1, random walk")
adfTest(nonstationary_z_1)

nonstationary_z_2 = arima.sim(model=list(order=c(0, 1, 0)), n=100, mean=1,sd=5)
ts.plot(nonstationary_z_2, main="비정상 시계열 2, random walk with intercept")
adfTest(nonstationary_z_2)


