rm(list=ls())

library(astsa)
set.seed(1234)

z = arima.sim(n=100, model=list(order=c(1, 0, 0), ar=0.5), rand.gen=rnorm)
y = arima.sim(n=100, model=list(order=c(1, 0, 0), ar=-0.5), rand.gen=rnorm)

# 그림 6-1
ts.plot(z, ylab="Z(t)", main=(expression(AR(1)~~phi==0.5)))
abline(h=0)

# 그림 6-2
ts.plot(y, ylab="Z(t)", main=(expression(AR(1)~~phi==-0.5)))
abline(h=0)

# 그림 6-6
set.seed(12347)
z = arima.sim(n=100, list(order=c(0, 0, 1), ma=-0.6), rand.gen=rnorm)
y = arima.sim(n=100, list(order=c(0, 0, 1), ma=0.6), rand.gen=rnorm)

ts.plot(z, yalb="z", main=(expression(MA(1)~~theta==0.6)))
abline(h=0)

# 그림 6-7
ts.plot(y, yalb="z", main=(expression(MA(1)~~theta==-0.6)))
abline(h=0)
