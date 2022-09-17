rm(list = ls())
setwd("Workspace/2022-Fall_TimeSeriesAnalysis/")

## Example 2.1
library(lmtest)

z = scan("data/population.txt")
pop = round(z / 10000)
pop = ts(pop, start=c(1960))
t = 1:length(pop)
t2 = t * t
m1 = lm(pop ~ t)
dwtest(m1)
summary(m1)

ts.plot(pop, fitted(m1), xlab="year", ylab="population", lty=1:2,
        main="Figure 2-3 The total population and the predicted values")
acf(resid(m1), main="Residuals of ACF")

m2 = lm(pop ~ t + t2)
summary(m2)

ts.plot(pop, fitted(m2), xlab="year", ylab="population", lty=1:2, 
        main="Figure 2-5 A predcited values of second order trend model")
ts.plot(resid(m2), type="o", ylab="residual",
        main="Figure 2-6 Residuals of second order trend model")
abline(h=0)
legend("topleft", legend=c("pop", "fitted"), lty=1:2)
acf(resid(m2), main="Residuals of ACF")

lnpop = log(pop)
m3 = lm(lnpop ~ t + t2)
dwtest(m3)
summary(m3)

ts.plot(lnpop, fitted(m3), xlab="year", ylab="log population", lty=1:2,
        main="A predcited values of second order trend model with log transformation")
legend("topleft", legend=c("lnpop", "fitted"), lty=1:2)
ts.plot(resid(m3), ylab="residual", 
        main="Figure 2-7 Residuals of second order trend model with log transformation")
abline(h=0)
acf(resid(m3), main="Residual of ACF")


## Example 2.2
library(astsa)
z = scan("data/depart.txt")
dep = ts(z, frequency=12, start=c(1984, 1))
ts.plot(dep, ylab="depart", 
        main="Figure 2-9 Sales revenue of department store")

lndep = log(dep)
ts.plot(lndep, ylab="log depart", 
        main="Figure 2-10 Log transformed sales revenue of department store")
trend = time(lndep) - 1984
y = factor(cycle(lndep))
reg = lm(lndep ~ 0 + trend + y)
dwtest(reg)
summary(reg)
model.matrix(reg)
resid = ts(resid(reg), start=c(1984, 1), frequency=12)
ts.plot(resid(reg), ylab="residual", 
        main="Figure 2-11 Residuals")
abline(h=0)
acf2(resid(reg), main="Residuals of ACF & PCAF")

## Example 2.3
z = scan("data/catv.txt")
k = 70000000
t = 1:length(z)
year = t + 1969
catv = ts(z, start=c(1970))
lncatv = log(k / catv - 1)

ts.plot(catv, xlab="year", ylab="catv",
        main="Figure 2-13 Number of subscribers of Cable TV")
ts.plot(lncatv, xlab="year", ylab="catv",
        main="Figure 2-14 Number of subscribers (log transformed) of Cable TV")
fit = lm(lncatv ~ t)
summary(fit)
pred = k / (exp(fitted(fit)) + 1)
resid = catv - pred
y = data.frame(catv, pred)
fig = ts(y, start=c(1970))
ts.plot(fig, xlab="year", ylab="catv", lty=1:2,
        main="Figure 2-15 Predicted value and observed value")
legend("right", legend =c("Catv", "Predict"), lty=1:2)
ts.plot(resid, xlab="time", ylab="residual",
        main="Figure 2-16 Residuals")
abline(h=0)
acf2(resid, main="Residuals of ACF & PCAF")

## Example 2.4
dept = scan("data/depart.txt")
n = 1:length(dept)
time = ts(n, frequency=12, start=c(1984, 1))
dept.ts = ts(dept, frequency=12, start=c(1984, 1))
lndept = log(dept.ts)
y = factor(cycle(time))
fit = lm(lndept ~ 0 + time + y)
anova(fit)
summary(fit)

resid = ts(resid(fit), start=c(1984, 1), frequency=12)
acf2(resid, main="Residuals of ACF & PACF")
autoreg = arima(residuals(fit), order=c(3, 0, 0))
summary(autoreg)
plot(resid(autoreg), main="Figure 2-17 Residuals after fitted autoregresive model")
abline(h=0)

## Figure 2.8
n = 100
t = 1:n
a1 = -0.8
a2 = 1.4
phi1 = pi / 8
phi2 = 3 * pi/4
first = a1 * sin(pi * t / 6 + phi1)
second = a2 * sin(pi * t / 3 + phi2)
z = first + second
plot(z, type="o", lty=1, xlab="time", ylab="z",
     main="Figure 2-8 Time series with two cyclic components")
lines(first, lty=2, col="blue")
lines(second, lty=3, col="red")
legend("left", legend=c("series", "first", "second"), lty=1:3)

fig = data.frame(z, first, second)
ts.plot(fig, col=c("black", "blue", "red"), lty=1:3,
        xlab="time", ylab="z", 
        main="Figure 2-8 Time series with two cyclic components")
legend("right", legend=c("series", "first", "second"))

## Figure 2.12
# S-curve
b0 = 0.2
b1 = -12
t = 1:60
z1 = exp(b0 + b1 / t)

# gompertz
b0 = 10
b1 = 0.15
k = 1
z2 = k * exp(-b0 * exp(-b1 * t))

# von
b0 = 0.95
b1 = 0.09
z3 = (1 - b0 * exp((-b1) * t)^3)

# Pear 1
b0 = 5
b1 = -0.2
k = 1
z4 = k / (1 + exp(b0 + b1 * t))

z = data.frame(z1, z2, z3, z4)
z.ts = ts(z)
ts.plot(z.ts, lty=1:4, main="Figure 2-12 S-Cruves")
legend("right", legend=c("Exp", "Gompertz", "Von", "Pearl"), lty=1:4)










