rm(list=ls())
setwd("Workspace/2022-Fall_TimeSeriesAnalysis/")

# Figure 1-1
set.seed(1245)
n = 100
z = 5000 + 20 * rnorm(n)
z_ts = ts(z, start=c(1980, 1), frequency=12)
ts.plot(z_ts, xlab="date", ylab="Zt", main="Figure 1-1 Irregular component")
abline(h=5000)

# Figure 1-2
set.seed(1234)
n = 100
t = 1:n
x = 0.5 * t
z = 0.5 * t + rnorm(n)
z_ts = ts(z,  start=c(1980, 1), frequency=12)
x_ts = ts(x, start=c(1980, 1), frequency=12)
ts.plot(z_ts, x_ts, col=c("blue", "red"), lty=1:2, 
        xlab="date", ylab="Zt", main="Figure 1-2 Trend component")

# Figure 1-3
n = 120
t = 1:n
a = rnorm(n, 0.1)
z = 10 + 3 * sin((2 * pi * t) / 12 + 0.8 * a)
z_ts = ts(z, start=c(1985, 1), frequency=12)
plot(z_ts, xlab="date", ylab="Zt", main="Figure 1-3 Seasonal component")

# Figure 1-4
z = scan("data/depart.txt")
lz = log(z)
t = 1:length(z)
x = 6.3 + 0.012 * t
fig = data.frame(lz, x)
z_ts = ts(fig, start=c(1984, 1), frequency=12)
ts.plot(z_ts, lty=1:2, xlab="date", ylab="logZ", 
        main="Figure 1-4 Trend component and seasonal component")

# Figure 1-5
z = scan("data/koreapass.txt")
z_ts = ts(z, start=c(1981, 1), frequency=12)
ts.plot(z_ts, xlab="date", ylab="Zt", 
        main="Figure 1-5 Time series with varying variance")

# Figure 1-6
set.seed(4321)
n = 120
t = 1:n
a = rnorm(n)
x = 3 * (t - 46)
x[t <= 60] = 0.5 * t[t <= 60]
z = x + a
z_ts = ts(z, frequency=12, start=c(1985, 1))
ts.plot(z_ts, xlab="date", ylab="Zt",
        main="Figure 1-6 Time series with varying trend")
abline(v=1990)

# Ex 1-5
n = 100
t = 1:n
z1 = 100 + rnorm(n)
z2 = 500 + rnorm(n)
z3 = 100 + 100 * rnorm(n)
z4 = 100 + t * rnorm(n)

fig = data.frame(z1, z2, z3, z4)
ts.plot(fig, lty=1:4, xlab="time", ylab="Z")

print(paste("Mean:", round(mean(z1), 4), "Var:", round(var(z1), 4)))
print(paste("Mean:", round(mean(z2), 4), "Var:", round(var(z2), 4)))
print(paste("Mean:", round(mean(z3), 4), "Var:", round(var(z3), 4)))
print(paste("Mean:", round(mean(z4), 4), "Var:", round(var(z4), 4)))

# Ex 1-6
n = 100
t = 1:n

z1 = 100 + rnorm(n)
z1_ts = ts(z1, frequency=12, start=c(1985, 1))
ts.plot(z1_ts, xlab="date", ylab="Zt")
abline(h=mean(z1), lty=2)

z2 = 100 + t + rnorm(n)
z2_ts = ts(z2, frequency=12, start=c(1985, 1))
ts.plot(z2_ts, xlab="date", ylab="Zt")
abline(h=mean(z2), lty=2)

z3 = 100 + t + 2 * t^2 + rnorm(n)
z3_ts = ts(z3, frequency=12, start=c(1985, 1))
ts.plot(z3_ts, xlab="date", ylab="Zt")
abline(h=mean(z3), lty=2)

z4 = 100 + sin((2 * pi * t) / 12) + cos((2 * pi * t) / 12) + rnorm(n)
z4_ts = ts(z4, frequency=12, start=c(1985, 1))
ts.plot(z4_ts, xlab="date", ylab="Zt")
abline(h=mean(z4), lty=2)

z5 = 100 + sin((2 * pi * t) / 4) + cos((2 * pi * t) / 4) + rnorm(n)
z5_ts = ts(z5, frequency=12, start=c(1985, 1))
ts.plot(z5_ts, xlab="date", ylab="Zt")
abline(h=mean(z5), lty=2)

z6 = 100 + 0.3 * t + sin((2 * pi * t) / 12) + cos((2 * pi * t) / 12) + rnorm(n)
z6_ts = ts(z6, frequency=12, start=c(1985, 1))
ts.plot(z6_ts, xlab="date", ylab="Zt")
abline(h=mean(z6), lty=2)

z7 = 100 + sin((2 * pi * t) / 12) + cos((2 * pi * t) / 12) + 0.8 * sin((2 * pi * t) / 6) + 0.7 * cos((2 * pi * t) / 6) + rnorm(n)
z7_ts = ts(z7, frequency=12, start=c(1985, 1))
ts.plot(z7_ts, xlab="date", ylab="Zt")
abline(h=mean(z7), lty=2)

# Ex 1-7
z1 = scan("data/female.txt")
z1_ts = ts(z1, start=c(1981, 1), frequency=12)
ts.plot(z1_ts, xlab="date", ylab="Zt")

z2 = scan("data/build.txt")
z2_ts = ts(z2, start=c(1981, 1), frequency=12)
ts.plot(z2_ts, xlab="date", ylab="Zt")

z3 = scan("data/export.txt")
z3_ts = ts(z3, start=c(1981, 1), frequency=12)
ts.plot(z3_ts, xlab="date", ylab="Zt")

z4 = scan("data/usapass.txt")
z4_ts = ts(z4, start=c(1981, 1), frequency=12)
ts.plot(z4_ts, xlab="date", ylab="Zt")
