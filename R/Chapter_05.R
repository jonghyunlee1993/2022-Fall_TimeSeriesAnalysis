rm(list=ls())

library(ggplot2)

t = 1:100
Z_1 = c()
z_1_n_1 = 0

Z_2 = c()
z_2_n_1 = 0

for (x in t){
  z_1_n = -0.7 * z_1_n_1 + rnorm(1)
  z_2_n = 0.5 * z_2_n_1 + rnorm(1)
  
  Z_1 = c(Z_1, z_1_n)
  Z_2 = c(Z_2, z_2_n)
  
  z_1_n_1 = z_1_n
  z_2_n_1 = z_2_n
  
}

Z_1_ts = ts(Z_1, start=c(1980, 1), frequency=12)
Z_2_ts = ts(Z_2, start=c(1980, 1), frequency=12)

ts.plot(Z_1_ts)
ts.plot(Z_2_ts)

plot(Z_1[1:99], Z_1[2:100])
cor(Z_1[1:99], Z_1[2:100])
acf(Z_1)
pacf(Z_1)

plot(Z_2[1:99], Z_2[2:100])
cor(Z_2[1:99], Z_2[2:100])
acf(Z_2)
pacf(Z_2)

custom_SACF = function(Z, k) {
  N = length(Z)
  Z_mean = mean(Z)
  Z_var = sum((Z - Z_mean)^2) / N
  
  term_1 = Z[1:(N-k)] - Z_mean
  term_2 = Z[(1+k):N] - Z_mean
  u = sum(term_1 * term_2) / N
  
  return (u / Z_var)
}

draw_SACF = function(Z) {
  results = as.data.frame(
    matrix(nrow=20, ncol=2)
  )
  colnames(results) = c("lag", "acf")
  
  for (i in 1:20){
    results[i, 1] = i
    results[i, 2] = custom_SACF(Z, i)
  }
  
  ggplot(data=results, aes(x=lag, y=acf)) +
    geom_bar(stat='identity', width=0.2) +
    ggtitle("SACF plot")
}

lagmat = function(Z, k){
  lagmat = matrix(nrow=length(Z), ncol=k+1)
  lagmat[1:length(Z), 1] = Z
  
  for (i in 1:k){
    Z = lag(Z)
    lagmat[1:length(Z), i+1] = Z
  }
  
  return (lagmat)
}

lagmat(Z_1, 5)

custom_SPACF = function(Z, k) {
  lag_mat = lagmat(Z, k)
  X = lag_mat[(k+1):length(Z), 2:k]
  y_forward = lag_mat[(k+1):length(Z), 1]
  y_backward = lag_mat[(k+1):length(Z), (k+1)]
  model_forward = lm(y_forward ~ X)
  model_backward = lm(y_backward ~ X)
  a = as.numeric(model_forward$residuals)
  b = as.numeric(model_backward$residuals)
  
  return (cor(a, b))
}

draw_SPACF = function(Z) {
  results = as.data.frame(
    matrix(nrow=20, ncol=2)
  )
  colnames(results) = c("lag", "pacf")
  
  for (i in 1:20){
    results[i, 1] = i
    results[i, 2] = custom_SPACF(Z, i)
  }
  
  ggplot(data=results, aes(x=lag, y=pacf)) +
    geom_bar(stat='identity', width=0.2) +
    ggtitle("SPACF plot")
}

draw_SACF(Z_1)
draw_SPACF(Z_1)

draw_SACF(Z_2)
draw_SPACF(Z_2)




