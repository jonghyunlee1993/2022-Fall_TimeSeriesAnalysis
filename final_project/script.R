rm(list=ls())

setwd("~/Workspace/2022-Fall_TimeSeriesAnalysis/final_project/")
par(family="AppleGothic")
library(astsa)
library(tidyverse)
library(tseries)
library(forecast)
library(tsoutliers)

data = read.csv("weather_data.csv")

seoul_df = data %>% filter(region == 0) %>% select(-c(region))
jeju_df = data %>% filter(region == 1) %>% select(-c(region))
seogwipo_df = data %>% filter(region == 2) %>% select(-c(region))

# 기온
seoul_temp_mean = ts(seoul_df$temp_mean, start=c(1995, 1), frequency=12)
seo_temp_mean = ts(seogwipo_df$temp_mean, start=c(1995, 1), frequency=12)

ts.plot(seoul_temp_mean, seo_temp_mean,
        main="서울과 제주의 평균 기온 시계열 그림",
        gpars=list(col=c("black", "red")))
legend("bottomright", legend=c("서울", "서귀포"), col=1:2, lty=1)

mean(seoul_temp_mean); sd(seoul_temp_mean)
mean(seo_temp_mean); sd(seo_temp_mean)

cor(seoul_df$temp_mean, seogwipo_df$temp_mean)

diff_12_seoul_temp = diff(seoul_temp_mean, lag=12)
diff_12_seo_temp = diff(seo_temp_mean, lag=12)

ts.plot(diff_12_seoul_temp, diff_12_seo_temp,
        main="서울과 제주의 평균 기온 계절 차분 후 시계열 그림",
        gpars=list(col=c("black", "red")))
legend("bottomright", legend=c("서울", "제주"), col=1:2, lty=1)

adf.test(diff_12_seoul_temp, k=0)
adf.test(diff_12_seo_temp, k=0)

acf2(diff_12_seoul_temp, main="계절 차분된 서울 평균 기온")
acf2(diff_12_seo_temp, main="계절 차분된 제주 평균 기온")

seoul_model = arima(seoul_temp_mean, order=c(2, 0, 0), 
                    seasonal=list(order=c(0, 1, 1), period=12))
seoul_model_resid = resid(seoul_model)
ts.plot(seoul_model_resid, main="서울 평균 기온의 잔차")

Box.test(seoul_model_resid, lag=6)
Box.test(seoul_model_resid, lag=12)
Box.test(seoul_model_resid, lag=24)
acf2(seoul_model_resid)

seoul_model_2 = arima(seoul_temp_mean, order=c(2, 0, 0), 
                      seasonal=list(order=c(0, 1, 1), period=12))
seoul_model_2_resid = resid(seoul_model_2)
ts.plot(seoul_model_2_resid, main="서울 평균 기온의 잔차")
Box.test(seoul_model_2_resid, lag=6)
Box.test(seoul_model_2_resid, lag=12)
Box.test(seoul_model_2_resid, lag=24)
acf2(seoul_model_2_resid)

seo_model_2 = arima(seo_temp_mean, order=c(2, 0, 0), 
                      seasonal=list(order=c(0, 1, 1), period=12))
seo_model_2_resid = resid(seo_model_2)
ts.plot(seo_model_2_resid, main="서귀포 평균 기온의 잔차")
Box.test(seo_model_2_resid, lag=6)
Box.test(seo_model_2_resid, lag=12)
Box.test(seo_model_2_resid, lag=24)
acf2(seo_model_2_resid)

autoplot(forecast(seoul_model_2, h=60)) +
  labs(x="Time", y="평균 기온", title="서울의 평균 기온 예측") +
  theme(axis.title.y=element_text(family="AppleGothic"),
        plot.title=element_text(family="AppleGothic"),
        plot.subtitle=element_text(family="AppleGothic")) 

autoplot(forecast(seo_model_2, h=60)) +
  labs(x="Time", y="평균 기온", title="서귀포의 평균 기온 예측") +
  theme(axis.title.y=element_text(family="AppleGothic"),
        plot.title=element_text(family="AppleGothic"),
        plot.subtitle=element_text(family="AppleGothic")) 

# 습도
seoul_hum_mean = ts(seoul_df$humidity, start=c(1995, 1), frequency=12)
seo_hum_mean = ts(seogwipo_df$humidity, start=c(1995, 1), frequency=12)

ts.plot(seoul_hum_mean, seo_hum_mean,
        main="서울과 제주의 평균 상대 습도 시계열 그림",
        gpars=list(col=c("black", "red")))
legend("bottomright", legend=c("서울", "서귀포"), col=1:2, lty=1)

mean(seoul_hum_mean); sd(seoul_hum_mean)
mean(seo_hum_mean); sd(seo_hum_mean)

diff_12_seo_hum = diff(seo_hum_mean, lag=12)
ts.plot(diff_12_seo_hum)
abline(h=0)
acf2(diff_12_seo_hum, main="계절 차분된 제주 평균 습도")

seo_model = arima(seo_hum_mean, order=c(0, 0, 0), 
                  seasonal=list(order=c(0, 1, 1), period=12))
seo_model_resid = resid(seo_model)
ts.plot(seo_model_resid, main="서귀포 평균 습도의 잔차")
acf2(seo_model_resid)

seo_model_2 = arima(seo_hum_mean, order=c(0, 1, 0), 
                  seasonal=list(order=c(0, 1, 1), period=12))
seo_model_2_resid = resid(seo_model_2)
ts.plot(seo_model_2_resid, main="서귀포 평균 습도의 잔차")
acf2(seo_model_2_resid)

seo_model_3 = arima(seo_hum_mean, order=c(0, 1, 1), 
                    seasonal=list(order=c(0, 1, 1), period=12))
seo_model_3_resid = resid(seo_model_3)
ts.plot(seo_model_3_resid, main="서귀포 평균 습도의 잔차")
acf2(seo_model_3_resid)

Box.test(seo_model_3_resid, lag=6)
Box.test(seo_model_3_resid, lag=12)
Box.test(seo_model_3_resid, lag=24)

autoplot(forecast(seo_model_3, h=60)) +
  labs(x="Time", y="평균 습도", title="서귀포의 평균 습도 예측") +
  theme(axis.title.y=element_text(family="AppleGothic"),
        plot.title=element_text(family="AppleGothic"),
        plot.subtitle=element_text(family="AppleGothic")) 

outlier = tso(ts(seo_hum_mean), types=c("AO", "IO", "LS"),
              tsmethod="arima", cval=3.0, 
              args.tsmethod=list(order=c(0, 1, 1), seasonal=list(order=c(0, 1, 1), period=12)))
outlier
