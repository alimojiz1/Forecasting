install.packages("tseries")
library(tseries)
x<-ts(AirPassengers)
plot(x)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)
acf(log(AirPassengers))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))


plot(decompose(AirPassengers))
acf<-acf(AirPassengers,lag.max=20)
pacf<-pacf(AirPassengers,lag.max=20)
install.packages("forecast")
library(forecast)
fit_2<-auto.arima(AirPassengers)
fit_2
summary(fit_2)
tsdiag(fit_2)
fc<-forecast.Arima(fit_2,h=24)
plot.forecast(fc)
?forecast.Arima

fit_3 <- arima(AirPassengers, c(1, 0, 1))
fit_3
fit_4 <- arima(AirPassengers, c(1, 1, 1))
fit_4
fit_5 <- arima(AirPassengers, c(0, 1, 1))
fit_5
?arma
fit_6<-arma(AirPassengers,order=c(1,0))
summary(fit_6)
plot(fit_6)
fit_7<-ar(AirPassengers)
summary(fit_7)
plot(fit_7)




