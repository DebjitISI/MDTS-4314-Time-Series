rm(list = ls())
library(astsa)
library(forecast)
library(tseries)
data = read.csv("C:\\Users\\DS-31\\Desktop\\Practice_438\\438\\Time Series\\data (2).csv")
plot(data[,'Sales'],type = 'l')

index = c(1:floor(0.8*dim(data)[1]))
train_data = data[index,]
test_data = data[-index,]
plot(train_data[,'Sales'],type = 'l')
plot(diff(train_data[,'Sales'],differences = 2),type = 'l')
adf.test(diff(train_data[,'Sales'],differences = 2))
#train_data

d=2
acf(diff(train_data[,'Sales'],differences = 2),lag.max = 100)
pacf(diff(train_data[,'Sales'],differences = 2),lag.max = 100)
par(mfrow = c(3,2))
for (p in c(0,1,2)) {
  for (q in c(0,1)) {
    m = arima(train_data[,'Sales'],order = c(p,2,q),method = 'ML', optim.control = list(maxit = 1500))
    print(paste("AIC of Arima for p=",p,',d=',2,"and q=",q,":",m$aic))
    forecast = forecast(m,h=length(test_data[,'Sales']))
    #plot(forecast1)
    point_forecast = as.numeric(forecast$mean)
    plot(point_forecast,type = 'l',ylim = c(5000,8000),main = paste("Arima for p=",p,',d=',2,"and q=",q))
    lines(test_data[,'Sales'],col='red')
    #acf(m1$residuals)
    rmse = sqrt(mean((point_forecast-test_data[,'Sales'])^2))
    print(paste("RMSE of Arima for p=",p,',d=',2,"and q=",q,":",rmse))
    #a = checkresiduals(m1)
    point_forecast = 0
  }
  
}


data1 = log(gnp)
data1
plot(data1,type = 'l',ylim = c(6,10))
gnp
index = c(1:floor(0.8*length(data1)))
train_data1 = data1[index]
test_data1 = data1[-index]
plot(train_data1,type = 'l')
plot(diff(train_data1,differences = 1),type = 'l')
adf.test(diff(train_data1,differences = 1))

#train_data
acf(diff(train_data1,differences = 1))
pacf(diff(train_data1,differences = 1))
p = c(0,1)
q = c(0,1,2)