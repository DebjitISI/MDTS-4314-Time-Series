library(astsa)
cardox
index = c(1:floor(0.7*length(cardox)))
train_data = cardox[index]
test_data = cardox[-index]
plot(train_data,type = 'l')
plot(diff(train_data,differences = 3),type = 'l')
adf.test(diff(train_data))
# its stationary


acf((train_data),lag.max = 100)
pacf((train_data),lag.max = 100)
acf(diff(train_data,differences = 3),lag.max = 100)
pacf(diff(train_data,differences = 3),lag.max = 100)
p = 12
q = c(0,1)
?arima
m1 = arima(diff(train_data,differences = 2),order = c(12,0,0),include.mean = T,method = 'ML', optim.control = list(maxit = 1500))
m2 = arima(diff(train_data,differences = 2),order = c(12,0,1),include.mean = T,method = 'ML', optim.control = list(maxit = 1500))
m3 = arima(diff(train_data,differences = 2),order = c(12,0,0),include.mean = T,method = 'ML', optim.control = list(maxit = 1500))
df = data.frame(c(m1$aic,m2$aic,m3$aic),row.names = c('arima(0,1,0)','arima(1,1,0)','arima(2,1,0)'))
colnames(df) = 'aic Score'
df
abs(m1$aic - m2$aic)
abs(m1$aic - m3$aic)
abs(m2$aic - m3$aic)

par(mfrow = c(1,2))
acf(m2$residuals)
acf(m3$residuals)


library(forecast)
?forecast()

m1 = arima(train_data,order = c(12,2,1),method = 'ML', optim.control = list(maxit = 1500))
forecast1 = forecast(m1,h=length(test_data))
plot(forecast1)
point_forecast = as.numeric(forecast1$mean)
plot(point_forecast,type = 'l',ylim = c(360,420))
lines(test_data,col='red')
acf(m1$residuals)
?checkresiduals
a = checkresiduals(m1)

rmse_1 = sqrt(mean((point_forecast-test_data)^2))
cardox
#df = data.frame(matrix(c(0,0,cardox,0,0,0,0,0,0,0,0,0),ncol = 12,byrow = T),row.names =1958:2023)
#df
#colnames(df) = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
#df
#write.csv(df,file = "C:\\Users\\DS-31\\Desktop\\Practice_438\\438\\Time Series\\cardox_data.csv")
Box.test(m1$residuals,lag = 20,type = 'Ljung-Box',fitdf = 13)
#Here lag has to be a large cutoff s.t it is greater tham p+q and here fitdf is p+q
