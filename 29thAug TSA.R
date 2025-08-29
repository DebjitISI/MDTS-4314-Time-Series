rm(list = ls())
library(astsa)
data = cardox
plot(data)
# here we see that the plot has both trend ans seasonal component with absence of increasing variability
index = floor(length(data)  * 0.8)
train_data = data[1:index]
test_data = data[(index + 1):length(data)]

plot(train_data, type = 'l', main = 'Train Data of Cardox Data')
acf(train_data, lag.max = 100)
pacf(train_data, lag.max = 100)

# trying to figure out the difference d in nonseasonal part
diff_data = diff(train_data, differences = 1)
plot(diff_data, type = 'l')
acf(diff_data, lag.max = 100)
pacf(diff_data, lag.max = 100)

# we see that the spikes are decaying so non stationary also the sig. spike are at lag 12 factors so its seasoonal

# we now stationarize/deseasonalize the detrended series, by doing this if not we keep increasing the beloow differences value: 
st_diff_data = diff(diff_data, lag = 12)
plot(st_diff_data, type = 'l')
acf = acf(st_diff_data, lag.max = 100)
pacf(st_diff_data, lag.max = 100)
acf = acf(st_diff_data, lag.max = 100)

Q = 0:1#sma
P = 0:4#sar
D = 1 #during deseasonalizing
q = 0:1#ma
p = 0:3#ar
d = 1#cduring detrending

#model = sarima(train_data,p = 1, d = 1, q = 1, P = 0, Q = 1, S = 12)
model = arima(train_data, 
      order = c(1, 1, 1),
      seasonal = list(order = c(0, 1, 1), period = 12),
      optim.control = list(maxit =1500),
      method = 'ML')

library(forecast)
forecast::checkresiduals(model,lag = 20) # we get p-value >0.05 so we accept the H0 and we can say its independent so its agood fit
forecast = forecast(model,h=length(test_data))
#plot(forecast1)
point_forecast = as.numeric(forecast$mean)
plot(point_forecast, ylim = c(380,420),type = 'l',main = "SARIMA")
lines(test_data,col='red')

model$aic

rmse = sqrt(mean((test_data - point_forecast)^2))
rmse

#TIKRAM
data_tsa = ts(train_data, start = c(1958,3), frequency = 12)
auto.arima(data_tsa, method = 'ML')
