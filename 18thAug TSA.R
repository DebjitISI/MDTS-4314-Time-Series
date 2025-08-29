plot(AirPassengers)
plot(log(AirPassengers))
data = log(AirPassengers)
plot(diff(data, order = 1),type = 'l')
acf(diff(data, order = 1),lag.max = 500)
acf(data,lag.max = 50)

library(astsa)
par(mfrow = c(2,2))
plot(log(gnp))
acf(log(gnp),lag.max = 50)
plot(diff(log(gnp), order = 1),type = 'l')
acf(diff(log(gnp), order = 1),lag.max = 500)
pacf(diff(log(gnp), order = 1),lag.max = 500)
adf.test
gnp
library(tseries)
?adf.test()
acf(diff(log(AirPassengers)),lag.max = 100)
write.csv()

adf.test(diff(log(gnp), order = 1))


?arima
m1 = arima(log(gnp),order = c(0,1,1))
m2 = arima(log(gnp),order = c(0,1,2))
m3 = arima(log(gnp),order = c(1,1,0))
m4 = arima(log(gnp),order = c(1,1,1))
df = data.frame(c(m1$aic,m2$aic,m3$aic,m4$aic),row.names = c('arima(0,1,1)','arima(0,1,2)','arima(1,1,0)','arima(1,1,1)'))
colnames(df) = 'aic Score'
df
abs(m1$aic - m2$aic)
abs(m1$aic - m3$aic)
abs(m1$aic - m4$aic)
abs(m2$aic - m3$aic)
abs(m2$aic - m4$aic)
abs(m3$aic - m4$aic)

#so we take m3 and m4 as their aics are close
par(mfrow = c(1,2))
acf(m3$residuals)
acf(m4$residuals)

# from the acf plots we see that the m4 and m3 have the single signif. spike so it is random and has no correlation but we select m3 as it has the least parameters(persimunious)
