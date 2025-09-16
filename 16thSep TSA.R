library(astsa)
rm(list=ls())
plot(nyse)
data=astsa::nyse
data
plot(data)

#grachSpec: generate a custom model
#garchSim: generate n obs of data from the abve model


data = read.csv("C:/Users/DS-31/Desktop/Practice_438/438/Time Series/archdata.csv")
View(data)

plot(data$data,type = 'l')
acf(data$data,lag.max = 50)
pacf(data$data,lag.max = 50)
acf2(data$data,max.lag = 50)#in asta package


model = arima(data$data,order =c(1,0,0),include.mean = F)
forecast::checkresiduals(model,lag = 20)

res = model$residuals
plot(res)
plot(res^2)
library(FinTS)
ArchTest(res)
acf2(res^2,max.lag = 50)



library(fGarch)
model2 = garchFit(~arma(1,0)+garch(2,0),data$data,trace = F)
#...............~to fit the mean + to dit the volatility(i.e. variance)
summary(model2)# omega is alpha_0
model3 = garchFit(~arma(1,0)+garch(1,0),data$data,trace = F)
summary(model3)
