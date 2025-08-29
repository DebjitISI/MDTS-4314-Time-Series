rm(list=ls())

par(mfrow=c(1,2))
white_noise = rnorm(1000,0,200)
plot(white_noise,type='l',main='White Noise for variance 1')
??acf
cor = acf(white_noise,lag.max = 50)
cor
#Here we see that thhere are spikes for lag != 0 even though theortically there should not be because we are working on sampled data
#here the blue dotted lines signify if the spike is signifcant or not if it crosses the blue lines it is sifnificant
?plot.acf
plot(cor,ci=0.95)
#data("CO2")  # Built-in CO2 dataset in R
#write.csv(co2, "C:\\Users\\DS-31\\Desktop\\Practice_438\\438\\Time Series\\co2_data.csv", row.names = FALSE)

par(mfrow = c(2,2))
wt_noise = rnorm(1000)
sma = filter(wt_noise,filter = rep(1/3,3),method='convolution',sides=1)
acf(na.omit(sma),lag.max = 50)
pacf(na.omit(sma),lag.max = 50)
#AR2    
sma_ar2 = filter(wt_noise,filter = c(1,-0.9),method='recursive',init = rnorm(2))
acf(sma_ar2)
pacf(sma_ar2)
acf(AirPassengers,lag.max=100)


write.csv(AirPassengers, "C:\\Users\\DS-31\\Desktop\\Practice_438\\438\\Time Series\\AirPassengers_data.csv", row.names = FALSE)
write.csv(CO2,"C:\\Users\\DS-31\\Desktop\\Practice_438\\438\\Time Series\\AirPassengers_data.csv")

acf(AirPassengers)


