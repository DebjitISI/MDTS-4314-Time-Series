library(astsa)
?sarima.sim()
sim = sarima.sim(n=50000,sar = 0.9,S = 12)
plot(sim,type ='l')
acf(sim,lag.max=100)
vpacf(sim,lag.max=100)

#ARMA(0,1)X(1,0)_12
a = sarima.sim(n=50000,ma = 0.9, sar = 0.9,S = 12)
acf(a,lag.max=100)
pacf(a,lag.max=100)

data = diff(AirPassengers,differences = 1)
acf(data,lag.max=100)
pacf(data,lag.max=100)
plot(data)
#plot(log(AirPassengers))


