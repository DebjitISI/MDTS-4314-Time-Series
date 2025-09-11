JohnsonJohnson
z=aggregate(JohnsonJohnson,nfrequency = 1,FUN=mean);z
plot(z)
t4=1:21
z
library(forecast)
library(astsa)
arma
ts(c(12,17,20,32,23,13,31),
   frequency = 5)
?filter
?decompose
m <- decompose(co2)
m$figure
plot(m)
m$trend

ma(co2,order = 12)
library(tseries)
adf.test()

decompose()
?filter
filter(co2,filter =c())
check
arima(dt, order = c())
?checkresiduals()
forecast(f,h=12)
forecast$mean
y=aggregate(AirPassengers,nfrequency = 1,FUN = mean);y
plot(y)
t2=1:12
f=lm(y~t2);f
t3=1949:1960
lines(t3,fitted(f),type="l",col="red")
summary(f)
#ex1
set.seed(42)
X=rnorm(48,2,3);X
ts1=ts(X,frequency = 12,start=c(1984,11));ts1
ts2=ts(X,frequency = 4,start=c(2002,4));ts2
ts3=ts(X,deltat = 1/12,start=c(1984,11));ts3
?ts
arima.sim(n=100,list(ar=c(0.8),ma=c(0.9)),rand.gen = rnorm)

ts_data <- ts(1:10, start = c(2000,1), frequency = 4)  # quarterly data
ts_data

# Convert to normal numeric vector
vec_data <- as.numeric(ts_data)
vec_data
