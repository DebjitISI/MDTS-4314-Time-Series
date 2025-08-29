plot(co2)#as seasonal fluctuation is constant so the factors trend and seasonal are dependent
#model of co2 is a additive model :T_t+S_t+I_t
plot(AirPassengers)# as seasonal fluctuation is intensified so the trend and seasonal are inter dependent
#model of airpassengers is a multiplicative model :T_t*S_t*I_t
plot(JohnsonJohnson)
plot(lynx)
#model of lynx: C_t+I_t
#For CO2 dataset Obtain the yealy averages and plot them against the years
data=co2;data   
View(data)
x=aggregate(co2,nfrequency = 1,FUN=mean);x
plot(x)
t=1:39
fit=lm(x~t);fit
t1=1959:1997
lines(t1,fitted(fit),type="l",col="red")
summary(fit)

y=aggregate(AirPassengers,nfrequency = 1,FUN = mean);y
plot(y)
t2=1:12
f=lm(y~t2);f
t3=1949:1960
lines(t3,fitted(f),type="l",col="red")
summary(f)

z=aggregate(JohnsonJohnson,nfrequency = 1,FUN=mean);z
plot(z)
t4=1:21
f1=lm(z~t4);f1
t5=1960:1980
lines(t5,fitted(f1),type="l",col="red")
f1
summary(f1)
      




ts(c(12,17,20,32,23,13,31),
   frequency = 12,
   start=c(2003,11),
   end=c(2004,5))          



#ex1
set.seed(42)
X=rnorm(48,2,3);X
ts1=ts(X,frequency = 1,start=1973);ts1
ts2=ts(X,frequency = 4,start=c(2002,4));ts2
ts3=ts(X,deltat = 1/12,start=c(1984,11));ts3

frequency(ts1)
frequency(ts2)
frequency(ts3)
cycle(ts2)
plot(ts1)
plot(ts2)
plot(ts3)




#ex2

XMAT=matrix(X,byrow = F,ncol=6);XMAT
ts4=ts(XMAT,frequency = 4,start=c(2022,3),names = c("Delhi","Mumbai","kolkata","Hyderabd","Agra","Chennai"));ts4
plot(ts4)
plot.ts(ts4,plot.type="single",col=1:6)


#15/07/2025 MOVING AVERAGE-which are the trend estimates
library(forecast)
par(mfrow=c(2,3))
sm=ma(co2,order = 12)
plot(co2)
#plot(sm)
lines(sm,col="red")
sm1=ma(JohnsonJohnson,order = 4)
plot(JohnsonJohnson)
#plot(sm1)
lines(sm1,col="red")
sm2=ma(AirPassengers,order = 12)
plot(AirPassengers)
#plot(sm2)
lines(sm2,col="red")

#DETRENDING-Eliminating the trend component from the model

co2_x = co2-sm#additive mode
plot(co2_x)

John_x = JohnsonJohnson/sm1#multiplicative model
plot(John_x)
air_x = AirPassengers/sm2#multiplicative model
plot(air_x)

plot(tapply(na.omit(co2_x),cycle(na.omit(co2_x)),mean))
co2_x[is.na(co2_x)]<-0
co=matrix(co2_x,ncol=12,byrow=T)

unadj_s_index=colMeans(co)
unadj_s_index
adj_s_index = unadj_s_index -mean(unadj_s_index)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
plot(adj_s_index,type='l')
plot(co2_x)

#
co2# here
avg=aggregate(co2,nfrequency = 1,FUN = mean)
t=6:473
fit=lm(avg~t)
plot(avg,type='l')
lines(t,fitted(fit),col='red',type = 'l')
