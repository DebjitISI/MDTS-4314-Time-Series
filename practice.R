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



co2
plot(co2)
library(forecast)
ma = ma(co2,order=12);ma
detrnd=co2-ma
plot(detrnd)
detrnd
detrnd[is.na(detrnd)]<-0
dt = matrix(detrnd,ncol=12,byrow=T)
unadj_s = colMeans(dt)
adj_s = unadj_s -mean(unadj_s)
plot(adj_s)


x=aggregate(co2,nfrequency = 1,FUN=mean);x
plot(x)
t=1:39
fit=lm(x~t)
t1=1959:1997
lines(t1,fitted(fit),type="l",col="red")
summary=summary(fit)
summary$coefficients[1]

#method1
t2=(6:473 +0.5)/12
fit2=lm(co2~t2)
t3=1959:1997
plot(t2,co2,type = 'l')
lines(t2,fitted(fit2),type="l",col="red")
length(co2)
length(t2)
summary(fit2)

#method2(Preferred)
month_tr=function(t){
  summary$coefficients[1] + summary$coefficients[2]*(t+0.5)/12
}
plot(1:468,ma,type='l',xlab = "months")
lines(1:468,month_tr(6:473),type='l',col='red')
length(ma)



m=matrix(c(16,-4,-4,10),ncol=2, byrow=T)
solve(m)
