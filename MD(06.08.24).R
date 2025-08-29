rm(list=ls())
set.seed(1234)
w=rnorm(1000,0,0.1)
plot(w,type="l")
acf(w,lag.max=50)
y=array(dim=1)
for(i in 5:1000){
  y[i]=((w[i]+w[i-1]+w[i-2]+w[i-3]+w[i-4]))/5
}
plot(y,type="l",xlim=c(0,1000),ylim=c(-0.5,0.5))
acf(na.omit(y),log.max=150)
y
pacf(na.omit(y),log.max=150)
###(07.08.24)(AR(1))
xt=array(dim=1)
xt[1]=0+w[1]
for(i in 2:length(w)){
  xt[i]=0.9*xt[i-1]+w[i]
}
plot(xt,type="l")
pacf(xt)

####another method
x=filter(w,filter=0.9,method="recursive")
#######
acf(xt,lag.max=150)
#AR(2)
xt2=filter(w,filter=c(1,-0.9),method="recursive")
plot(xt2)
acf(xt2,lag.max=100)
pacf(xt2)
###
rm(list=ls())
set.seed(1234)
wn=rnorm(300,0,1)
del1=0
del2=0.2
del3=-0.6
r1=r2=r3=array(dim=1)
r1[1]=del1+wn[1]
r2[1]=del2+wn[1]
r3[1]=del3+wn[1]
for(i in 2:length(wn)){
  r1[i]=del1+r1[i-1]+wn[i]
  r2[i]=del2+r2[i-1]+wn[i]
  r3[i]=del3+r3[i-1]+wn[i]
}
plot(r1,type="l",ylim=c(-100,100))
lines(del1*(1:300),col="blue")
lines(r2,col="red",type="l")
lines(del2*(1:300),col="blue")
lines(r3,col="cyan")
lines(del3*(1:300),col="blue")
