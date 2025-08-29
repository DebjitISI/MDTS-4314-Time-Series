rm(list=ls())
set.seed(1234)
w=rnorm(1000,0,0.1)
w
plot(w,type="l")
xt=array(dim=1)
xt[1]=0+w[1]
for(i in 2:length(w)){
  xt[i]=0.8*xt[i-1]+w[i]
}
plot(xt,type="l")
acf(xt,lag.max=150)
pacf(xt)
#uses of filter function
x=filter(w,filter=0.8,method="recursive")
x
acf(x)
pacf(x)
w1=rnorm(10000)
x1=filter(w1,filter=0.8,method="recursive")
x1
acf(x1)
pacf(x1)
#
#uses of filter function
x=filter(w,filter=-0.8,method="recursive")
x
acf(x)
pacf(x)
w1=rnorm(10000)
x1=filter(w1,filter=-0.8,method="recursive")
x1
acf(x1)
pacf(x1)

x[1]
w[1]