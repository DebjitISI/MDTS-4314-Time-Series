rm(list=ls())
set.seed(123)
library(forecast)
epsilon = rnorm(1004)
order =4
beta = 1/(order+1)
xt = array(dim=1000)
for (i in c(5:1004)){
  xt[i-4] = beta*(epsilon[i]+epsilon[i-1]+epsilon[i-2]+epsilon[i-3]+epsilon[i-4])
}

par(mfrow=c(2,2))
plot(epsilon,type='o')
plot(xt,type='o')
acf(epsilon,lag.max = 50)
acf(xt,lag.max = 50)

?filter
sma = filter(epsilon,rep(beta,5), method ="convolution",
       sides = 1, circular = FALSE, init)
sma = na.omit(sma)
acf(sma,lag.max = 50)
?ma

#if we are trying to find the time points using ma we take X_t = beta0*e_t + beta1*e_t-1 + beta2*e_t-2 + ..
#id we are trying to smooth the we take x_t = beta0*e_t-1 + beta1*e_t + beta2*e_t+1 +...

#Generate 10000 obs from a a simple moving process of order 3 and obtain the sample correlogram, hence comment
rm(list=ls())
set.seed(123)
epsilon_t = rnorm(10003)
order = 3
beta = 1/4

par(mfrow=c(2,2))
sma_t = filter(epsilon_t,rep(1/4,4),method='convolution',sides=1)
sma_t = na.omit(sma_t)
acf(sma_t,lag.max = 50)
cor = acf(sma_t,lag.max = 50)
plot(cor,main='Correlogram-ACF')
plot(cor,type='l',main='Correlogram-ACF')
#We see a linear drop from h=0 to h=3 then we see irregular oscillation around acf=0 for h>3
pacf(sma_t,lag.max = 50)
pcor = pacf(sma_t,lag.max = 50)
plot(pcor,main='Correlogram-PACF')
plot(pcor,type='l',main='Correlogram-PACF')

#if we see that the acf is cutting off at lag=q and pacf is tailing down we say that the precoss is MA(q)