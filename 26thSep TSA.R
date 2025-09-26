library(astsa)
library(forecast)
soi
plot(soi)
acf2(soi,max.lag=100)
sarima.for(flu,60,1,1,0,1,1,1,12)
auto.arima(flu)
auto.arima(flu)

### FREQUENCY DOMAIN

Tt = 1:100
X_t=X_t1=X_t2=X_t3=array(dim = length(Tt))
for (t in Tt) {
  X_t1[t] = 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
  X_t2[t] = 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
  X_t3[t] = 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
  X_t[t] = X_t1[t] + X_t1[t] + X_t1[t]
}  
par(mfrow = c(2,2))
plot(X_t1,type = 'l',main = paste('A=',(2^2+3^2)))
plot(X_t2,type = 'l',main = paste('A=',(4^2+5^2)))
plot(X_t3,type = 'l',main = paste('A=',(6^2+7^2)))
plot(X_t,type = 'l',main = 'Superimposed')


set.seed(1234)
ep = rnorm(500)
A = 2
Tt = 1:500
phi = .6*pi
omega = 1/50
Xt = A * cos(2*pi*omega*Tt + phi)+ep
plot(Xt,type = 'l')

b2=-A*sin(phi)
b1=A*cos(phi)

#model
t = 1:500
y = Xt
m = lm(y ~ 0+cos(2*pi*omega*t)+sin(2*pi*omega*t))
summary(m)
lines(fitted(m),col='red',lwd=2)
or = b1*cos(2*pi*omega*t)+b2*sin(2*pi*omega*t)
lines(or,col='blue',lwd=2)


