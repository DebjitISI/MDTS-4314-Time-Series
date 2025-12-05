epsilont=rnorm(100, mean = 0,sd = 1)

#gaussian white noise
w = seq(-0.5,0.5,0.01)
f.w = rep(1, length(w))
plot(w, f.w, type = "l")

#MA(1)
##1i -> iota
w1 = seq(0, 0.5, 0.01) 
f.w1 = 1.25 + 0.5*exp(-2*pi*1i*w1) + 0.5*exp(2*pi*1i*w1)

plot(w1, f.w1, type = "l")

#AR(2) process = ARMA(2,0) process
w2 = seq(0, 0.5, 0.01)
f.w2 = 1/(abs(1 - exp(-2*pi*1i*w2) + 0.9*exp(-4*pi*1i*w2))^2)

plot(w2, f.w2, type = "l")

w2[f.w2 == max(f.w2)] 

xt = arima.sim(n = 1000, list(ar = c(1, -0.9)), sd = 1)
plot(xt, type = "l")
