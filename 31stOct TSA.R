?fft
#Gen 500 obs from a periodic process 
set.seed(1000)
w = 1/50
x= 2*cos(2*pi*w*t + 0.6*pi) + rnorm(500,0,5)
I = abs(fft(x))^2/500;I
P = (4/500)*I[1:250];P
f = 0:249/500;f
x
#Now we see the whole frequency plot
f1 = 0:499/500;f1
P1 = (4/500)*I;P1
plot(x,type='l')
plot(f,P,type='l',xlab='Frequency',ylab = 'Scaled Periodogram')
plot(f1,P1,type='l',xlab='Frequency',ylab = 'Scaled Periodogram')
f[P==max(P)]
f1[P1==max(P1)]

########
rm(list = ls())
X_t1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
X_t2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
X_t3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
X = X_t1 + X_t2 + X_t3

I = abs(fft(X))^2/100;I
P = (4/100)*I[1:50];P
f = 0:49/100;f

plot(f,P,type='l',xlab='Frequency',ylab = 'Scaled Periodogram')


#f[which(P==sort(P,decreasing = T))]
f[order(P,decreasing = T)][1:3]

w = seq(-0.5,0.5,length.out = 100)
plot(w,rep(1,100),type='l')

w = seq(0,0.5,length.out = 100)
f = 1.25 + 0.5*(exp(-2i*pi*w)+exp(2i*pi*w))
plot(w,f,type='l')

w = seq(0,0.5,length.out = 100)
f = 1/abs(1-exp(-2i*pi*w)+0.9*exp(-4i*pi*w))^2
plot(w,f,type='l')
w[f==max(f)]
plot(arima.sim(n=5000,list(ar=c(1,-0.9)),rand.gen = rnorm))
