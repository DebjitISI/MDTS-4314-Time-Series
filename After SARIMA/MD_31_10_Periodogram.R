w = 1/50
# add some noise to these generated data -from N(0,1)

set.seed(1234)
t = 1:500
epsilont = rnorm(500, mean = 0, sd = 1)
#A*cos(2*pi*w*t) + epsilont, here w,A,phi is provided to generate the series
#A = 2, phi = 0.65
x = 2*cos(2*pi*w*t + 0.65) + epsilont 
plot(xt.new, type = "l")

## ma'am
set.seed(1000)
x = 2*cos(2*pi*1:500/50 + 0.6 * pi) + rnorm(500, 0, 5)
I = abs(fft(x))^2/500 #the periodogram
P = (4/500)*I[1:250] # the scaled periodogram
f = 0:249/500 #frequencies
# considering one half, bcoz folding frequnecy, the other one will be the mirror image of the first one

plot(f, P, type = "l", xlab = "frequnency", ylab = "scaled periodogram")
f[P == max(P)] #dominant freqeuncy


####################### 
#if entire thing is considered then, one half is the mirror image of the first part
set.seed(1000)
x = 2*cos(2*pi*1:500/50 + 0.6 * pi) + rnorm(500, 0, 5)
I = abs(fft(x))^2/500 #the periodogram
P = (4/500)*I # the scaled periodogram
f = 0:449/500 #frequencies
# considering one half, bcoz folding frequnecy, the other one will be the mirror image of the first one

plot(f, P, type = "l", xlab = "frequnency", ylab = "scaled periodogram")
f[P == max(P)]