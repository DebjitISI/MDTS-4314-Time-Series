set.seed(1000)
# for 100 observations
t = 1:100
xt1 = 2*cos(2*pi*t*0.06) + 3*sin(2*pi*t*0.06)
xt2 = 4*cos(2*pi*t*0.10) + 5*sin(2*pi*t*0.10)
xt3 = 6*cos(2*pi*t*0.40) + 7*sin(2*pi*t*0.40)

xt = xt1 + xt2 + xt3 #superimposition


I = abs(fft(xt))^2/100 #the periodogram
P = (4/100)*I[1:50] # the scaled periodogram
f = 0:49/100 #frequencies
# considering one half, bcoz folding frequency, the 
#other one will be the mirror image of the first one

plot(f, P, type = "l", xlab = "frequnecy", ylab = "scaled periodogram")
f[P == max(P)] #most dominant frequency
f[order(P, decreasing = TRUE)][1:3] # dominant frequencies

# order() -> gives the indices of the P, when arranged in decreasing order, using those 
#indices to get hold of the corresponding to the f values on those indices

##second half is the mirror image of the first part
I = abs(fft(xt))^2/100 #the periodogram
P = (4/100)*I # the scaled periodogram
f = 0:99/100 #frequencies
# considering one half, bcoz folding frequency, the other one will be the mirror image of the first one

plot(f, P, type = "l", xlab = "frequnecy", ylab = "scaled periodogram")
f[P == max(P)] #dominant frequency