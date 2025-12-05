par(mfrow = c(2,2))
t = 1:100
xt1 = 2*cos(2*pi*t*0.06) + 3*sin(2*pi*t*0.06)

plot(xt1, type = "l")

xt2 = 4*cos(2*pi*t*0.10) + 5*sin(2*pi*t*0.10)

plot(xt2, type = "l")

xt3 = 6*cos(2*pi*t*0.40) + 7*sin(2*pi*t*0.40)

plot(xt3, type = "l")

xt = xt1 + xt2 + xt3 #superimposition

plot(xt, type = "l")

##Next question
par(mfrow = c(1,1))
set.seed(1234)
t = 1:500
epsilont = rnorm(500, mean = 0, sd = 1)
#A*cos(2*pi*w*t) + epsilont, here w,A,phi is provided to generate the series
#A = 2, phi = 0.6*pi
xt.new = 2*cos(2*pi*0.02*t + 0.6*pi) + epsilont 
plot(xt.new, type = "l")

##find the estimates of A and phi (using linear regression)
fit = lm(xt.new ~ 0 + cos(2*pi*0.02*t) + sin(2*pi*0.02*t))
summary(fit) #b1.hat, b2.hat is obtained

##original b1, b2
A = 2; phi = 0.6*pi
b1 = A*cos(phi)
b2 = -A*sin(phi)

#original series without contamination
original_without_contamination = b1*cos(2*pi*0.02*t) + b2*sin(2*pi*0.02*t)

plot(xt.new, type = "l", col = "red") #original series with contamination
lines(predict(fit), col = "black", lwd = 2) #estimated smooth signal
lines(original_without_contamination, col = "blue", lwd = 1) #original smooth signal without contamination


