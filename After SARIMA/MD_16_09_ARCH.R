library(FinTS)
#null: no arch effect, alt: arch effect is present
#ArchTest()

data = read.csv("G:/Sem 3/Time Series/September/archdata.csv")
attach(data)
d = data$data
plot(d, type = "l") #volatility is present - sudden spikes, sudden bursts
#mean level is runing around zero, but still to get hold if any autocorrelation, get acf and pacf plots

#modelling the mean part
acf(d, lag.max = 100)
pacf(d, lag.max = 100)

library(astsa)
acf2(d)  #will give both acf & pacf plots together

#serial autocorrelations is present -> significant spikes at lag 1,2,3,4
#so here we cannot simply subtract the mean from the data

#AR(1) model - pacf cuts off at lag 1, acf tails off

fit_mean = arima(d, order = c(1,0,0), method = "ML", include.mean = FALSE)

#residuals should be random (fit is good) - Lunj Box test
library(forecast)
checkresiduals(fit_mean, lag = 20) #p>0.05 -> accept null -> residuals are random (fit is satisfactory)
acf(fit_mean$residuals, lag.max = 100) # there is a single spike at lag 1, 

res = fit_mean$residuals #get hold of the res, yt = xt - uhat_t

library(FinTS)
ArchTest(res, lag = 1) #p<0.05, reject Ho -> no arch effect gets rejected (arch effect is present)
#since i want to test for ARCH(1) effect is present?, that is why lag = 1

#check the pacf of (res)^2 before the Archtest??

#check the pacf of res^2 - to get the order of the ARCH model
acf2(res^2)
#q = 1,2,3,4,5,6 (from acf)
#p = 1,2 (from pacf)
#acf is tailing off. pacf is seems to be cutting off at lag2

#fit a ARCH(2) model
# here we are modelling both the mean and variance part - using AR(1), ARCH(2) resp
library(fGarch)
m = garchFit(~arma(1,0)+garch(2,0), d, trace = FALSE)
summary(m)
#AR(1)+ARCH(2) = ARMA(1,0)+GARCH(2,0)
#(p,q) -> this are orders corresponding to AR and MA component
 
#fit a ARCH(1) model [AR(1) + ARCH(1))
m1 = garchFit(~arma(1,0)+garch(1,0), d, trace = FALSE)
summary(m1)
#get the estimates, omega -> alpha0_hat, alpha1 -> alpha1_hat

# aic for arch(2) -> -10.10126, 
# aic for arch(1) -> -9.9667
#ARCH(2) better fit (less aic)



#For simulation
spec = garchSpec()
garchSim(spec, n = 10000)


#we are getting the predicted values (estimates) of the variability (conditional variance), i.e, sigmat^2
u = garchFit(~arma(1,0)+garch(1,0), d, trace = FALSE)@sigma.t
u
u^2 #sigmat^2 = alpha0 + alpha1*yt-l^2


#sigma(garchFit(~arma(1,0)+garch(1,0), d, trace = FALSE))


##Not sure (forecasting of mean+variance)
#predict(m, n.ahead = 10)
