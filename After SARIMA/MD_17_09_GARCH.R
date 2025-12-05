library(astsa)

plot(nyse, type = "l")
d = nyse
##volatility is present - sudden spikes, sudden bursts

#modelling the mean part
acf(nyse, lag.max = 100)
pacf(nyse, lag.max = 100)

acf2(nyse)  #will give both acf & pacf plots together
#significant spikes in the acf - so serial correlation is present

#q = 0,1,2
#p = 0,1,2

fit.1 = arima(nyse, order = c(0,0,1), method = "ML")
fit.2 = arima(nyse, order = c(0,0,2), method = "ML")
fit.3 = arima(nyse, order = c(1,0,0), method = "ML")
fit.4 = arima(nyse, order = c(1,0,1), method = "ML")
fit.5 = arima(nyse, order = c(1,0,2), method = "ML")
fit.6 = arima(nyse, order = c(2,0,0), method = "ML")
fit.7 = arima(nyse, order = c(2,0,1), method = "ML")
fit.8 = arima(nyse, order = c(2,0,2), method = "ML")

#after obtaining the fitted models, perform ljung box test - residuals should be random (pvalue>0.05 - random)
checkresiduals(fit.1, lag = 20) #autocorrelated
checkresiduals(fit.2, lag = 20) #autocorrelated
checkresiduals(fit.3, lag = 20) #autocorrelated
checkresiduals(fit.4, lag = 20) #autocorrelated
checkresiduals(fit.5, lag = 20) #autocorrelated
checkresiduals(fit.6, lag = 20) #autocorrelated
checkresiduals(fit.7, lag = 20) #random
checkresiduals(fit.8, lag = 20) #random

#check the AIC 
fit.7$aic
fit.8$aic #select this one (lower aic value)

#select ARMA(2,2)

res = fit.8$residuals

#Testing for arch effect
library(FinTS)
ArchTest(res, lag = 1)
#pvalue<0.05, no arch effect gets rejected (arch effect is present)

#to determine the order of the volatility model
acf(res^2, lag = 100)
pacf(res^2, lag = 100)

#both tailing off - Garch(p,q) model
#significant spikes
#using acf: q - 0,1,2,3
#using pacf: p - 0,1,2,3

library(fGarch)
#gfit.1 = garchFit(~arma(2,2) + garch(0,1), d, trace = FALSE)
#gfit.2 = garchFit(~arma(2,2) + garch(0,2), d, trace = FALSE)
#gfit.3 = garchFit(~arma(2,2) + garch(0,3), d, trace = FALSE)

# in Garch(p,q): p>0, so
gfit.4 = garchFit(~arma(2,2) + garch(1,0), d, trace = FALSE)
gfit.5 = garchFit(~arma(2,2) + garch(1,1), d, trace = FALSE)
gfit.6 = garchFit(~arma(2,2) + garch(1,2), d, trace = FALSE)
gfit.7 = garchFit(~arma(2,2) + garch(1,3), d, trace = FALSE) #error
gfit.8 = garchFit(~arma(2,2) + garch(2,0), d, trace = FALSE)
gfit.9 = garchFit(~arma(2,2) + garch(2,1), d, trace = FALSE)
gfit.10 = garchFit(~arma(2,2) + garch(2,2), d, trace = FALSE)
gfit.11 = garchFit(~arma(2,2) + garch(2,3), d, trace = FALSE) #error
gfit.12 = garchFit(~arma(2,2) + garch(3,0), d, trace = FALSE)
gfit.13 = garchFit(~arma(2,2) + garch(3,1), d, trace = FALSE)
gfit.14 = garchFit(~arma(2,2) + garch(3,2), d, trace = FALSE) #error
gfit.15 = garchFit(~arma(2,2) + garch(3,3), d, trace = FALSE) #error


#get the standardized residuals
res4 = residuals(gfit.4, standardize = TRUE)
res5 = residuals(gfit.5, standardize = TRUE)
res6 = residuals(gfit.6, standardize = TRUE)
res8 = residuals(gfit.8, standardize = TRUE)
res9 = residuals(gfit.9, standardize = TRUE)
res10 = residuals(gfit.10, standardize = TRUE)
res12 = residuals(gfit.12, standardize = TRUE)
res13 = residuals(gfit.13, standardize = TRUE)


acf(res4, lag = 100) #random, similar for other
#these standardized residuals should be iid random variables 
#performing the Ljung Box test - residuals would be random (pvalue> 0.05 - residuals are random)
checkresiduals(res4, lag = 20) #random
checkresiduals(res5, lag = 20) #random
checkresiduals(res6, lag = 20) #random
checkresiduals(res8, lag = 20) #random
checkresiduals(res9, lag = 20) #random
checkresiduals(res10, lag = 20) #random
checkresiduals(res12, lag = 20) #random
checkresiduals(res13, lag = 20) #ranodm


#check the aic values, select the model best model
summary(gfit.4) #-6.642565
summary(gfit.5) #-6.728621
summary(gfit.6) #-6.730819
summary(gfit.8) #-6.675308
summary(gfit.9) #-6.727552
summary(gfit.10) #-6.729819
summary(gfit.12) #-6.691220
summary(gfit.13) #-6.726521



#so Garch(1,2) is the best model (lowest aic)
#for mean part - ARMA(2,2)
#for var part - GARCH(1,2)

#to get the predicted values/estimates of the conditional variance (sigmat^2)
u = garchFit(~arma(2,2)+garch(1,2), d, trace = FALSE)@sigma.t
u^2
