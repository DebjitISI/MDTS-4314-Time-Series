rm(list=ls())
AirPassengers
print("This data is a monthly data from year 1949-1960.The classic Box & Jenkins airline data. Monthly totals of international airline passengers, 1949 to 1960.")
plot(AirPassengers)
print("X_t be the monthly passenger total")
print("As from the plot we can say that the model is multiplicative model.")

#Description of the data:
#here we are given  Monthly totals of international airline passengers, 1949 to 1960.
#Let X_t denote the monthly total of international airline passengers corresponding to the time point t.
#Where the origin and the state of time is so chosen that t takes up the values 1,2,3,4,...144(i.e Dec 1948,unit 1)
# we first obtain time series plot of the data
plot(AirPassengers)
#From the plot we observe that an evolutive pattern is present in the data dominated by an upward trend component. seasonal variation is also present.Cyclical Variation is absent.
#Since the intensity of the seasonal fluctuatio is increasing with increasing trend , a multiplicative model will be more approprate hence the model under study is:
#X_t = T_t*S_t*I_t



#Now we find the moving average for the model

#TO ONTAIN SEASONAL INDEX 
#Here we will first obtain the trend estimate by the method of moving average.
# 1st 12 values are considered and A.M is calculated and the corresponding modpoint with rest to the modpoint of june or july 1949
# in the next step the obs corresponding to the jan 1949 is dropped and the obs corresponding to jan 1950 is included
#The corresponding A.M is calulated for this dataset and is reported against the mid point of july and Aug 1949.
# in this way the subsequent m.a are obtained until we reach the last set of 12 datapoints
#To obtain the centered 12 point moving average values a subsequent 2 item moving average is calculated
#This are the trend estimated T_t_hat. It is to noted that the trend estimates correspond to the 1st 6 months of 1949 and the ;ast 6 months 1960 are NA.
# Next we obtain the detrended data by dividing the original series X_t by the corresponding trend estimate T_t_hat i.e we take the ratios X_t/T_t_hat.
#Tis ratios should ideally contain S_t*I_t. For each month the corresponding unadjusted seasonal index is obtained by averaging the ratios over theconsequtive years for a corresponding monath.
# Finally the adjusted seasonal indices are obtained by dividing the unadjusted seasonal indices by their grand mean.
#This method is called ratio to moving average method
#(In case of less time we write only this instead of writing the whole thing) the trend estimate is obtained by 12 point moving average.Mention the adjusted seasonal indices


library(forecast)
par(mfrow=c(2,2))
ma = ma(AirPassengers,order=12);ma
#x_year=aggregate(AirPassengers,nfrequency = 1,FUN=mean);x
#plot(x_year)
detrend = AirPassengers/ma
plot(detrend,main = "Detrended")
detrend[is.na(detrend)]<-0
dt = matrix(detrend,ncol=12,byrow=T)
unadj_s = colMeans(dt)
adj_s = unadj_s /mean(unadj_s)
plot(1949:1960,adj_s,type='l',xlab='Time',main='Adjusted Seasonal Index')




?decompose
decomp=decompose(AirPassengers,type='multiplicative')
# we are again find the trend estimate as we see that the trend estimate for the first and lst 6 months are absent
deseasonalized_estimate = AirPassengers/adj_s
#or
deseasonalized_estimate = AirPassengers/decomp$seasonal
par(mfrow=c(1,2))
plot(1:144,deseasonalized_estimate,main = 'Deseasonalized',type='l',xlab='time_months')
t= 1:144

fit = lm(deseasonalized_estimate ~t)
lines(fitted(fit),type="l",col="red")

trend_estimate = predict(fit)
res = deseasonalized_estimate/trend_estimate
plot(res,main='linear')

#Next we obtain the deseasonalized series by dividing the original data by the corrresponding adjusted seasonal indices
#This series should ideally contain the trend component and the irregular fluctuation
# we now fit an appropritae trend eq to this deseasonalized data
#From the plot of deseasonaled data t seems that: 

fit_quad = lm(deseasonalized_estimate ~t+I(t^2))

lines(fitted(fit_quad),type="l",col="blue")

trend_estimate_quad = predict(fit_quad)
res_quad = deseasonalized_estimate/trend_estimate_quad
plot(res_quad,main='Quadratic')



#NOW WE DO THE RATIO TO TREND METHOD

#Yearly averages will be obtained to remove several fluctuations
#plotting yearly yearly averages gainst time and fitting an appropiate yearly trend eq
#COnvert the yearly trend equation to monthly trend equation
#Then detrend the original series by taking ratios X_t/T_t
#Obtain adjusted Seasonal indices from the detrended data 
#Deseasonalize the detrended data by dividing the detrended series by the corresponding adjusted seasonal 


#Yearly averages will be obtained to remove several fluctuations
x_year=aggregate(AirPassengers,nfrequency = 1,FUN=mean);x
t_year=1:12
plot(x_year)

#plotting yearly yearly averages gainst time and fitting an appropiate yearly trend eq
fit_rt = lm(x_year~t_year)
lines(1949:1960,fitted(fit_rt),type="l",col="red")

#COnvert the yearly trend equation to monthly trend equation
month_tr=function(t){
  fit_rt$coefficients[1] + fit_rt$coefficients[2]*(t+0.5)/12
}
plot(1:144,AirPassengers,type='l',xlab = "months")
lines(1:144,month_tr(6:149),type='l',col='red')

#Then detrend the original series by taking ratios X_t/T_t
detrend_rt = AirPassengers/month_tr(6:149)

dt_rt = matrix(detrend_rt,ncol=12,byrow=T)

#Obtain adjusted Seasonal indices from the detrended data 
unadj_s_rt = colMeans(dt_rt)
adj_s_rt = unadj_s_rt /mean(unadj_s_rt)
plot(1949:1960,adj_s_rt,type='l',xlab='Time',main="Adjusted_Seasonal Idices")

#Deseasonalize the detrended data by dividing the detrended series by the corresponding adjusted seasonal 
deseasonalized_estimate_rt = detrend_rt/adj_s_rt
plot(1:144,deseasonalized_estimate_rt,main = 'Deseasonalized and Detrended',type='l',xlab='time_months')


