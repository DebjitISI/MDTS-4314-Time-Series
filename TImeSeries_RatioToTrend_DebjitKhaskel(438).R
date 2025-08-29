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


### QUADRATIC MODEL

#### 1.Yearly averages will be obtained to remove several fluctuations


x_year=aggregate(AirPassengers,nfrequency = 1,FUN=mean);x
t_year=1:12
plot(x_year)


#### 2.plotting yearly yearly averages against time and fitting an appropiate yearly trend eq

fit_rt_quad = lm(x_year~t_year + I(t_year^2))
lines(1949:1960,fitted(fit_rt_quad),type="l",col="red")


#### 3.Convert the yearly trend equation to monthly trend equation

month_tr_quad=function(t){
  fit_rt_quad$coefficients[1] + fit_rt_quad$coefficients[2]*(t+0.5)/12 + fit_rt_quad$coefficients[3]*((t+0.5)/12)^2
}
plot(1:144,AirPassengers,type='l',xlab = "months")
lines(1:144,month_tr_quad(6:149),type='l',col='red')


#### 4.Then detrend the original series by taking ratios X_t/T_t


detrend_rt_quad = AirPassengers/month_tr_quad(6:149)
plot(detrend_rt_quad)
dt_rt_quad = matrix(detrend_rt_quad,ncol=12,byrow=T)


#### 5.Obtain adjusted Seasonal indices from the detrended data


unadj_s_rt_quad = colMeans(dt_rt_quad)
adj_s_rt_quad = unadj_s_rt_quad /mean(unadj_s_rt_quad)
plot(1949:1960,adj_s_rt_quad,type='l',xlab='Time',main="Adjusted_Seasonal Idices")


#### 6.Deseasonalize the detrended data by dividing the detrended series by the corresponding adjusted seasonal

deseasonalized_estimate_rt_quad = detrend_rt_quad/adj_s_rt_quad
plot(1:144,deseasonalized_estimate_rt_quad,main = 'Deseasonalized and Detrended for Quadratic Model',type='l',xlab='time_months')

#WE SEE THAT THE QUADRATIC MODEL GIVES MUCH GOOD REPRESENTATION OF IRREGULARITY