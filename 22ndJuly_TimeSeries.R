co2
#Removing Trend
diff1 = diff(co2,lag=1,differences = 1)
plot(diff1)

diff2 = diff(diff1,lag=1,differences = 1)
plot(diff2)

data_trans = log(AirPassengers)# if trend is non linear so we take order 2 or if we still see an upward trend we keep increasing the order
#we transform the data as its a multipicative model and so we make it additive to do difference
diff3 = diff(data_trans,lag=1,differences = 1)
plot(diff3)

#Removing seasonal component

diff4 = diff(co2,lag=12,differences = 1)
plot(diff4)# only desasonalized
diff5 = diff(diff1,lag=12,differences = 1)
plot(diff5)#Deseasonalized and detrended

diff4 = diff(AirPassengers,lag=12,differences = 2)
plot(diff4)# only desasonalized
diff5 = diff(diff3,lag=12,differences = 1)
plot(diff5)#Deseasonalized and detrended after transformation


data_trans1=log(JohnsonJohnson)
diff_detrend = diff(data_trans1,lag=1,differences = 2)
plot(diff_detrend)
diff_detrend_deseason = diff(diff_detrend,lag=12,differences=2)
plot(diff_detrend_deseason)
