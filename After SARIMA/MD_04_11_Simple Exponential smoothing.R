sales = c(26,28,23,27,24,30,26,27)

library(forecast)

a = seq(0.1,0.3,0.01)
sse = c()
f = c()
for (i in 1:length(a)) {
  fit = HoltWinters(sales, alpha = a[i], gamma = FALSE, beta = FALSE)
  #forecast1 = forecast::forecast(fit, h = 1) # 1 step ahead prediction
  #f[i] = as.numeric(forecast1$mean) #prediction (point forecast)
  sse[i] = fit$SSE #error 
}

#data.frame(a, f, sse)
sse

#optimal alpha
a[sse == min(sse)]

fit = HoltWinters(sales, alpha = 0.1, gamma = FALSE, beta = FALSE)
forecast1 = forecast::forecast(fit, h = 1)
f = as.numeric(forecast1$mean)
f #final forecasted value for the 9th day
fit$SSE # final model's SSE

plot(forecast1)
