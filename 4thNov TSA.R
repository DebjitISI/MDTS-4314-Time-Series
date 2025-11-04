## Forecasting
rm(list=ls())
data = c(26,28,23,27,24,30,26,27)
?HoltWinters()

alpha = seq(0.1,0.3,length.out=5)
df <- data.frame(
  Alpha = alpha
)
m=c()
for (a in alpha) {
  paste("For alpha:",a)
  fit=HoltWinters(data,alpha=a,beta = F,gamma = F)
  forecast = forecast::forecast(fit,length=1)
  pred = as.numeric(forecast$mean)
  m = c(m,fit$SSE)
}
df$mse=m
df
