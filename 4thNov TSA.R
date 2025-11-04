## Forecasting
rm(list=ls())
data = c(26,28,23,27,24,30,26,27)
?HoltWinters()

alpha = seq(0.1,0.3,length.out=5)
df <- data.frame(
  Alpha = alpha
)
m=c()
f=c()
for (a in alpha) {
  model=HoltWinters(data,alpha=a,beta = F,gamma = F)
  forecast = forecast::forecast(model,h=1)
  f = c(f,as.numeric(forecast$mean))
  m = c(m,model$SSE)#This is the training error
}
df$SSE=m
df$Forecast=f
df
alpha = df$Alpha[df$SSE==min(df$SSE)];alpha
model=HoltWinters(data,alpha=0.1,beta = F,gamma = F)
forecast = forecast::forecast(model,h=1)
plot(forecast)

opt_model = HoltWinters(data,beta = F,gamma = F)#without using specified value of alpha we ge the optimal value
opt_alpha = opt_model$alpha;opt_alpha