rm(list=ls())

# ARIMA of Tractor Sales from 2003-2014
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
Tractors = ts(data[,2], start = c(2003,1), frequency = 12)
Tractors # dislay the tractor data
plot(Tractors, xlab = 'Years', ylab = 'Tractor Sales')
require(forecast)
Tractorfit = auto.arima(log10(Tractors), approximation=FALSE, trace=FALSE)
summary(Tractorfit)
Tractorpred <- predict(Tractorfit, n.ahead = 5*12)

Tractorpred
