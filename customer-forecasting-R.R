#install.packages("VIM")
#install.packages("mice")
#install.packages("forecast")
library(VIM)
library(tidyverse)
library(Amelia)
library(mice)
library(forecast)
dental <- read.csv("BestSmileDental.csv", header=TRUE, stringsAsFactors = FALSE)
head(dental)
str(dental)
summary(dental)
attach(dental)

#convert to integers, convert rows with characters to NA
dental$Customers <- as.integer(Customers)
summary(dental)

#convert negative values to NA
dental$Customers[dental$Customers <= 0] <- NA
summary(dental)

#vizualize missing data
aggr(dental, prop=FALSE, numbers=TRUE)
missmap(dental)

#visualize time series before imputation
plot.ts(dental$Customers,ylab="Customers",main="Dental Customers Over Time")

#impute values with the MICE package, 3 sets
set.seed(1235)
imp <- mice(dental,3)
imputedValues <- imp$imp$Customers
imputedData <- complete(imp)
imputedData2 <- complete(imp,2)
imputedData3 <- complete(imp,3)

#add column to imputed datasets to keep track of which values were imputed
imputedData$imputed <- imp$where[,3]
imputedData2$imputed <- imp$where[,3]
imputedData3$imputed <- imp$where[,3]

#examine each of the imputed sets
plot.ts(imputedData$Customers, col="red")
lines(imputedData2$Customers, col="blue")
lines(imputedData3$Customers, col="green")
lines(dental$Customers)
legend("topleft",c('Set 1','Set 2','Set 3'),pch=c(15),col=c('red','blue','green'))

#forcast using Holt-Winters exponential smoothing
#alpha is smoothing constant, beta is trend, gamma is seasonality

#HW MODEL 1
hwForecast <- HoltWinters(imputedData2$Customers, alpha = 0.2, beta= FALSE, gamma=FALSE)
hwPredict <- predict(hwForecast, n.ahead=12, prediction.interval=TRUE)
hwAccuracy <- accuracy(forecast(hwForecast, n.ahead=12, prediction.interval=TRUE))

#HW MODEL 2
hwForecast2 <- HoltWinters(imputedData2$Customers, alpha = 0.3, beta= TRUE, gamma=FALSE)
hwPredict2 <- predict(hwForecast2, n.ahead=12, prediction.interval=TRUE)
hwAccuracy2 <- accuracy(forecast(hwForecast2, n.ahead=12, prediction.interval=TRUE))

#HW MODEL 3
hwForecast3 <- HoltWinters(imputedData2$Customers, gamma=FALSE)
hwPredict3 <- predict(hwForecast3, n.ahead=12, prediction.interval=TRUE)
hwAccuracy3 <- accuracy(forecast(hwForecast3, n.ahead=12, prediction.interval=TRUE))

#view values for next 12 predictions
hwPredict
hwPredict2
hwPredict3

#view fitted means
hwForecast$fitted
hwForecast2$fitted
hwForecast3$fitted

#view smoothing parameters
hwForecast
hwForecast2
hwForecast3

#plot bounds for Holt-Winters forecast #1
plot.ts(imputedData2$Customers, xlim=c(0,100), ylim=c(100,9000))
lines(hwForecast$fitted[,1], col="green")
lines(hwPredict[,1], col="blue")
lines(hwPredict[,2], col="red")
lines(hwPredict[,3], col="red")

#plot bounds for Holt-Winters forecast #2
plot.ts(imputedData2$Customers, xlim=c(0,100), ylim=c(100,9000))
lines(hwForecast2$fitted[,1], col="green")
lines(hwPredict2[,1], col="blue")
lines(hwPredict2[,2], col="red")
lines(hwPredict2[,3], col="red")

#plot bounds for Holt-Winters forecast #3
plot.ts(imputedData2$Customers, xlim=c(0,100), ylim=c(100,9000))
lines(hwForecast3$fitted[,1], col="green")
lines(hwPredict3[,1], col="blue")
lines(hwPredict3[,2], col="red")
lines(hwPredict3[,3], col="red")

#ARIMA MODEL 1
#forecast using ARIMA, let R choose p,d,q -> (0,1,1)
arimaForecast <- auto.arima(x=imputedData2$Customers)
arimaForecast
#print acf, pcf, and coefficients
acf(arimaForecast$residuals)
pacf(arimaForecast$residuals)
coef(arimaForecast)
#perform forecasting for next 12 periods
arimaPredictions<- forecast(arimaForecast, h=12)
arimaPredictions
#plot predictions
plot(arimaPredictions)
#save accuracy data
arimaAccuracy = accuracy(arimaPredictions)

#ARIMA MODEL 2
#forecast using ARIMA(0,2,1)
arimaForecast2 <- arima(imputedData2$Customers, order=c(0,2,1))
arimaForecast2
#print acf, pcf, and coefficients
acf(arimaForecast2$residuals)
pacf(arimaForecast2$residuals)
coef(arimaForecast2)
#perform forecasting for next 12 periods
arimaPredictions2  <- forecast(arimaForecast2, h=12)
arimaPredictions2
#plot predictions
plot(arimaPredictions2)
#save accuracy data
arimaAccuracy2 = accuracy(arimaPredictions2)

#ARIMA MODEL 3
#forecast using ARIMA(1,1,2)
arimaForecast3 <- arima(imputedData2$Customers, order=c(1,1,2))
arimaForecast3
#print acf, pcf, and coefficients
acf(arimaForecast3$residuals)
pacf(arimaForecast3$residuals)
coef(arimaForecast3)
#perform forecasting for next 12 periods
arimaPredictions3  <- forecast(arimaForecast3, h=12)
arimaPredictions3
#plot predictions
plot(arimaPredictions3)
#save accuracy data
arimaAccuracy3 = accuracy(arimaPredictions3)

#Compare accuracy measures of models
hwAccuracy
hwAccuracy2
hwAccuracy3
arimaAccuracy
arimaAccuracy2
arimaAccuracy3

