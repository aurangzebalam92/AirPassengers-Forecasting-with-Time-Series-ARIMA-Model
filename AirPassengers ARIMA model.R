#Clear all the objects
rm(list=ls())

#Load the inbulit data that is AirPassengers
data("AirPassengers")

#Check the data type 
class(AirPassengers)

#Check the start of AirPassengers
start(AirPassengers)

#Check the end of the AirPassengers
end(AirPassengers)

#Check the frequency of the Data
frequency(AirPassengers)

#Summary of the Data
summary(AirPassengers)

#This will print the cycle of the year
cycle(AirPassengers)

#The plotting of data shows the trend of the data
#Plottting of the data
plot(AirPassengers)

#Now lets plot the mean
abline(reg=lm(AirPassengers~time(AirPassengers)))
#This shows that the data series is not stationary so our main goal is to make it stationary. 
#We have to make mean contant with the time and also variance should be equal at different time intervals from the mean.

#Aggregate cycles and show a year to year trend
plot(aggregate(AirPassengers, FUN = mean), ylab = "AirPassengers (mean)")

#Using a box plot we will try to get a sense for a possible seasonal effect
boxplot(AirPassengers~cycle(AirPassengers), xlab = "Month", ylab = "AirPassengers", main = "Average Passengers per Month", names = month.abb, col = "yellow")
#higher numbers for July and August
#different but small variance 

#Checking if time series data are stationary
#Before testing for stationarity in our time series data, we need to address two issues:
#We need to remove unequal variances. This can be done by taking the log of the values.
#We need to address the trend component. This can be done by taking the difference of the series
#We perform the Dickey-Fuller Test, in order to check if our data are stationary, using the difference of logged values.
library(tseries)
#Dickey Fuller test with the difference of logged values
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

#Now lets make variance equal at different time interval from the mean by using log function.
plot(log(AirPassengers))

#Now variance is equal at different time interval but mean is not constant according to time so lets make it constant.
plot(diff(log(AirPassengers)))

#Build the Arima Model
#p, d and q are the parameter of the arima models and their value we will get from auto correlation graph
#Now plot the autocorrelation function graph
acf(AirPassengers)
#After plotting auto correlation graph then we see that values are exceeding the blue line but our aim is to keep the values under the blue line.

#Now lets find the value of q which is cofficient of MA(Moving average)
acf(diff(log(AirPassengers)))
#We have to take the line value which is just before the first inverted line and that will be the value of q that is 1

#Now lets find the value of q which is cofficient of I(Integrated)
pacf(diff(log(AirPassengers)))
#We have to take the line value which is just before the first inverted line and that will be the value of q that is 0

#Now we have to find the value of d which we can achieve by differentiating. 
#We can do multiple differentiation till we don't achieve the constant mean. the number of differentiation we will do that will be the value of d.
#Here we dp differention once so value of d will be 1.
#the value of p,d and q we will be using in our arima model

library(forecast)
auto.arima(AirPassengers)
#Lets fit the Arima model
model_fit = arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

#after arima model fit, predict the future 10 years
forecasting1 = predict(model_fit, n.ahead=10*12) 

#Now find the log of the predicted values.
print(forecasting1$pred)

#Our values are in logarithmic form so we have to convert it in decimal using e whose value is 2.718
logarithm_to_decimal = 2.718^forecasting1$pred

#find the actual predicted values.
logarithm_to_decimal

#We now visualize the prediction along with the training data.
ts.plot(AirPassengers,2.718^forecasting1$pred, log = "y", lty = c(1,3))
#log = "y' is to plot on a logarithmic scale
#lty = c(1,3) will set the LineTYpe to 1 (for solid) for the original time series and 3 (for dotted) for the predicted time series.
 
#print(2.718^pred$pred) would give us the actual predicted values.

#Testing our model
#Recreating model till 1959
datawide = ts(AirPassengers, frequency = 12,start = c(1949,1),end=c(1959,12))

#Fitting model now
fit_model = arima(log(datawide),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))

#Prediction
prediction_value = predict(fit_model,n.ahead = 10*12)
prediction_value

#Change logarithmic values into decimal form
prediction_value1 = 2.718^prediction_value$pred
prediction_value1

#Lets find 1960 value alone
data1 = head(prediction_value1,12)

#Now round the values 
data_1960 = round(data1,digits = 0)
data_1960

#Now lets compare from the original values 
original_1960 = tail(AirPassengers,12)
original_1960


