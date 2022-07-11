

#importing libraries
library(timeSeries)
library(forecast)
library(astsa)

#reading the dataset from the file location. 
data <- read.csv("C:/Users/tejas/Desktop/PSG/sem 3/R programming/beer.csv")
#As the file is intially stored as a dataframe, to convert it into a time series dataset
beerdata <- ts(data, frequency=12,start=c(1956,1),end=c(1995,8))
#obtaining the 2nd column
beerdata<-beerdata[,2]
#The class would show the typr of dataset it is
class(beerdata)

#Basic Functions on data sets
start(beerdata)
end(beerdata)
frequency(beerdata)
cycle(beerdata)


plot(beerdata)
# in this plot the mean isn't constant and the variance isnt constant
#i.e it isn't stationary data. 
#we can perform time series analysis only on stationary data

plot(diff(beerdata))
#hence to make it stationary we differnatiate and apply logarithm
abline(reg=lm(diff(log(beerdata))~time(diff(log(beerdata)))))
#reason for differntiation

#AR I MA -Auto Regressive Integration Moving Average
# p d q
#p- We use p past observations from the time series as predictors
#d- No of times differentiated
#q- Uses q past forecast errors as predictors

#acf-auto correlation function

boxplot(diff(log(beerdata))~cycle(diff(log(beerdata))))
acf(diff(log(beerdata)))#Determines the value of q  here it's 1(line before getting inverted)

#partial auto correlation function - determines value of p
pacf(diff(log(beerdata)))
#d - number of differentiation 1
auto.arima(beerdata)

#fitting the arima model 
fit<-arima(log(beerdata),order=c(1,1,2),list(order=c(1,1,2),period=12))

#preditcion of the beerdata for 10 years ahead
pred<-predict(fit,n.ahead=10*12)

#plotting the predicted data
ts.plot(beerdata,2.718^pred$pred,log="y",lty=c(1,3))


#checking the accuracy of prediction with the already existing original values
beerdata1<-ts(data,frequency=12,start = c(1956,1),end=c(1980,12))
beerdata1<-beerdata1[,2]
class(beerdata1)
auto.arima(diff(log(beerdata1)))
fit<-arima(log(beerdata1),c(1,1,2),list(order=c(1,1,2),period=12))

pred<-predict(fit,n.ahead = 5*12)

predicted<-2.718^pred$pred
predicted

beerdata


#onestep ahead sarima

set<-window(beerdata,start=c(1956,1),end=c(1992,12))   #subset until 1992
validation<-window(beerdata,start=c(1993,1))
one_step_ahead_sarima<-matrix(ncol=2,nrow=36)
arima_optional<-auto.arima(set)

#P,D,Q- Seasonal Model
for(i in 1:36){
  
  observed_set<-window(beerdata,start=c(1956,1),end=c(1992,12+i),frequency=12)
  forecasted_sarima<-sarima.for(observed_set,n.ahead=1,p=1,d=1,q=2,P=0,D=1,Q=3,S=12)
  demandforecast=forecasted_sarima$pred
  observed=validation[[i]]
  one_step_ahead_sarima[i,1]=observed
  one_step_ahead_sarima[i,2]=demandforecast    
}

plot(beerdata,col='blue',xlab='Year')
lines(ts(one_step_ahead_sarima[,2],start=c(1993,1),frequency=12),col='red',lwd=3)