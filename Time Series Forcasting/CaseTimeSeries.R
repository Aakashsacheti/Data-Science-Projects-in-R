#Time Series Forecasting



#Analytical Problem: To forecast the 36 Sale points

#Setting the working directory-------------------------------------------#

Path<-"C:/Users/aakas/OneDrive/IVY/IVY- R Arpendu/data"
setwd(Path)
getwd()

data0<-read.csv("1Sales1.csv",header = TRUE)
#View(data0)
data=data0
TSdata=data#To create a backup of original data


#Exploring data
#View(TSdata)
head(TSdata)#
dim(TSdata)
#We have 169 time series points (date level) and 2 vars(Date, Sales)
#but we deleted an extra time point lying in middle of our data i.e of 17th jan
#also sorted the data in excel according to dates
str(TSdata)
summary(TSdata)
colSums(is.na(TSdata))



#converting data into time series object
TSdata=ts(TSdata[,2],start=c(3,1),frequency=14)
start(TSdata)
end(TSdata)
frequency(TSdata)
str(TSdata)
TSdata
#3 jan is the start date and 12 is the frequency of the time series (monthly series)

str(TSdata)#converted to time series object


#plotting sales variable
par(mfrow=c(1,1))
plot(TSdata,ylab="Sales", xlab="month",main="Sales between jan-dec",col="blue")
abline(reg = lm(TSdata~time(TSdata)))
cycle(TSdata)
plot(aggregate(TSdata,FUN=mean))
#This plot displays the month on month trend in the sales from 3rd january 2018 

#Data has both a trend and drift, i.e. time invariant mean and variance 



#--------------->Differencing the data to remove trend and drift

#log of data to make series stationary
plot(log10(TSdata),ylab="log(Sales)",xlab="months",main="log(Sales) between jan-dec,2018",col="grey")

#Differencing the data to remove trend
plot(diff(TSdata,differences = 1),ylab="Diff(Sales)",xlab="months",main="Diff(Sales) between jan-dec,2018",col="blue")
#The differenced data continues to have unequal variance 

#Differencing + Log transformation the data to remove trend and unequal variance
plot(diff(log10(TSdata),differences = 1),ylab="Diff(Sales)",xlab="months",main="Diff(Log(Sales)) between jan-dec,2018",col="blue")
#So, with Log10 and 1 order of differencing makes the series stationary

#Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)
LDTSdata<-diff(log10(TSdata),differences = 1)

tseries::adf.test(LDTSdata,alternative="stationary")
#Since, the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary 

#testing the stationarity by kpss test
tseries::kpss.test(LDTSdata)


#creating the ACF and PACF plot
par(mfrow=c(1,2))
acf(diff(log10(TSdata)),main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(diff(log10(TSdata)),main="PACF plot")#PACF PLOT -- Auto Regressive or p

#Running the ARIMA model
ARIMAFit=arima((log10(TSdata)),c(1,1,0))

#getting mape(lower is better)
summary(ARIMAFit)

#Running the ARIMA using auto.arima, gives the best model fit 
ARIMAFit1= forecast::auto.arima(log10(TSdata),approximation=TRUE,trace=FALSE,allowdrift=FALSE)
summary(ARIMAFit1)


#?Predicting the future values
pred= predict(ARIMAFit1,n.ahead=36)
pred

#Ploting the observed data and forecasted data together

par(mfrow=c(1,1))
plot(TSdata,type="l",xlim=c(1,20),ylim=c(0,130000),xlab="months",ylab="Sales")
lines(10^(pred$pred),col="red")

#plotting the +-2 standard error to range of expected error
plot(TSdata,type="l",xlim=c(1,20),ylim=c(1,130000),xlab = "months",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue")
lines(10^(pred$pred-2*pred$se),col="black")
## then forecast the result
pred = predict(ARIMAFit1, n.ahead = 36)
write.csv(pred,"predictcase.csv")

## then do the exponential since you had used log earlier.
normal_result=10^pred$pred ## you get the desired result. 
write.csv(normal_result,"finalpredictcase.csv", row.names = FALSE)
plot(normal_result)
     
