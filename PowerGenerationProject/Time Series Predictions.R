# Date: 6 Aug 2019
# Task: 1. Predict future values of flow using the past 50 years flow data (We tried
#          Box-Jenkins Method and fitting ARIMA model methods)
#       2. Compare the above two methods using RMSE

#Here, we try to use past water flow data to predict future flow data.
#We then use the predicted future flow to predict the future power generation

# Transfer the flow data to time series objects
short.date = strftime(flow$date, "%Y/%m")
year=substring(short.date,1,4)
month=substring(short.date,6,7)
aggr.stat = aggregate(flow$flow ~ month+year, FUN = sum)
dfrm <- aggr.stat[c("year", "month", "flow$flow")]
flowdata=ts(dfrm$`flow$flow`,frequency = 12,start=c(1969,12),end=c(2019,06))

# Load the package
library(forecast)

# Plot the time teries pattern
plot(flowdata)
acf(flowdata)
pacf(flowdata)

# Exponential Smoothing method
ses(flowdata,13)

# Holt's method
holt(flowdata,13)

################# Box-Jenkin method
forecast(flowdata,13)

# Present the table containing results of Box-Jenkins method
write.csv(forecast(flowdata,13), file="/Users/lilu/Desktop/dissertation 2/data/results_boxj.csv")
boxj<- read.csv("/Users/lilu/Desktop/dissertation 2/data/results_boxj.csv")
format(boxj, digits = 4)
boxj[,3] <- NULL
boxj[,3] <- NULL
boxj

# Load the package
library(Hmisc)
latex(boxj, file="")  

# Get subset of predicted data
flowdata2=ts(dfrm$`flow$flow`,frequency = 12,start=c(1969,12),end=c(2017,06))
forecastflow<-forecast(flowdata2,12)

# Plot the actual data and predicted data
plot(dfrm[572:583,3],type="l",ylim = c(0,1500),col="red",xlab="Time",ylab="River Flow", xaxt = "n") #real
lines(1:12,forecastflow$mean,col="blue") #prediction
axis(1,at=1:12,labels= c("Jul 2017","Aug 2017","Sep 2017","Oct 2017","Nov 2017","Dec 2017",
                         "Jan 2018","Feb 2018","Mar 2018","Apr 2018","May 2018","Jun 2018"))
legend("topleft",c("Real Data","Prediction Data"),cex=.8,col=c("red","blue"),lty=1:2)

# Write function of RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# Calculate RMSE of Box-Jenkins Approach
RMSE(c(374.3904,418.8751, 474.6043, 755.5532, 871.0379, 822.7324,937.6922, 761.7143, 758.1431, 730.5950, 460.0254, 352.5165),
     c(336.943,  348.775,  686.157,  527.270,  848.110, 1046.882,  631.771,506.370, 1378.157,
       844.333,  392.292,  249.461))
#245.8494

################## fit the ARIMA model
fit_arima <- auto.arima(flowdata)

# Simulate future data from the ARIMA model
simulate(refit, nsim = 40, future = T)

# Check the residuals
checkresiduals(fit_arima)

# Ljung-Box Test
Box.test(resid(fit_arima),type="Ljung",lag=20)

# Plot the prediction vs real data
plot(fitted(fit_arima),col="blue", ylab="River Flow") # plot prediction
lines(flowdata,col="red")
legend("topleft",c("Real Data","Prediction Data"),cex=.8,col=c("red","blue"),lty=1:2)

# Check the RMSE
refit <- Arima(flowdata, model=fit_arima)
accuracy(refit)

# One improvement on that is we could have divide the data into weekly data instead of 
# monthly data and this may result in a more accurate result for the time-series fitting