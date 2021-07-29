# Upload the Dataset
data <- read.csv('/Users/lilu/Desktop/Electricity_demand_data.csv')
data$demand <- as.numeric(data$demand)
summary(data)

# Add daylight column to the dataset (Note: daylight = hours before sunset - hours before sunrise)
new_data <- data
new_data[,'daylight'] <- new_data$hours.before.sunset - new_data$hours.before.sunrise

# Plot Correlation Matrix
library(ggcorrplot)
ggcorrplot(round(cor(new_data[, c(2,5,6)], use = "complete.obs"),2), hc.order = FALSE, type = "lower",
           lab = TRUE,lab_size = 2)
# Barring the non significant coeffcients
pmat <- cor_pmat(new_data[, c(2,5,6)], use = "complete.obs")
ggcorrplot(round(cor(new_data[, c(2,5,6)], use = "complete.obs"),2), hc.order = FALSE, type = "lower",p.mat = pmat)

# Plot the trend
mymatrix1 <- t(matrix(new_data$temperature, nrow=1095, byrow = TRUE))
meanTemp = colMeans(mymatrix1)
mymatrix2 <- t(matrix(new_data$demand, nrow=1095, byrow = TRUE))
DailyDemand = colSums(mymatrix2)
mymatrix3 <- t(matrix(new_data$daylight, nrow=1095, byrow = TRUE))
Dailydaylight = colMeans(mymatrix3)
plot(meanTemp, DailyDemand)
plot(Dailydaylight, DailyDemand)


## Prediction Methods 

# Load the package
library(forecast)

# Transfer the data to time series objects
tsdata=ts(data$demand[!is.na(data$demand)])

# Plot the time teries patterns
plot(tsdata)
acf(tsdata)
pacf(tsdata)

####### Method 1: Exponential Smoothing method
ses(tsdata,4320)$model

####### Method 2: Holt's method
holt(tsdata,4320)$model

####### Method 3: Autofit the ARIMA model
fit_arima <- auto.arima(tsdata)
summary(fit_arima)

####### Method 4: Autofit Arima with regression 
# auto arima xreg
dataTrain <- new_data[!is.na(data$demand),]
dataTest <- new_data[is.na(data$demand),]
tsdata_Train <- ts(dataTrain$demand)
# use hours before sunrise and hours before sunset
fit_new <- auto.arima(tsdata_Train, xreg = as.matrix(dataTrain[,2:4]))
summary(fit_new)

# just use daylight duration
fit_new_1 <- auto.arima(tsdata_Train, xreg = as.matrix(dataTrain[,c(2,6)]))
summary(fit_new_1)

# Temp, Daylight Duration, and fourier terms with frequency 48, found that 13 fourier terms is the best
fit <- list(aicc=Inf)
Demand_new=ts(dataTrain$demand, frequency = 48)
for(i in 1:25)
{
  fit <- auto.arima(Demand_new, xreg=cbind(fourier(Demand_new, K=i),as.matrix(dataTrain[,2:4])), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else 
    break;
}

fit_final_sim <- simulate(bestfit, xreg = cbind(fourier(Demand_new, K=13, 4320),as.matrix(dataTest[,c(2,6)])),nsim = 4320)

# Check the residuals
checkresiduals(bestfit)

# Ljung box test
Box.test(resid(bestfit),type="Ljung",lag=20)

# Plot the prediction vs real data
plot(fitted(bestfit),col="blue", ylab="time series data") # plot prediction
lines(Demand_new,col="red")
legend("topleft",c("Real Data","Prediction Data"),cex=.8,col=c("red","blue"),lty=1:2)

# Check the RMSE
accuracy(bestfit)


# Export predicted data
write.xlsx(fit_final_sim, "fit_final_sim.xlsx")


