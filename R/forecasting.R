library(forecast)
source('R/data_import.R')
source('R/model_ts_NEW.R')

#Based on the full data we got these models: hw1, hw2, m1, m2, m3, m4,m5, m6, m7, m8, r1, r2, r3, r4, r5, sub
#Now we retrain them with 90% of the data and see how they do with forecasting

# Split the data into training and testing sets
Train_Test_Split <- function(tsObject) {
  n <- length(tsObject)
  train_size <- floor(0.90 * n)
  train_data <- head(tsObject, train_size)
  test_data <- tail(tsObject, n - train_size)
  return(list(train_data = train_data, test_data = test_data))
}

##################################
### Forecast for crime_ts WITHOUT predictors 

##############
### ARIMA
crime_data = Train_Test_Split(crime_ts)
train_data = crime_data$train_data
test_data = crime_data$test_data

# Fit the ARIMA model with drift
m8_train_data <- Arima(train_data, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 1), period = 12), include.drift = TRUE)
# Make forecasts on the test data
forecast_result <- forecast(m8_train_data, h = length(test_data))
# Plot the results
plot(forecast_result, main = "M8 ARIMA Forecast for Crime. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("topright", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2))
# Evaluate the model (optional). First row is based on training data, second row is based on test data
accuracy(forecast_result, test_data)

# Model selection using auto.arima (information criterion default is AIC)
auto_m = auto.arima(train_data, ic = "bic")   #Dont use autoarima, use the explicit models found on model_ts_NEW
forecast_result2 <- forecast(auto_m, h = length(test_data))

plot(forecast_result2, main = "Auto-ARIMA Forecast for Crime. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("topright", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2))
# Evaluate the model (optional). First row is based on training data, second row is based on test data
accuracy(forecast_result2, test_data)


##############
### Holt-Winters
hw_train = HoltWinters(train_data, seasonal = "multiplicative")
hw_train_p<-predict(hw_train,n.ahead=length(test_data), prediction.interval = TRUE, level=.95)
plot(hw_train, hw_train_p, main='H-W multiplicative Forecast for Crime. In-Sample')
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set", "95% CI"), col = c("red", "black",'lightblue'), lty = c(1, 2))
accuracy(hw_train_p, test_data)


##################################
### Forecast for crime_ts WTIH predictors
# Model selection using lag11 as a predictor
housing_data = Train_Test_Split(housing_ts)
housing_train_data = housing_data$train_data
housing_test_data = housing_data$test_data

unemp_data = Train_Test_Split(d_unemp_ts)
unemp_train_data = unemp_data$train_data
unemp_test_data = unemp_data$test_data

crime11_data = Train_Test_Split(lag11_crime_ts)
crime11_train_data = crime11_data$train_data
crime11_test_data = crime11_data$test_data

#ARIMA(0,1,0)(2,0,0)[12]
r5_train <- Arima(train_data, xreg=cbind(housing_train_data, unemp_train_data, crime11_train_data),
                  order = c(0, 1, 0), seasonal = list(order = c(2, 0, 0), period = 12), include.drift = TRUE)

forecast_result3 <- forecast(r5_train, h = length(test_data),xreg = cbind(housing_test_data, unemp_test_data, crime11_test_data))
plot(forecast_result3, main = "R5 ARIMA for Crime with predictors. In Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2))
# Evaluate the model (optional). First row is based on training data, second row is based on test data
accuracy(forecast_result3, test_data)


#######
### Out of sample predictions

year_forecast = forecast(m8, h = 12)
plot(year_forecast, main = "M8 ARIMA Forecast for Crime. Out-of-Sample (1yr)")

hw_p<-predict(hw2,n.ahead=12, prediction.interval = TRUE, level=.95)
plot(hw2, hw_p, main='H-W multiplicative Forecast for Crime. Out-of-Sample (1yr)')


#Estimate predictors using HW and then forecast
housing_hw<-HoltWinters(housing_ts,seasonal='mult')
housing_pred = predict(housing_hw,n.ahead=12)

unem_hw<-HoltWinters(d_unemp_ts) #,seasonal='mult')
unem_pred = predict(unem_hw,n.ahead=12)

lag11crime_hw<-HoltWinters(lag11_crime_ts,seasonal='mult')
lag11_crime_pred = predict(lag11crime_hw,n.ahead=12)

forecast_r5 <- forecast(r5, h = 12,xreg = cbind(housing_pred, unem_pred, lag11_crime_pred))
plot(forecast_r5, main = "R5 ARIMA Forecast for Crime with predictors. Out-of-Sample (1yr).")




