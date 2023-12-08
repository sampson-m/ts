library(forecast)
library(tidyr)
library(dplyr)
source('R/model_ts_NEW.R')

#Based on the full data we got these models: hw1, hw2, m1, m2, m3, m4,m5, m6, m7, m8, r1, r2, r3, r4, r5, sub
#Now we retrain them with 90% of the data and see how they do with forecasting

# Split the data into training and testing sets
Train_Test_Split <- function(tsObject) {
  n <- length(tsObject)
  train_size <- floor(0.9 * n)   #IF THIS IS CHANGED TO 0.8 MODELS PERFORM BETTER
  train_data <- head(tsObject, train_size)
  test_data <- tail(tsObject, n - train_size)
  return(list(train_data = train_data, test_data = test_data))
}

##################################
### Forecast for crime_ts WITHOUT predictors 

### ARIMA
crime_data = Train_Test_Split(crime_ts)
train_data = crime_data$train_data
test_data = crime_data$test_data

# Fit the ARIMA model with drift
m8_train_data <- Arima(train_data, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 1), period = 12), include.drift = TRUE)
# Make forecasts on the test data
forecast_result <- forecast(m8_train_data, h = length(test_data))
# Evaluate the model. First row is based on training data, second row is based on test data
m8_90_accuracy = accuracy(forecast_result, test_data)

# Model selection using auto.arima
auto_m = auto.arima(train_data, ic = "bic")   #Maybe dont use autoarima, use the explicit models found on model_ts_NEW.
forecast_result2 <- forecast(auto_m, h = length(test_data))
# Evaluate the model
auto_arima90_accuracy = accuracy(forecast_result2, test_data)

##############
### Holt-Winters
hw_train = HoltWinters(train_data, seasonal = "multiplicative")
hw_train_p<-predict(hw_train,n.ahead=length(test_data), prediction.interval = TRUE, level=.95)
hw_90_accuracy = accuracy(hw_train_p, test_data)

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
# Evaluate the model. First row is based on training data, second row is based on test data
r5_90_accuracy = accuracy(forecast_result3, test_data)

## neural net with predictors:

unemp_nn_data = Train_Test_Split(unemp_ts)
unemp_nn_train = unemp_nn_data$train_data
unemp_nn_test = unemp_nn_data$test_data

nn_fit_train_pred = nnetar(train_data, xreg = cbind('housing' = housing_train_data,
                                               'unemp' = unemp_nn_train,
                                               'lag11_response' = crime11_train_data))

nn_fit_train_no_pred = nnetar(train_data)
checkresiduals(nn_fit_train_no_pred)

nn_forecast_pred = forecast(nn_fit_train_pred,PI=T, xreg=cbind('housing' = housing_test_data,
                                                       'unemp' = unemp_nn_test,
                                                       'lag11_response' = crime11_test_data))
nn_forecast_no_pred = forecast(nn_fit_train_no_pred,PI=T)
nn_accuracy_90_pred = accuracy(nn_forecast_pred, test_data)
nn_accuracy_90_no_pred = accuracy(nn_forecast_no_pred, test_data)

pdf('plots/nnar_90_insample_no_pred.pdf')
plot(nn_forecast_no_pred, main = "NNAR without preds. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2),cex = 0.7)  
dev.off()

pdf('plots/nnar_90_insample_with_pred.pdf')
plot(nn_forecast_pred, main = "NNAR with preds. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2),cex = 0.7)  
dev.off()

#Model accuracy
m8_90_accuracy
auto_arima90_accuracy
hw_90_accuracy
r5_90_accuracy

# Create a results table
models <- c("M8 90% of data", "Auto Arima 90% of data", "R5 90% of data", 'NNAR-pred 90% of data', 'NNAR-no pred 90% of data')
metrics <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Theil's U")

df <- data.frame(Model = rep(models, each = length(metrics)),
                 Metric = rep(metrics, length(models)),
                 Value = c(m8_90_accuracy[2,], auto_arima90_accuracy[2,], r5_90_accuracy[2,],
                           nn_accuracy_90_pred[2,], nn_accuracy_90_no_pred[2,]))
# Pivot the data
results_table <- df %>%
  pivot_wider(names_from = Metric, values_from = Value)
# Performance table for test set
print(results_table)

### Repeating code down here for 80% split. not sure where the rest is

Train_Test_Split <- function(tsObject) {
  n <- length(tsObject)
  train_size <- floor(0.8 * n)   #IF THIS IS CHANGED TO 0.8 MODELS PERFORM BETTER
  train_data <- head(tsObject, train_size)
  test_data <- tail(tsObject, n - train_size)
  return(list(train_data = train_data, test_data = test_data))
}

### ARIMA
crime_data = Train_Test_Split(crime_ts)
train_data = crime_data$train_data
test_data = crime_data$test_data

# Fit the ARIMA model with drift
m8_train_data <- Arima(train_data, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 1), period = 12), include.drift = TRUE)
# Make forecasts on the test data
forecast_result <- forecast(m8_train_data, h = length(test_data))
# Evaluate the model. First row is based on training data, second row is based on test data
m8_80_accuracy = accuracy(forecast_result, test_data)

# Model selection using auto.arima
auto_m = auto.arima(train_data, ic = "bic")   #Maybe dont use autoarima, use the explicit models found on model_ts_NEW.
forecast_result2 <- forecast(auto_m, h = length(test_data))
# Evaluate the model
auto_arima80_accuracy = accuracy(forecast_result2, test_data)

##############
### Holt-Winters
hw_train = HoltWinters(train_data, seasonal = "multiplicative")
hw_train_p<-predict(hw_train,n.ahead=length(test_data), prediction.interval = TRUE, level=.95)
hw_80_accuracy = accuracy(hw_train_p, test_data)

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
# Evaluate the model. First row is based on training data, second row is based on test data
r5_80_accuracy = accuracy(forecast_result3, test_data)

## neural net with predictors:

unemp_nn_data = Train_Test_Split(unemp_ts)
unemp_nn_train = unemp_nn_data$train_data
unemp_nn_test = unemp_nn_data$test_data

nn_fit_train_pred = nnetar(train_data, xreg = cbind('housing' = housing_train_data,
                                                    'unemp' = unemp_nn_train,
                                                    'lag11_response' = crime11_train_data))

nn_fit_train_no_pred = nnetar(train_data)
checkresiduals(nn_fit_train_no_pred)

nn_forecast_pred = forecast(nn_fit_train_pred,PI=T, xreg=cbind('housing' = housing_test_data,
                                                               'unemp' = unemp_nn_test,
                                                               'lag11_response' = crime11_test_data))
nn_forecast_no_pred = forecast(nn_fit_train_no_pred,h = length(test_data),PI=T)
nn_accuracy_80_pred = accuracy(nn_forecast_pred, test_data)
nn_accuracy_80_no_pred = accuracy(nn_forecast_no_pred, test_data)

pdf('plots/nnar_80_insample_no_pred.pdf')
plot(nn_forecast_no_pred, main = "NNAR without preds. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2),cex = 0.7)  
dev.off()

pdf('plots/nnar_80_insample_with_pred.pdf')
plot(nn_forecast_pred, main = "NNAR with preds. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2),cex = 0.7)  
dev.off()


# Create a results table
models <- c("M8 80% of data", "Auto Arima 80% of data", "R5 80% of data", 'NNAR-pred 80% of data', 'NNAR-no pred 80% of data')
metrics <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Theil's U")

df <- data.frame(Model = rep(models, each = length(metrics)),
                 Metric = rep(metrics, length(models)),
                 Value = c(m8_80_accuracy[2,], auto_arima80_accuracy[2,], r5_80_accuracy[2,],
                           nn_accuracy_80_pred[2,], nn_accuracy_80_no_pred[2,]))
# Pivot the data
results_table <- df %>%
  pivot_wider(names_from = Metric, values_from = Value)
# Performance table for test set
print(results_table)


# Set up a 2x2 layout for subplots
par(mfrow = c(2, 2))
# Plot the first subplot
plot(forecast_result, main = "ARIMA without preds. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), 
       lty = c(1, 2),cex = 0.7)  
# Plot the second subplot
plot(forecast_result2, main = "Auto-ARIMA without preds. In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2),cex = 0.7)  
# Plot the third subplot
plot(hw_train, hw_train_p, main='H-W multiplicative. In-Sample')
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("red", "black"), lty = c(1, 2),cex = 0.7)  
# Plot the fourth subplot
plot(forecast_result3, main = "ARIMA with preds.In-Sample")
lines(test_data, col = "black", lty = 2)  # Add the test set to the plot with a dashed line
legend("bottomleft", legend = c("Forecast", "Test Set"), col = c("blue", "black"), lty = c(1, 2),cex = 0.7)  
# Reset the layout to the default (1x1)
par(mfrow = c(1, 1))

########
### Out of sample predictions using the models trained in model_ts_NEW.R

year_forecast = forecast(m8, h = 12)

hw_p<-predict(hw2,n.ahead=12, prediction.interval = TRUE, level=.95)

#Estimate predictors using HW and then forecast
housing_hw<-HoltWinters(housing_ts,seasonal='mult')
housing_pred = predict(housing_hw,n.ahead=12)

unem_hw<-HoltWinters(d_unemp_ts) #,seasonal='mult')
unem_pred = predict(unem_hw,n.ahead=12)

lag11crime_hw<-HoltWinters(lag11_crime_ts,seasonal='mult')
lag11_crime_pred = predict(lag11crime_hw,n.ahead=12)

forecast_r5 <- forecast(r5, h = 12,xreg = cbind(housing_pred, unem_pred, lag11_crime_pred))

## nnar forecasts

nn_test = nnetar(crime_ts, p=1, P=2, size = 5)
nn_test
nn_test = nnetar(crime_ts, p=1, P=2, size = 20)
nn_test

nn_test1 = nnetar(crime_ts, p=1, P=3, size = 3, xreg = cbind(housing_ts,
                                                              unemp_ts,
                                                              lag11_crime_ts))

plot(nn_test1$fitted)

forecast_test = forecast(nn_test1, PI=T, xreg = cbind(housing_pred,
                                                      unemp_nn_pred,
                                                      lag11_crime_pred))
plot(forecast_test)

nn_test1

unemp_hw = HoltWinters(unemp_ts, seasonal = 'mult')
unemp_nn_pred = predict(unemp_hw, n.ahead=12)

forecast_nn1 = forecast(nn_test, PI=T, h=12)

forecast_nn2 = forecast(nnfit2, PI=T, xreg = cbind(housing_pred,
                                             unemp_nn_pred,
                                             lag11_crime_pred))

plot(forecast_nn1, main = "NNAR without preds.Out-of-Sample (1yr)")
plot(forecast_nn2, main = "NNAR with preds.Out-of-Sample (1yr)")



# Set up a 2x2 layout for subplots
pdf('plots/out_of_sample_pred.pdf')
par(mfrow = c(2, 2))

# Plot the first subplot
plot(year_forecast, main = "ARIMA no preds.Out-of-Sample (1yr)")

# Plot the second subplot
plot(hw2, hw_p, main='H-W mult. Out-of-Sample (1yr)')

# Plot the third subplot
plot(forecast_r5, main = "Arima with preds. Out-of-Sample (1yr)") 

# Plot the fourth subplot 

plot(forecast_nn1, main = "NNAR no preds.Out-of-Sample (1yr)")
dev.off()
# Reset the layout to the default (1x1)
# par(mfrow = c(1, 1))



