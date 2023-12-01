library(forecast)
source('R/data_import.R')
source('R/model_ts_NEW.R')

#Based on the full data we got these models: hw1, hw2, m1, m2, m3, m4,m5, m6, m7, m8, r1, r2, r3, r4, r5, sub
#Now we retrain them and see how they do with forecasting

# Split the data into training and testing sets
Train_Test_Split <- function(tsObject) {
  n <- length(tsObject)
  train_size <- floor(0.9 * n)
  train_data <- head(tsObject, train_size)
  test_data <- tail(tsObject, n - train_size)
  return(list(train_data = train_data, test_data = test_data))
}

##################################
### Forecast for crime_ts WITHOUT predictors 

crime_data = Train_Test_Split(crime_ts)
train_data = crime_data$train_data
test_data = crime_data$test_data

# Model selection using auto.arima (information criterion default is AIC)
m8 = auto.arima(train_data, ic = "bic")   #Dont use autoarima, use the explicit models found on model_ts_NEW

# Make forecasts on the test data
forecast_result <- forecast(m8, h = length(test_data))

# Plot the results
plot(forecast_result, main = "ARIMA Forecast with Test Set")
lines(test_data, col = "blue", lty = 2)  # Add the test set to the plot with a dashed line
legend("topright", legend = c("Forecast", "Test Set"), col = c("black", "blue"), lty = c(1, 2))

# Evaluate the model (optional)
#First row is based on training data, second row is based on test data
accuracy(forecast_result, test_data)




