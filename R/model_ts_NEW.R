
library(TSA)
library(tseries)
library(astsa)
library(forecast)
source('R/data_import.R')

###############################################################################
## Best model for crime_ts is model r5 with three predictors d_unemp_ts 
## (differenced unemployment variable), housing_ts (housing permit variable),
## and lag11_crime_ts (lag11 crime variable). 

## Adequate models: multiplicative Holt-Winters (hw2) and r5
## Inadequate model: ARIMA model without predictors (m8)

## Lowest (aka best) AIC, BIC, sigma2 values: m8
## Worst sigma2 values: hw2
###############################################################################


###############################################################################
## Find a best fit model for crime_ts WITHOUT predictors as a baseline

## SUMMARY: best model for crime_ts WITHOUT predictors is m8 or 
## ARIMA(1,0,0)(0,1,1)[12] with drift based on lowest AIC and BIC values and
## plot; m8 is not 100% adequate (one significant residual) but other 
## potential best fit models were also not 100% adequate

# Test Assumptions (time series must be stationary)
adf.test(crime_ts) # null is time series is NOT stationary;
# p-value = .01 < .05 so reject null and find crime_ts is stationary (we do
# not need to difference our data)
pp.test(crime_ts) # confirms that crime_ts is stationary

# Examine ACF and PACF to determine ARMA/ARIMA model order
acf(crime_ts, lag.max = 60) # appears to have decaying trend -- hard to tell
pacf(crime_ts, lag.max = 60) # appears to have decaying trend -- hard to tell

# Model selection using BIC 
sub = armasubsets(crime_ts, nar = 5, nma = 5)
plot(sub) # appears best modeled by AR(1), AR(4), AR(5), MA(2), ARMA(1,2),
# ARMA(4,2), or ARMA(5,2). 

m1 = arima(crime_ts, order = c(1,0,0))
m2 = arima(crime_ts, order = c(4,0,0))
m3 = arima(crime_ts, order = c(5,0,0))
m4 = arima(crime_ts, order = c(0,0,2))
m5 = arima(crime_ts, order = c(1,0,2))
#m6 = arima(crime_ts, order = c(4,0,2)) # fails because not stationary
m7 = arima(crime_ts, order = c(5,0,2))

par(mfrow = c(2,3))
acf(m1$residuals) # inadequate
acf(m2$residuals) # inadequate
acf(m3$residuals) # inadequate (just at lag 1)
acf(m4$residuals) # inadequate
acf(m5$residuals) # inadequate
acf(m7$residuals) # inadequate (just at lag 1)

# Model selection using auto.arima (information criterion default is AIC)
m8 = auto.arima(crime_ts, ic = "bic") 
m8 # "best" model is ARIMA(1,0,0)(0,1,1)[12] with drift 
acf(m8$residuals) # we see one residual is significant at lag 0.9167

# Compare contending models based off residuals, BIC, plots
par(mfrow = c(2,2))
par(cex.lab=1, cex=0.7, cex.axis = .9)
acf(m3$residuals, main = "AR(5)") 
acf(m7$residuals, main = "ARMA(5,2)")
acf(m8$residuals, main = "ARIMA(1,0,0)(0,1,1)[12] with drift")

AIC_values = as.data.frame(rbind(AIC(m3), AIC(m7), AIC(m8)))
BIC_values = as.data.frame(rbind(BIC(m3), BIC(m7), BIC(m8)))
performance = cbind(AIC_values, BIC_values)
rownames(performance) = c("m3", "m7", "m8")
colnames(performance) = c("AIC", "BIC")
performance

par(mfrow = c(2,2))
par(cex.lab=1, cex=0.7, cex.axis = .9)
plot(crime_ts, main = "AR(5)", ylab = "crime")
lines(fitted(m3), col = "red")
plot(crime_ts, main = "ARMA(5,2)", ylab = "crime")
lines(fitted(m7), col = "red")
plot(crime_ts, main = "ARIMA(1,0,0)(0,1,1)[12] with drift", ylab = "crime")
lines(fitted(m8), col = "red")
###############################################################################


###############################################################################
## Find a best fit model for crime_ts WITH predictors

## SUMMARY: best model for crime_ts WITH predictors is r5 or 
## ARIMA(0,1,0)(2,0,0)[12] based on lowest AIC and BIC values and
## plot; r5 is 100% adequate and uses our two predictors plus lag11_crime_ts
## as a third predictor

# Test Assumptions (time series must be stationary)
adf.test(housing_ts) # null is time series is NOT stationary;
# p-value = .01 < .05 so reject null and find housing_ts is stationary (we do
# not need to difference our data)
pp.test(housing_ts) # confirms that crime_ts is stationary

adf.test(unemp_ts) # null is time series is NOT stationary;
# p-value = .31 > .05 so reject null and find housing_ts is NOT stationary
pp.test(unemp_ts) # confirms that unemp_ts is NOT stationary
d_unemp_ts = diff(unemp_ts)
adf.test(diff(d_unemp_ts)) # p-value = .01 < .05 so reject null and find 
# d_unemp_ts is stationary
pp.test(diff(d_unemp_ts)) # confirms that d_unemp_ts is stationary

# Compare different ARIMA models with increasing complexity where I 
# pick the order
r1 = arima(crime_ts, order=c(1,0,0), xreg=cbind(housing_ts, d_unemp_ts))
acf(r1$residuals[-(1)]) # to get rid of NAs; inadequate

r2 = arima(crime_ts, order=c(0,0,1), xreg=cbind(housing_ts, d_unemp_ts))
acf(r2$residuals[(-1)]) # to get rid of NAs; inadequate

r3 = arima(crime_ts, order=c(1,0,1), xreg=cbind(housing_ts, d_unemp_ts))
acf(r3$residuals[(-1)]) # to get rid of NAs; inadequate

# Model selection using auto.arima (information criterion default is AIC)
r4 = auto.arima(crime_ts, xreg=cbind(housing_ts, d_unemp_ts), ic = "bic") 
AIC(r4)
BIC(r4)

par(mfrow = c(1,2))
par(cex.lab=1, cex=0.7, cex.axis = .9)
plot(crime_ts, main = "ARIMA(0,1,0)(2,0,0)[12]", ylab = "crime")
lines(fitted(r4), col = "red")
acf(r4$residuals[(-1)], main = "ACF of ARIMA(0,1,0)(2,0,0)[12]") 
# to get rid of NAs; inadequate (just at lag 11)

# Model selection using lag11 as a predictor
r5 = auto.arima(crime_ts, xreg=cbind(housing_ts, d_unemp_ts, lag11_crime_ts), 
                ic = "bic") 
AIC(r5)
BIC(r5)

par(mfrow = c(1,2))
par(cex.lab=1, cex=0.7, cex.axis = .9)
plot(crime_ts, main = "ARIMA(0,1,0)(2,0,0)[12]", ylab = "crime")
lines(fitted(r5), col = "red")
acf(r5$residuals[(-1)], main = "ACF of ARIMA(0,1,0)(2,0,0)[12]") 
# to get rid of NAs; inadequate (just at lag 11)
###############################################################################


###############################################################################
## Compare best model WITHOUT predictors vs best model WITH predictors; also
## Compare to Holt Winters

## SUMMARY: best model WITHOUT predictors has higher performance than
## best model WITH predictors (based on AIC and BIC values); however, model
## WITH predictors is adequate and model WITHOUT predictors is not; both
## ARIMA models have far lower sigma^2 than Holt-Winters model; Holt-Winters
## is adequate

# Holt-Winters
hw1 = HoltWinters(crime_ts)
hw2 = HoltWinters(crime_ts, seasonal = "multiplicative")
hw1$SSE
hw2$SSE # lower SSE value so go with multiplicative

# Compare performance of model with predictors to model without predictors
performance[4,c(1,2)] = c(AIC(r5), BIC(r5))
performance = performance[c(3,4),]
performance$sigma2 = c(m8$sigma2, r5$sigma2)
performance[3,c(1,2,3)] = c(NA, NA, hw2$SSE)
rownames(performance) = c("Model Without Predictors", 
                          "Model With Predictors", "Holt-Winters")
performance

# Compare HW model with predictors to model without predictors plots
par(mfrow = c(2,3))
par(cex.lab=1, cex=0.35, cex.axis = .9)
plot(hw2, main = "Holt-Winters", 
     sub = "alpha = .8, beta = 0, gamma = .7",
     ylab = "crime")
plot(crime_ts, main = "ARIMA Without Pred.", 
     sub = "ARIMA(1,0,0)(0,1,1)[12] with drift",
     ylab = "crime")
lines(fitted(m8), col = "red")
plot(crime_ts, main = "ARIMA With Pred.", 
     sub = "ARIMA(0,1,0)(2,0,0)[12]", 
     ylab = "crime")
lines(fitted(r5), col = "red")
acf(residuals(hw2), main = "Residuals Holt-Winters",
    sub = "alpha = .8, beta = 0, gamma = .7")
acf(m8$residuals, main = "Residuals ARIMA Without Pred.", 
    sub = "ARIMA(1,0,0)(0,1,1)[12] with drift")
acf(r5$residuals[(-1)], main = "Residuals ARIMA With Pred.", 
    sub = "ARIMA(0,1,0)(2,0,0)[12]") 
###############################################################################