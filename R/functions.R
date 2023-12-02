plot_acf <- function(){
  
  pdf('plots/response_acf.pdf')
  acf(crime_ts, main='DC Crime ACF')
  dev.off()
  pdf('plots/housing_acf.pdf')
  acf(housing_ts, main = 'DC Private Housing ACF')
  dev.off()
  pdf('plots/unemp_acf.pdf')
  acf(unemp_ts, main = 'DC Unemployment ACF')
  dev.off()
  pdf('plots/lag11_acf.pdf')
  acf(lag11_crime_ts, main='Lag 11 DC Crime ACF')
  dev.off()
  
}

plot_pacf <- function() {
  pdf('plots/pacf_response.pdf')
  pacf(crime_ts, main = 'DC Crime PACF')
  dev.off()
  pdf('plots/housing_pacf.pdf')
  pacf(housing_ts, main = 'DC Private Housing PACF')
  dev.off()
  pdf('plots/unemp_pacf.pdf')
  pacf(unemp_ts, main = 'DC Unemployment PACF')
  dev.off()
  pdf('plots/lag11_pacf.pdf')
  pacf(lag11_crime_ts, main='Lag 11 DC Crime PACF')
  dev.off()
}

plot_ccf <- function(){
  pdf('plots/housing_ccf.pdf')
  ccf(housing_ts, crime_ts, type='correlation', main = 'Housing, Crime CCF')
  dev.off()
  pdf('plots/unemp_ccf.pdf')
  ccf(unemp_ts, crime_ts, type='correlation', main = 'Unemployment, Crime CCF')
  dev.off()
  pdf('plots/lag11_ccf.pdf')
  ccf(lag11_crime_ts, crime_ts, type='correlation', main='Lag 11 Crime, Crime CCF')
  dev.off()
}












