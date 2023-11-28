library(ggplot2)
source('R/data_import.R')

response_plot = ggplot(full_df, aes(x=DATE, y=n)) +
  geom_line() +theme_bw() +
  labs(x='Time',y='Number of Crimes') +
  ggtitle('Number of Crimes in DC by Month')
ggsave('plots/response_plot.pdf', plot=response_plot)

unemp_plot = ggplot(full_df, aes(x=DATE, y=DCURN)) +
  geom_line() +theme_bw() +
  labs(x='Time',y='Percent Unemployment') +
  ggtitle('Percent Unemployment in DC by Month')
ggsave('plots/unemp_plot.pdf', plot=unemp_plot)

housing_plot = ggplot(full_df, aes(x=DATE, y=WASH911BPPRIV)) +
  geom_line() +theme_bw() +
  labs(x='Time',y='Private Housing Permits in DC') +
  ggtitle('New Private Housing Units')
ggsave('plots/housing_plot.pdf', plot=housing_plot)

plot(decompose(crime_ts, type = 'addit'))
plot(decompose(crime_ts, type='mult'))

pdf('plots/acf_plots.pdf')
par(mfrow=c(2,2))
acf(crime_ts, main='DC Crime ACF')
acf(housing_ts, main = 'DC Private Housing ACF')
acf(unemp_ts, main = 'DC Unemployment ACF')
dev.off()

pdf('plots/pacf_plots.pdf')
par(mfrow=c(2,2))
pacf(crime_ts, main = 'DC Crime PACF')
pacf(housing_ts, main = 'DC Private Housing PACF')
pacf(unemp_ts, main = 'DC Unemployment PACF')
dev.off()

pdf('plots/ccf_plots.pdf')
par(mfrow=c(1,2))
ccf(housing_ts, crime_ts, type='correlation', main = 'Housing, Crime CCF')
ccf(unemp_ts, crime_ts, type='correlation', main = 'Unemployment, Crime CCF')
dev.off()

pdf('plots/scatterplot.pdf')
pairs(full_df[,2:4] %>% rename(Crime = n, Unemp = DCURN, Housing = WASH911BPPRIV),
      main='Scatterplot Between Response and Predictors')
dev.off()


