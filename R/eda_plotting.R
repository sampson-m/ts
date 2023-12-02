library(ggplot2)
source('R/data_import.R')
source('R/functions.R')

plot_acf()
plot_pacf()
plot_ccf()

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

housing_plot = ggplot(full_df, aes(x=DATE, y=lag11_crime)) +
  geom_line() +theme_bw() +
  labs(x='Time',y='Lag 11 Crime in DC') +
  ggtitle('Lag 11 Number of Crimes in DC by Month')
ggsave('plots/lag11_crime.pdf', plot=housing_plot)

pdf('plots/decomposition.pdf')
plot(decompose(crime_ts, type='mult'))
dev.off()

pdf('plots/scatterplot.pdf')
pairs(full_df[,2:5] %>% rename(Crime = n, Unemp = DCURN, Housing = WASH911BPPRIV, Lag_Crime = lag11_crime))
dev.off()


