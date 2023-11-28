
library(data.table)
library(dplyr)

import_crime <- function() {
  
  filenames = list.files('data', pattern="Crime", full.names=TRUE)
  ldf = lapply(filenames, fread)
  df = bind_rows(ldf)
  
  df = df %>%
    mutate(year = substr(REPORT_DAT, 1,4),
           month = substr(REPORT_DAT, 6, 7))
  
  counts = df %>%
    group_by(year, month) %>%
    summarize(n = n()) %>%
    mutate(DATE = paste0(year, '-', month, '-', '01')) %>%
    filter(DATE < '2023-11-01') %>%
    ungroup() %>%
    select(DATE, n) %>%
    mutate(DATE = as.Date(DATE))
  
  return(counts)
  
}

import_housing <- function() {
  
  df = fread('data/WASH911BPPRIV.csv')
  df = df %>% filter(DATE > '2011-12-01') %>%
    mutate(DATE = as.Date(DATE))
  return(df)
  
}

import_unemp <- function() {
  
  df = fread('data/DCURN.csv')
  df = df %>% filter(DATE > '2011-12-01') %>%
    mutate(DATE = as.Date(DATE))
  return(df)
  
}

import_data <- function() {
  
  crime = import_crime()
  unemp = import_unemp()
  housing = import_housing()
  
  df = crime %>%
    left_join(., unemp, by = 'DATE') %>%
    left_join(., housing, by = 'DATE')
  
}

full_df = import_data()
crime_ts = ts(data = full_df$n, start = c(2012, 1), frequency = 12)
housing_ts = ts(data = full_df$WASH911BPPRIV, start = c(2012, 1), frequency = 12)
unemp_ts = ts(data = full_df$DCURN, start = c(2012, 1), frequency = 12)

#write.csv(x = full_df, file = 'data_processed/processed_data.csv', row.names = F)
