library(xts)
library(tidyverse)

load("./output/cepal_33_countries")
load("./output/debt_data_JEDH")

# convert curret character debt 1990 Q1, to standard 1990-01-01
debt_dates = as.yearqtr(debt_data$date)
debt_data$dateYQ = debt_dates
debt_dates = as.Date.yearqtr(debt_dates)
debt_data$date = as.Date(debt_dates, "%Y-%m-%d")

# group by indicator
foo = debt_data %>% 
      group_by(indicator, country) %>% 
      summarise(count=n(), start=min(dateYQ), end=max(dateYQ)) %>% 
      arrange(indicator, count)

moo = debt_data %>% 
      group_by(country, indicator) %>% 
      summarise(count=n(), start=min(dateYQ), end=max(dateYQ)) %>% 
      arrange(country, count)



