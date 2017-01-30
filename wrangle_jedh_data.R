library(xts)
library(tidyverse)
library(countrycode)



load("./output/cepal_33_countries")
load("./output/debt_data_JEDH")

# convert curret character debt 1990 Q1, to standard 1990-01-01
debt_dates = as.yearqtr(debt_data$date)
debt_data$dateYQ = debt_dates
debt_dates = as.Date.yearqtr(debt_dates)
debt_data$date = as.Date(debt_dates, "%Y-%m-%d")

# group by indicator
indicator_first = debt_data %>% 
      group_by(indicator, country) %>% 
      summarise(count=n(), start=min(dateYQ), end=max(dateYQ)) %>% 
      arrange(indicator, count)

country_first = debt_data %>% 
      group_by(country, indicator) %>% 
      summarise(count=n(), start=min(dateYQ), end=max(dateYQ)) %>% 
      arrange(country, count)

bigger_countries_codes <- c("ARG", "BRA", "CHL", "COL", "MEX", "PER", "VEN")
bigger_countries_names <- countrycode(bigger_countries_codes,  "iso3c", "country.name")

# refine the country sample
foo <-  indicator_first %>% filter(country %in% bigger_countries_names)


