library(tibble)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

load("./produced_data/data_with_basic_wrangling/WDI_Data_33_tidy")

risk <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "isk") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()
risk_ic <- growth$indicator_code[c(7)]
risk_in <- growth$indicator_name[c(7)]

growth <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "rowth") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()
growth_ic <- growth$indicator_code[c(6:24)]
growth_in <- growth$indicator_name[c(6:24)]


cred <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "redit") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()

cred_ic <- cred$indicator_code[c(4,5,6)]
cred_in <- cred$indicator_name[c(4,5,6)]

claims <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "laims") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()

claims_ic <- claims$indicator_code[c(2,3)]
claims_in <- claims$indicator_name[c(2,3)]


debt <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "ebt") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()

debt_ic <- debt$indicator_code[
  c(7, 8, 9, 10, 11, 12, 13, 34,
    41, 42, 44, 49, 50, 51, 57, 58, 81, 82, 83, 88)]

debt_in <- debt$indicator_name[
  c(7, 8, 9, 10, 11, 12, 13, 34,
    41, 42, 44, 49, 50, 51, 57, 58, 81, 82, 83, 88)]

final <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "inal") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()

gdp <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "GDP") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()
gdp_ic <- gdp$indicator_code[
  c(11:12, 20:25, 27:32, 45, 49:54, 58, 59, 60, 65, 67, 68, 71, 72, 76, 77, 79:82)]
gdp_in <- gdp$indicator_name[
  c(11:12, 20:25, 27:32, 45, 49:54, 58, 59, 60, 65, 67, 68, 71, 72, 76, 77, 79:82)]

central <- WDI_Data_33_tidy %>% 
  filter(str_detect(indicator_name, "entral") ) %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()

  
selection_codes = c(claims_ic, debt_ic, cred_ic, gdp_ic, growth_ic, risk_ic)
selection_names = c(claims_in, debt_in, cred_in, gdp_in, growth_in, risk_in)


WDI_33_selected_vars <-  WDI_Data_33_tidy %>% 
  filter(indicator_code %in% selection_codes)

save(WDI_33_selected_vars, 
     file = "./produced_data/data_with_basic_wrangling/WDI_33_selected_all")


code_names_in_WDI_33_sel_vars <-  WDI_33_selected_vars %>% 
  select(indicator_name, indicator_code) %>% 
  distinct()

iso2countries <- unique(WDI_33_selected_vars$iso2c)

# # use code_names_in_WDI_33_selected_vars$indicator_code and 
# # iso2countries to retrive updated versions of the data
# 
# fresh_download <- wb(country = iso2countries,
#                      indicator = code_names_in_WDI_33_sel_vars$indicator_code)


