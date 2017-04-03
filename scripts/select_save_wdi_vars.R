library(tibble)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

load("./produced_data/data_with_basic_wrangling/WDI_Data_33_tidy")

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


sel_codes = c(claims_ic, debt_ic, cred_ic)
sel_names = c(claims_in, debt_in, cred_in)



WDI_33_selected_vars <-  WDI_Data_33_tidy %>% 
  filter(indicator_code %in% sel_codes) %>% 
  mutate(date = as.Date(as.character(year), format = "%Y"))

save(WDI_33_selected_vars, 
     file = "./produced_data/data_with_basic_wrangling/WDI_33_selected_all")


claims_on_cgov = WDI_33_selected_vars %>%
  filter(indicator_code == "FS.AST.CGOV.GD.ZS")

claims_on_others = WDI_33_selected_vars %>%
  filter(indicator_code == "FS.AST.DOMO.GD.ZS")



