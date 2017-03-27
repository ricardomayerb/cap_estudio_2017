library(wbstats)
library(tidyverse)
library(countrycode)
load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_19_countries")

source("./functions/add_iso.R")
library(WDI)

library(readr)

WDI_Data <- read_csv("./raw_data/WDI_Data.csv", col_types = 
                       cols(.default = col_double(),
                            `Country Name` = col_character(),
                            `Country Code` = col_character(),
                            `Indicator Name` = col_character(),
                            `Indicator Code` = col_character()
                       )
)

names(WDI_Data)[[1]] <- "country_name"
names(WDI_Data)[[2]] <- "iso3c"
names(WDI_Data)[[3]] <- "indicator_name"
names(WDI_Data)[[4]] <- "indicator_code"

WDI_Data_33_tidy <-   WDI_Data %>% 
  filter(iso3c %in% cepal_33_countries[["iso3c"]]) %>% 
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c")) %>% 
  gather(key = year, value = value,  -c(country_name, iso3c, iso2c, indicator_name, indicator_code))

WDI_Data_19_tidy <-   WDI_Data %>% 
  filter(iso3c %in% cepal_19_countries[["iso3c"]]) %>% 
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c")) %>% 
  gather(key = year, value = value,  -c(country_name, iso3c, iso2c, indicator_name, indicator_code))


save(WDI_Data_33_tidy, 
     file = "./produced_data/data_with_basic_wrangling/WDI_Data_33_tidy")

save(WDI_Data_19_tidy, 
     file = "./produced_data/data_with_basic_wrangling/WDI_Data_19_tidy")


rm(WDI_Data)
