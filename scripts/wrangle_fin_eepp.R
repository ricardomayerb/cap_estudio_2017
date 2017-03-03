library(tidyverse)
library(stringr)
library(countrycode)

load("./produced_data/fin_eepp")
load("./produced_data/cepal_20_countries")
load("./produced_data/cepal_33_countries")


fin_eepp_tidy <- fin_eepp %>% 
  mutate(country_name = str_to_title(country)) %>% 
  mutate(country_name = recode(country_name,
                               Bolivia = "Bolivia (Plurinational State of)",  
                               Brasil = "Brazil",
                               Costarica = "Costa Rica",
                               Elsalvador = "El Salvador",
                               Repdominicana = "Dominican Republic",
                               Trinidad = "Trinidad and Tobago"),
         iso3c = countrycode(country_name, "country.name.en", "iso3c" ),
         iso2c = countrycode(country_name, "country.name.en", "iso2c" )
        ) %>% 
  select(-country) %>% 
  filter(!is.na(year))

fin_eepp_tidy_20 <- filter(
    fin_eepp_tidy, iso2c %in% cepal_20_countries[["iso2c"]]) 

save(fin_eepp_tidy, fin_eepp_tidy_20, file = "./produced_data/fin_eepp_tidy")
