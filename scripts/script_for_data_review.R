# this script contains all relevant code for data review document


## to emulate parametrized reports I will define a params df

params = data.frame(end_year = 2015, start_year = 1990)

end_year_for_filter = rep(params$start_year,2)


# prelimnary steps: libraries and dictionaries and country selection
library(tidyverse)
library(stringr)
library(stringi)
library(DT)
library(lubridate)
library(ineq)
library(knitr)


options(digits = 3)
load("./produced_data/cepal_19_countries")
load("./produced_data/cepal_33_countries")
# coi <-  cepal_19_countries$iso3c
coi <-  c("ARG", "BRA", "CHL")


# prepare tables re external debt
## make the tibble
load("./produced_data/data_with_basic_wrangling/cs_deuda_externa")
dext_to_gdp <- cs_deuda_externa %>% 
  filter(iso3c %in% coi) %>% 
  rename(deuda_ext_porc_pib = valor,
         year = Años) %>% 
  filter(year <= params$end_year) %>% 
  filter(str_detect(indicador, "porcentaje")) 

narr_dext_to_gdp <- dext_to_gdp %>% 
  select(iso3c, year, deuda_ext_porc_pib) 

mr_3_dext_to_gdp <- narr_dext_to_gdp %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  summarise(year_mr = max(year),
            deuda_ext_mr = last(deuda_ext_porc_pib),
            avg_mr_3 = mean(tail(deuda_ext_porc_pib, 3) , rm.na = TRUE)
  ) %>% 
  arrange(desc(deuda_ext_mr)) %>% 
  mutate(rnk_mr_dext = min_rank(-deuda_ext_mr),
         rnk_mr_3_dext = min_rank(-avg_mr_3)) %>% 
  arrange(iso3c)



## make the table
kb_mr_3_dext_to_gdp <- kable(mr_3_dext_to_gdp)

kb_dext_to_gdp <- kable(narr_dext_to_gdp)

dt_dext_to_gdp <- datatable(narr_dext_to_gdp)

## present the table


