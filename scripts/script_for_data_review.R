# this script contains all relevant code for data review document


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
  filter(str_detect(indicador, "porcentaje")) %>% 
  rename(deuda_ext_porc_pib = valor,
         year = Años) 

narr_dext_to_gdp <- dext_to_gdp %>% 
  select(iso3c, year, deuda_ext_porc_pib)

## make the table
kb_dext_to_gdp <- kable(narr_dext_to_gdp)

dt_dext_to_gdp <- datatable(narr_dext_to_gdp)

## present the table


