library(tidyverse)
library(countrycode)

load("./produced_data/trab_cuenta_propia_jurgen")
load("./produced_data/cepal_20_countries")

TCP_tidy <-  TCP_2015 %>% 
  mutate( País = recode(País, 
                        Bolivia = "Bolivia (Estado Plurinacional de)",
                        "Rep. Dominicana" = "República Dominicana",
                        Venezuela = "Venezuela (República Bolivariana de)")) %>% 
  rename(nombre_pais = País) %>% 
  mutate(
    iso2c = countrycode(nombre_pais, "country.name.es",
                        "iso2c", custom_dict = cepal_20_countries),
    iso3c = countrycode(nombre_pais, "country.name.es",
                        "iso3c", custom_dict = cepal_20_countries))

TCP_tidy_20 <- TCP_tidy %>% filter(nombre_pais %in% cepal_20_countries[["country.name.es"]]) 

save(TCP_tidy_20, TCP_tidy, file = "./produced_data/TCP_tidy")

