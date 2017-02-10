library(tidyverse)

load("./produced_data/trab_cuenta_propia_jurgen")
load("./produced_data/cepal_names_es_en_20")

TCP_tidy <-  TCP_2015 %>% 
  mutate( País = recode(País, 
                        Bolivia = "Bolivia (Estado Plurinacional de)",
                        "Rep. Dominicana" = "República Dominicana",
                        Venezuela = "Venezuela (República Bolivariana de)")) %>% 
  rename(country_name = País)

TCP_for_20 <- TCP_tidy %>% filter(country_name %in% cepal_names_es_en_20[["country.name.es"]]) 

save(TCP_for_20, file = "./produced_data/TCP_tidy_20")

