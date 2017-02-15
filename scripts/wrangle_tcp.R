library(tidyverse)

load("./produced_data/trab_cuenta_propia_jurgen")
load("./produced_data/cepal_20_countries")

TCP_tidy <-  TCP_2015 %>% 
  mutate( País = recode(País, 
                        Bolivia = "Bolivia (Estado Plurinacional de)",
                        "Rep. Dominicana" = "República Dominicana",
                        Venezuela = "Venezuela (República Bolivariana de)")) %>% 
  rename(nombre_pais = País)

TCP_for_20 <- TCP_tidy %>% filter(nombre_pais %in% cepal_20_countries[["country.name.es"]]) 

save(TCP_for_20, file = "./produced_data/TCP_tidy_20")

