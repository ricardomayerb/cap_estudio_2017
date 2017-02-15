library(tidyverse)
library(stringr)

load("./produced_data/datos_mon_finan_alej_messy")
load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_20_countries")


## tidying tasa de politica moetaria
country_names_tpm_mess <- names(tpm)[-1]
tmp_dates <- seq.Date(from = as.Date("1986-01-01"), to = as.Date("2016-10-01"), by = 'month')
tpm_tidy <-  tpm %>% 
        mutate(Col1 = tmp_dates) %>%
        gather(key = pais_name, value = tasa_politica_monetaria, -Col1) %>% 
        rename(date = Col1) %>%
        mutate( pais_name = recode(pais_name, 
                             Antigua.y.Barbuda = "Antigua y Barbuda",
                             Bolivia = "Bolivia (Estado Plurinacional de)",
                             Costa.Rica = "Costa Rica",
                             El.Salvador = "El Salvador",
                             República.Dominicana = "República Dominicana",
                             Saint.Kitts.y.Nevis = "Saint Kitts y Nevis",
                             San.Vicente.y.las.Ganadinas = "San Vicente y las Granadinas",
                             Santa.Lucía = "Santa Lucía",
                             Trinidad.y.Tabago = "Trinidad y Tabago",
                             Venezuela = "Venezuela (República Bolivariana de)"))

tpm_tidy_20 <- tpm_tidy %>% 
          filter(pais_name %in% cepal_20_countries[["country.name.es"]])


## tidyng cartera vencida
### separate Col1 into year and month
country_names_cv_mess <- names(cartera_vencida)[-1]

cartera_vencida_tidy <- cartera_vencida %>% 
                      separate(col=Col1, into=c('year', 'month'), sep='-M') %>% 
                      mutate(year = as.numeric(year), month = as.numeric(month)) %>%
                      gather(key=pais_name, value=cartera_vencida_percent, -year, -month) %>% 
                      mutate( pais_name = recode(pais_name, 
                                                 Antigua.y.Barbuda = "Antigua y Barbuda",
                                                 Bolivia = "Bolivia (Estado Plurinacional de)",
                                                 Costa.Rica = "Costa Rica",
                                                 El.Salvador = "El Salvador",
                                                 Rep..Dominicana = "República Dominicana",
                                                 Saint.Kitts.y.Nevis = "Saint Kitts y Nevis",
                                                 San.Vicente.y.las.Ganadinas = "San Vicente y las Granadinas",
                                                 Santa.Lucía = "Santa Lucía",
                                                 Trinidad.y.Tabago = "Trinidad y Tabago",
                                                 Venezuela = "Venezuela (República Bolivariana de)"))


cartera_vencida_tidy_20 <- cartera_vencida_tidy %>% 
                          filter(pais_name %in% cepal_20_countries[["country.name.es"]])


## tidyng credito interno

country_names_credito_interno <- country_names_mess_ci %>% 
  select_if(! is.na(country_names_mess_ci)) %>% 
  str_split( "\\(") %>% 
  map_chr( .f = c(1,1)) %>% 
  str_trim()

# insert country names founds in credito interno sheet as a column of each data block
# in the same order as they appeared on top of each block

dates_credito_interno <- seq.Date(from = as.Date("1990-01-01"), to = as.Date("2016-09-01"), by = 'month')

dfs_ci_to_modify = dfs_ci
lower_case_names = str_to_lower(names(dfs_ci_to_modify[[1]]))

for(i in 1:length(dfs_ci)){
  
  names(dfs_ci_to_modify[[i]]) <-  lower_case_names
  
  # print( c(country_names_credito_interno[[i]], names(dfs_ci_to_modify[[i]])))
  
  dfs_ci_to_modify[[i]] <- dmap(dfs_ci_to_modify[[i]], as.numeric)
  
  dfs_ci_to_modify[[i]]$pais_name <- country_names_credito_interno[[i]]  
  
  dfs_ci_to_modify[[i]]$date <- dates_credito_interno
}

credito_interno = bind_rows(dfs_ci_to_modify)

credito_interno_tidy <- credito_interno %>% 
   mutate( pais_name = recode(pais_name, 
                             Bolivia = "Bolivia (Estado Plurinacional de)",
                             Costa.Rica = "Costa Rica",
                             "Rep. Dominicana" = "República Dominicana",
                             "San Kitts y Nevis" = "Saint Kitts y Nevis",
                             Surinam = "Suriname",
                             "República Bolivariana de Venezuela" = "Venezuela (República Bolivariana de)"))

credito_interno_tidy_20 = credito_interno_tidy %>% 
  filter(pais_name %in% cepal_20_countries[["country.name.es"]])




## tidyng prestamos bancarios
country_names_prestamos_bancarios <- country_names_mess_pb %>% 
  select_if(! is.na(country_names_mess_pb)) %>% 
  str_split( "\\(") %>% 
  map_chr( .f = c(1,1)) %>% 
  str_trim()

dates_prestamos_bancarios <- seq.Date(from = as.Date("1988-01-01"), to = as.Date("2016-08-01"), by = 'month')

dfs_pb_to_modify = dfs_pb
lower_case_names_pb = str_to_lower(names(dfs_pb_to_modify[[1]]))
lower_case_names_pb = lower_case_names_pb %>% str_replace("x.", "")

for(i in 1:length(dfs_pb)){
  
  names(dfs_pb_to_modify[[i]]) <-  lower_case_names_pb
  
  # print( c(country_names_prestamos_bancarios[[i]], names(dfs_pb_to_modify[[i]])))
  
  dfs_pb_to_modify[[i]] <- dmap(dfs_pb_to_modify[[i]], as.numeric)
  
  dfs_pb_to_modify[[i]]$pais_name <- country_names_prestamos_bancarios[[i]]  
  
  dfs_pb_to_modify[[i]]$date <- dates_prestamos_bancarios
}

prestamos_bancarios = bind_rows(dfs_pb_to_modify)

prestamos_bancarios_tidy <- prestamos_bancarios %>% 
  mutate( pais_name = recode(pais_name, 
                             Bolivia = "Bolivia (Estado Plurinacional de)",
                             "República Bolivariana de Venezuela" = "Venezuela (República Bolivariana de)"))

prestamos_bancarios_tidy_20 = prestamos_bancarios_tidy %>% 
  filter(pais_name %in% cepal_20_countries[["country.name.es"]])


## tidy







