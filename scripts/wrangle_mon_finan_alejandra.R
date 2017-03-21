library(tidyverse)
library(stringr)
library(countrycode)
library(lubridate)
library(stringi)

load("./produced_data/datos_mon_finan_alej_messy")
load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_20_countries")


## tidying tasa de politica moetaria
country_names_tpm_mess <- names(tpm)[-1]
tmp_dates <- seq.Date(from = as.Date("1986-01-01"), to = as.Date("2016-10-01"), by = 'month')
tpm_tidy <-  tpm %>% 
        mutate(Col1 = tmp_dates) %>%
        gather(key = nombre_pais, value = tasa_politica_monetaria, -Col1) %>% 
        rename(date = Col1) %>%
        mutate( nombre_pais = recode(nombre_pais, 
                             Antigua.y.Barbuda = "Antigua y Barbuda",
                             Bolivia = "Bolivia (Estado Plurinacional de)",
                             Costa.Rica = "Costa Rica",
                             El.Salvador = "El Salvador",
                             República.Dominicana = "República Dominicana",
                             Saint.Kitts.y.Nevis = "Saint Kitts y Nevis",
                             San.Vicente.y.las.Ganadinas = "San Vicente y las Granadinas",
                             Santa.Lucía = "Santa Lucía",
                             Trinidad.y.Tabago = "Trinidad y Tabago",
                             Venezuela = "Venezuela (República Bolivariana de)"),
                year = year(date),
                month = month(date)
                )

names(tpm_tidy) <- stringi::stri_trans_general(
  names(tpm_tidy), "Latin-ASCII") 

tpm_33_tidy <- tpm_tidy %>% 
    mutate(iso2c = countrycode(nombre_pais, "country.name.es", "iso2c", 
                               custom_dict=cepal_33_countries),
           iso3c = countrycode(nombre_pais, "country.name.es", "iso3c", 
                               custom_dict=cepal_33_countries))

tpm_33_tidy$tasa_politica_monetaria <- 
  as.numeric(tpm_33_tidy$tasa_politica_monetaria)


tpm_20_tidy  <- tpm_33_tidy  %>% 
          filter(nombre_pais %in% cepal_20_countries[["country.name.es"]]) 


## tidyng cartera vencida
### separate Col1 into year and month
country_names_cv_mess <- names(cartera_vencida)[-1]
cv_dates <- seq.Date(from = as.Date("1993-12-01"), to = as.Date("2016-12-01"), by = 'month')
# del 12 del 93 al 6 del 16

cartera_vencida_tidy <- cartera_vencida %>% 
                      mutate(date = cv_dates) %>% 
                      separate(col=Col1, into=c('year', 'month'), sep='-M') %>% 
                      mutate(year = as.numeric(year), month = as.numeric(month)) %>%
                      gather(key=nombre_pais, value=cartera_vencida_percent, -year, -month, -date) %>% 
                      mutate( nombre_pais = recode(nombre_pais, 
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

names(cartera_vencida_tidy) <- stringi::stri_trans_general(
  names(cartera_vencida_tidy), "Latin-ASCII") 

cartera_vencida_33_tidy <- cartera_vencida_tidy %>% 
  mutate(iso2c = countrycode(nombre_pais, "country.name.es", "iso2c", 
                             custom_dict=cepal_33_countries),
         iso3c = countrycode(nombre_pais, "country.name.es", "iso3c", 
                             custom_dict=cepal_33_countries))

cartera_vencida_33_tidy$cartera_vencida_percent <- 
  as.numeric(cartera_vencida_33_tidy$cartera_vencida_percent)

cartera_vencida_20_tidy <- cartera_vencida_33_tidy %>% 
                          filter(nombre_pais %in% cepal_20_countries[["country.name.es"]])


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
  
  dfs_ci_to_modify[[i]]$nombre_pais <- country_names_credito_interno[[i]]  
  
  dfs_ci_to_modify[[i]]$date <- dates_credito_interno
}

credito_interno = bind_rows(dfs_ci_to_modify)

credito_interno_tidy <- credito_interno %>% 
   mutate( nombre_pais = recode(nombre_pais, 
                             Bolivia = "Bolivia (Estado Plurinacional de)",
                             Costa.Rica = "Costa Rica",
                             "Rep. Dominicana" = "República Dominicana",
                             "Santa Lucia" = "Santa Lucía",
                             "San Kitts y Nevis" = "Saint Kitts y Nevis",
                             Surinam = "Suriname",
                             "República Bolivariana de Venezuela" = "Venezuela (República Bolivariana de)"),
           year = year(date),
           month = month(date)
           )

names(credito_interno_tidy) <- stringi::stri_trans_general(
  names(credito_interno_tidy), "Latin-ASCII") 


credito_interno_33_tidy <- credito_interno_tidy %>% 
  mutate(iso2c = countrycode(nombre_pais, "country.name.es", "iso2c", 
                             custom_dict=cepal_33_countries),
         iso3c = countrycode(nombre_pais, "country.name.es", "iso3c", 
                             custom_dict=cepal_33_countries))
         




credito_interno_20_tidy = credito_interno_33_tidy %>% 
  filter(nombre_pais %in% cepal_20_countries[["country.name.es"]])




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
  
  dfs_pb_to_modify[[i]]$nombre_pais <- country_names_prestamos_bancarios[[i]]  
  
  dfs_pb_to_modify[[i]]$date <- dates_prestamos_bancarios
}

prestamos_bancarios = bind_rows(dfs_pb_to_modify)

prestamos_bancarios_tidy <- prestamos_bancarios %>% 
  mutate( nombre_pais = recode(nombre_pais, 
                             Bolivia = "Bolivia (Estado Plurinacional de)",
                             Venezuela = "Venezuela (República Bolivariana de)"),
          year=year(date), month=month(date))

names(prestamos_bancarios) <- stringi::stri_trans_general(
  names(prestamos_bancarios), "Latin-ASCII") 

prestamos_bancarios_33_tidy <- prestamos_bancarios_tidy %>% 
  mutate(iso2c = countrycode(nombre_pais, "country.name.es", "iso2c", 
                             custom_dict=cepal_33_countries),
         iso3c = countrycode(nombre_pais, "country.name.es", "iso3c", 
                             custom_dict=cepal_33_countries))

prestamos_bancarios_20_tidy = prestamos_bancarios_33_tidy %>% 
  filter(nombre_pais %in% cepal_20_countries[["country.name.es"]])




# tydig inflation targets

inf_dates = seq.Date(from=as.Date("2004-01-01"), to=as.Date("2018-12-01"), by="month")
inf_dates_ch <- as.character.Date(inf_dates, format = "%Y-%m-%d")


meta_inf$Col181 <- meta_inf$Col180
colnames(meta_inf) <- append(c("nombre_pais"), inf_dates_ch)
meta_inf$nombre_pais <- str_to_title(meta_inf$nombre_pais)

meta_inf_liminf$Col181 <- meta_inf_liminf$Col180
colnames(meta_inf_liminf) <- append(c("nombre_pais"), inf_dates_ch)
meta_inf_liminf$nombre_pais <- str_to_title(meta_inf_liminf$nombre_pais)

meta_inf_limsup$Col181 <- meta_inf_limsup$Col180
colnames(meta_inf_limsup) <- append(c("nombre_pais"), inf_dates_ch)
meta_inf_limsup$nombre_pais <- str_to_title(meta_inf_limsup$nombre_pais)

meta_inf_long <- gather(meta_inf, date, meta_inf, -c(nombre_pais))
meta_inf_liminf_long <- gather(meta_inf_liminf, date, meta_inf_low,
                               -c(nombre_pais))
meta_inf_limsup_long <- gather(meta_inf_limsup, date, meta_inf_hi,
                               -c(nombre_pais))

meta_inf_tidy <- meta_inf_long %>% 
    left_join(meta_inf_liminf_long, by=c("nombre_pais","date")) %>% 
    left_join(meta_inf_limsup_long, by=c("nombre_pais","date")) %>% 
    mutate(year=year(date), month=month(date))

meta_inf_tidy <- meta_inf_tidy %>% 
  mutate(iso2c = countrycode(nombre_pais, "country.name.es", "iso2c", 
                             custom_dict=cepal_33_countries),
         iso3c = countrycode(nombre_pais, "country.name.es", "iso3c", 
                             custom_dict=cepal_33_countries))

names(meta_inf_tidy) <- stringi::stri_trans_general(
  names(meta_inf_tidy), "Latin-ASCII") 


# save(tpm_33_tidy, prestamos_bancarios_33_tidy, credito_interno_33_tidy,
#      cartera_vencida_33_tidy, tpm_20_tidy, prestamos_bancarios_20_tidy, 
#      credito_interno_20_tidy, cartera_vencida_20_tidy, 
#      meta_inf_tidy,
#      file = "./produced_data/monetary_fin_tidy")




save(tpm_33_tidy, prestamos_bancarios_33_tidy, credito_interno_33_tidy,
     cartera_vencida_33_tidy, tpm_20_tidy, prestamos_bancarios_20_tidy, 
     credito_interno_20_tidy, cartera_vencida_20_tidy, 
     meta_inf_tidy,
     file = "./produced_data/data_with_basic_wrangling/monetary_fin_tidy")

