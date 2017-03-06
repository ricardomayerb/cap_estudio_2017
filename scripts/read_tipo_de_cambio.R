library(openxlsx)
library(tidyverse)
library(countrycode)

load("./produced_data/cepal_20_countries")
load("./produced_data/cepal_33_countries")

# tipo_cambio_mensual_big <- read_excel(
#   "./raw_data/Tipos_de_cambio_Nominal_Dolar_mensual_y_diario.xlsx", 
#   skip = 1)

tipo_cambio_mensual_big <- read.xlsx(
  "./raw_data/Tipos_de_cambio_Nominal_Dolar_mensual_y_diario.xlsx",
  startRow = 2)

tcdate <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2016-08-01"),
                 by = "month") 

ndates <- length(tcdate)

tipo_cambio_mensual <- tipo_cambio_mensual_big[1:(ndates + 1), ]

# rm(tipo_cambio_mensual_big)

tipo_cambio_mensual_clean <- tipo_cambio_mensual[-1,] 
nc <- ncol(tipo_cambio_mensual_clean)
# tipo_cambio_mensual_clean <- tipo_cambio_mensual_clean[,1:nc)] 
names(tipo_cambio_mensual_clean)[1:2] <- c("date", "mes") 

tipo_cambio_mensual_tidy <- tipo_cambio_mensual_clean %>% 
  select(-c(Rusia, Estados.Unidos, Nueva.Zelandia, Reino.Unido,
            Suecia, Tailandia, Malasia, China.PR, India, Japon,
            Australia, Sudafrica, Euro.Zona, Suiza, Corea,
            Canada, Taiwan)) %>% 
  mutate(date = tcdate) %>% 
  gather(key = nombre_pais, value = tc_mensual, -c(date, mes)) %>% 
  mutate(nombre_pais = recode(nombre_pais,
                              "Arg" = "Argentina",
                              "Bolivia" = "Bolivia (Estado Plurinacional de)",
                              "El.Salvador" = "El Salvador",
                              "Costa.Rica" = "Costa Rica",
                              "Rep..Dominicana" = "República Dominicana",
                              "Trinidad.y.T" = "Trinidad y Tabago",
                              "Ven" = "Venezuela (República Bolivariana de)",
                              "Haiti" = "Haití",
                              "Mexico" = "México",
                              "Peru" = "Perú"                              )
)
  
tipo_cambio_mensual_tidy_33 <- tipo_cambio_mensual_tidy %>% 
  mutate(iso2c = countrycode(nombre_pais, "country.name.es", "iso2c", 
                             custom_dict=cepal_33_countries),
         iso3c = countrycode(nombre_pais, "country.name.es", "iso3c", 
                             custom_dict=cepal_33_countries))

tipo_cambio_mensual_tidy_20  <- tipo_cambio_mensual_tidy_33  %>% 
  filter(nombre_pais %in% cepal_20_countries[["country.name.es"]]) 


save(tipo_cambio_mensual_tidy, tipo_cambio_mensual_tidy_33,
     tipo_cambio_mensual_tidy_20,
     file = "./produced_data/tipo_de_cambio_mensual_bloom_clean")

# tipo_cambio_mensual <- tipo_cambio_mensual %>% 
  

