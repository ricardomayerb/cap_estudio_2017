library(readxl)
library(tidyverse)

tipo_cambio_mensual_big <- read_excel(
  "./raw_data/Tipos_de_cambio_Nominal_Dolar_mensual_y_diario.xlsx", 
  skip = 1)


tcdate <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2016-08-01"),
                 by = "month") 

ndates <- length(tcdate)

tipo_cambio_mensual <- tipo_cambio_mensual_big[1:(ndates + 1), ]

rm(tipo_cambio_mensual_big)

tipo_cambio_mensual_clean <- tipo_cambio_mensual[-1,] 
nc <- ncol(tipo_cambio_mensual_clean)
tipo_cambio_mensual_clean <- tipo_cambio_mensual_clean[,1:(nc-4)] 
names(tipo_cambio_mensual_clean)[1:2] <- c("date", "mes") 

tipo_cambio_mensual_tidy <- tipo_cambio_mensual_clean %>% 
  select(-c(Rusia, `Estados Unidos `, `Nueva Zelandia`, `Reino Unido`,
            Suecia, Tailandia, Malasia, `China PR`, India, Japon,
            Australia, Sudafrica, `Euro Zona`, Suiza))

tipo_cambio_mensual_tidy$date <- tcdate

save(tipo_cambio_mensual_tidy,
     file = "./produced_data/tipo_de_cambio_mensual_bloom_clean")

# tipo_cambio_mensual <- tipo_cambio_mensual %>% 
  

