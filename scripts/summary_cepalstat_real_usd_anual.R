library(lubridate)
library(stringr)
library(tidyverse)

load( "./produced_data/cs_real_dolares_20")
load( "./produced_data/cs_real_dolares")

cs_real <- cs_real_dolares %>% 
  select(-c(notas, fuente))

names_real <- names(cs_real)

cs_real_PIB <- cs_real %>% 
  filter(Rubro != "n/a") %>% 
  select(-c(Rubro_1, Rubro_2)) %>% 
  unite(rubro_en_indicador, c(Rubro, indicador), sep = "_en_", remove = FALSE)

cs_real_PIB_por_actividad <- cs_real %>% 
  filter(Rubro_1 != "n/a") %>% 
  select(-c(Rubro, Rubro_2)) %>% 
  rename(Rubro = Rubro_1) %>% 
  unite(rubro_en_indicador, c(Rubro, indicador), sep = "_en_", remove = FALSE)


cs_real_Ingreso_nacional <- cs_real %>% 
  filter(Rubro_2 != "n/a") %>% 
  select(-c(Rubro, Rubro_1)) %>% 
  rename(Rubro = Rubro_2) %>% 
  unite(rubro_en_indicador, c(Rubro, indicador), sep = "_en_", remove = FALSE)

  
cs_pib_pibsec_in  <- bind_rows(cs_real_PIB,
                               cs_real_PIB_por_actividad,
                               cs_real_Ingreso_nacional)

cs_pib_pibsec_in_corrientes <- cs_pib_pibsec_in %>% 
  filter(str_detect(indicador, "corriente"))

cs_pib_pibsec_in_constantes <- cs_pib_pibsec_in %>% 
  filter(str_detect(indicador, "constante"))

cs_real_PIB_corrientes <- cs_real_PIB %>% 
  filter(str_detect(indicador, "corriente"))

cs_real_PIB_constantes <- cs_real_PIB %>% 
  filter(str_detect(indicador, "constante"))

cs_real_PIB_por_actividad_corrientes <- cs_real_PIB_por_actividad %>% 
  filter(str_detect(indicador, "corriente"))

cs_real_PIB_por_actividad_constantes <- cs_real_PIB_por_actividad %>% 
  filter(str_detect(indicador, "constante"))

cs_real_Ingreso_nacional_corrientes <- cs_real_Ingreso_nacional %>% 
  filter(str_detect(indicador, "corriente"))

cs_real_Ingreso_nacional_constantes <- cs_real_Ingreso_nacional %>% 
  filter(str_detect(indicador, "constante"))


rpib <- unique(cs_real_PIB_por_actividad$Rubro)
rpibsec <- unique(cs_real_PIB_por_actividad$Rubro)
rin <- unique(cs_real_Ingreso_nacional$Rubro)
ipib <- unique(cs_real_PIB$indicador)
ipibsec <- unique(cs_real_PIB_por_actividad$indicador)
iin <- unique(cs_real_Ingreso_nacional$indicador)

pibcon_by_idp <- cs_real_PIB_constantes %>% 
  group_by(indicador, Rubro, iso3c) %>% 
  summarise(nobs = n(),
            first_year = min(Años),
            last_year = max(Años)
            ) %>% 
  arrange(indicador, Rubro, iso3c)


pibcor_by_idp <- cs_real_PIB_corrientes %>% 
  group_by(indicador, Rubro, iso3c) %>% 
  summarise(nobs = n(),
            first_year = min(Años),
            last_year = max(Años)
  ) %>% 
  arrange(indicador, Rubro, iso3c)


pibseccon_by_idp <- cs_real_PIB_por_actividad_constantes %>% 
  group_by(indicador, Rubro, iso3c) %>% 
  summarise(nobs = n(),
            first_year = min(Años),
            last_year = max(Años)
  ) %>% 
  arrange(indicador, Rubro, iso3c)


pibseccor_by_idp <- cs_real_PIB_por_actividad_corrientes %>% 
  group_by(indicador, Rubro, iso3c) %>% 
  summarise(nobs = n(),
            first_year = min(Años),
            last_year = max(Años)
  ) %>% 
  arrange(indicador, Rubro, iso3c)



incon_by_idp <- cs_real_Ingreso_nacional_constantes %>% 
  group_by(indicador, Rubro, iso3c) %>% 
  summarise(nobs = n(),
            first_year = min(Años),
            last_year = max(Años)
  ) %>% 
  arrange(indicador, Rubro, iso3c)


incor_by_idp <- cs_real_Ingreso_nacional_corrientes %>% 
  group_by(indicador, Rubro, iso3c) %>% 
  summarise(nobs = n(),
            first_year = min(Años),
            last_year = max(Años)
  ) %>% 
  arrange(indicador, Rubro, iso3c)




