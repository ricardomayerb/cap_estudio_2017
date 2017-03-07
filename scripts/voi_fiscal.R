## ---- prepare_cs_fiscal

library(stringr)
library(lubridate)
library(tidyverse)
library(tidyquant)

load("./produced_data/cs_sector_publico")

ing_trib <- cs_sector_publico %>% 
  filter(`Cobertura institucional` != "n/a") %>% 
  rename(year = Años)
# unique(ing_trib$indicador)

ing_trib_porpib <- ing_trib %>% 
  filter( str_detect(indicador, "porcentajes"))

ing_trib_lc <- ing_trib %>% 
  filter( str_detect(indicador, "moneda"))


operaciones <- cs_sector_publico %>% 
  filter(`Cobertura institucional_1` != "n/a") %>% 
  select(-c(Clasificacion_impuestos, `Cobertura institucional`, 
            `Tipo de persona`, `Tasa mínima/máxima`, Periodo_1,
            Clasificación_deuda, `Cobertura institucional_2`, fuente,
            notas, Periodo)
        )  %>% 
  rename(year = Años, 
         operacion = `Clasificación económica Operaciones del gobierno`) %>% 
  arrange(iso3c, operacion, year)


dupli_op_idx <-  duplicated(operaciones[ , 1:5])

dupli_op <- operaciones[dupli_op_idx, ] %>% 
  arrange(iso3c, operacion, year)

operaciones_no_dupli <- operaciones[!dupli_op_idx, ] %>% 
  arrange(iso3c, operacion, year)

#unique(operaciones$indicador)

# there are 2 observations for the year 2012!!! why??
operaciones_porpib <- operaciones_no_dupli %>% 
  filter( str_detect(indicador, "porcentajes")) 

# dupli_op_pib_idx <-  duplicated(operaciones_porpib[ , 1:5])
# 
# dupli_op_pib <- operaciones_porpib[dupli_op_pib_idx, ] %>% 
#   arrange(iso3c, operacion, year)



operaciones_lc <- operaciones_no_dupli %>% 
  filter( str_detect(indicador, "moneda"))


saldo_deuda <- cs_sector_publico %>% 
  filter(`Cobertura institucional_2` != "n/a")
# unique(saldo_deuda$indicador)

saldo_deuda_porpib <- operaciones %>% 
  filter( str_detect(indicador, "porcentajes"))

saldo_deuda_usd <- operaciones %>% 
  filter( str_detect(indicador, "millones"))



## ---- summarize cs_fiscal

tab_op_gc_pib_by_year <- operaciones_porpib %>% 
  group_by(iso3c, `Cobertura institucional_1`, operacion, year) %>% 
  summarise(nobs = n())


tab_op_gc_pib_by <- operaciones_porpib %>% 
  group_by(iso3c, `Cobertura institucional_1`, operacion) %>% 
  summarise(nobs = n())

## ---- examine selected operations

res_global_porpib <- operaciones_porpib %>% 
  filter(operacion == "Resultado global" )

res_global_gc_porpib <- operaciones_porpib %>% 
  filter(operacion == "Resultado global" &
         `Cobertura institucional_1`=="Gobierno central") %>% 
  rename( Cobertura = `Cobertura institucional_1` )

year_as_date <- date( as.POSIXct(as.character(res_global_gc_porpib$year), 
                                 format = "%Y", 
                                 origin=as.POSIXct("1990-12-31")))

res_global_gc_porpib$year <- year_as_date

res_global_gc_porpib_xts <- as_xts(res_global_gc_porpib, 
                                   date_col = year)


res_global_gg_porpib <- operaciones_porpib %>% 
  filter(operacion == "Resultado global" &
           `Cobertura institucional_1`=="Gobierno general")

res_global_gc_porpib_for_xts_long <- res_global_gc_porpib %>% 
  select(iso3c, year, valor)

res_global_gc_porpib_for_xts_wide <- res_global_gc_porpib %>% 
  select(iso3c, year, valor) %>% 
  spread(iso3c, valor)

library(dygraphs)
dygraph(res_global_gc_porpib_for_xts_wide[, c("year", "ARG", "BRA", "CHL")])


res_global_gc_porpib_for_xts_wide_d <- res_global_gc_porpib_for_xts_wide
res_global_gc_porpib_for_xts_wide_d$year <- seq.Date(as.Date("1990-12-31"), as.Date("2015-12-31"), "year")
foo <- as_xts(res_global_gc_porpib_for_xts_wide_d, date_col = year)



