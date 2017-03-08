#### functions to report basic statistics from cepal-related datasets
library(lubridate)
library(stringr)
library(tidyverse)
library(lazyeval)

load("./produced_data/cs_sector_publico")

sp_data_possible_duplicates <- cs_sector_publico %>% 
  select( -c(fuente, notas) ) %>% 
  rename(year = Años)

rm(cs_sector_publico)

sp_data_dupli_idx <- duplicated(sp_data_possible_duplicates[,1:13])

sp_data <- sp_data_possible_duplicates[!sp_data_dupli_idx, ]

operaciones <- sp_data %>% 
  filter(`Cobertura institucional_1` != "n/a") %>% 
  select(-c(Clasificacion_impuestos, `Cobertura institucional`, 
            `Tipo de persona`, `Tasa mínima/máxima`, Periodo_1,
            Clasificación_deuda, `Cobertura institucional_2`, Periodo))  %>% 
  rename(operacion = `Clasificación económica Operaciones del gobierno`) %>% 
  mutate(operacion = recode(operacion,
                            resultado_global = `Resultado global`,
                            resultado_primario = `Resultado primario`
                            )
         ) %>% 
  arrange(iso3c, operacion, year)

operaciones_pib <- operaciones %>% 
  filter( str_detect(indicador, "porcentajes")) 


ing_trib <- sp_data %>% 
  filter(`Cobertura institucional` != "n/a") %>% 
  rename(cobertura = `Cobertura institucional`) %>% 
  select(-c(`Tipo de persona`, Periodo, `Tasa mínima/máxima`, Periodo_1,
            `Clasificación económica Operaciones del gobierno`, 
            `Cobertura institucional_1`, `Cobertura institucional_2`,
            Clasificación_deuda))
# unique(ing_trib$indicador)

ing_trib_pib <- ing_trib %>% 
  filter( str_detect(indicador, "porcentajes"))

ing_trib_lc <- ing_trib %>% 
  filter( str_detect(indicador, "moneda"))



fiscal_reporting <- function(countries = c("ARG", "CHL", "MEX")) {

  filter_countries = interp(~ iso3c %in% countries, 
                           list(iso3c = as.name("iso3c"),
                           countries = as.name("countries")))
                           
  s_ing_trib_pib <- ing_trib_pib %>% 
    filter_(filter_countries)  
  
  it_report <- s_ing_trib_pib %>% 
    group_by(cobertura, iso3c, Clasificacion_impuestos) %>% 
    summarise(avg_inc = mean(valor))

  s_operaciones_pib <- operaciones_pib %>% 
    filter_(filter_countries)  
  
  op_report <- s_ing_trib_pib %>% 
    group_by(cobertura, iso3c, year) %>% 
      summarise(rg = `Resultado global`)
 
    
  return (list(it_df = s_ing_trib_pib, it_rep = it_report))
}


my_fr = fiscal_reporting()
my_fr

foo <- my_fr[[1]]
moo <- my_fr[[2]]


foos = c("ARG", "CHL", "MEX")
fool = unique(ing_trib_pib$iso3c)
