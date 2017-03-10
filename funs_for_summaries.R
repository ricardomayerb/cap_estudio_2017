#### functions to report basic statistics from cepal-related datasets
library(lubridate)
library(stringr)
library(tidyverse)
library(lazyeval)

load("./produced_data/cs_sector_publico")
load("./produced_data/cepal_19_countries")
load("./produced_data/cs_deuda_externa")


de_data_possible_duplicates <- cs_deuda_externa  %>% 
  select( -c(fuente, notas) ) %>% 
  rename(year = Años)
rm(cs_deuda_externa)
de_data_dupli_idx <- duplicated(de_data_possible_duplicates[,1:3])
de_data <- de_data_possible_duplicates[!de_data_dupli_idx, ] 
de_pib <- de_data %>% 
  filter( str_detect(indicador, "porcentaje")) 

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
  rename(cobertura =  `Cobertura institucional_1`) %>% 
  mutate(cobertura = str_to_lower(cobertura),
         cobertura = str_replace_all(cobertura, pattern = " ",
                                     replacement = "_")) %>% 
  mutate(operacion =  str_to_lower(operacion),
         operacion = str_replace_all(operacion, pattern = " ",
                                     replacement = "_"))  %>% 
  arrange(iso3c, operacion, year)

operaciones_pib <- operaciones %>% 
  filter( str_detect(indicador, "porcentajes")) 

members_operaciones_pib <- unique(operaciones_pib$operacion)

ing_trib <- sp_data %>% 
  filter(`Cobertura institucional` != "n/a") %>% 
  rename(cobertura =  `Cobertura institucional`) %>% 
  select(-c(`Tipo de persona`, Periodo, `Tasa mínima/máxima`, Periodo_1,
            `Clasificación económica Operaciones del gobierno`, 
            `Cobertura institucional_1`, `Cobertura institucional_2`,
            Clasificación_deuda))
# unique(ing_trib$indicador)

ing_trib_pib <- ing_trib %>% 
  filter( str_detect(indicador, "porcentajes"))

ing_trib_lc <- ing_trib %>% 
  filter( str_detect(indicador, "moneda"))



fiscal_reporting <- function(countries = c("ARG", "CHL", "MEX"),
                             c_year=2015,
                             ops_cobertura = c("gobierno_central",
                                               "gobierno_general",
                                               "sector_público_no_financiero"),
                             sel_ops = c("resultado_global",
                                         "resultado_primario")) {

  filter_countries = interp(~ iso3c %in% countries, 
                           list(iso3c = as.name("iso3c"),
                           countries = as.name("countries")))
  
  s_de_pib <- de_pib %>% 
    filter_(filter_countries) 
    

  s_operaciones_pib <- operaciones_pib %>% 
    filter_(filter_countries) %>% 
    filter(cobertura %in% ops_cobertura)
  
  print(names(s_operaciones_pib))
  
  de_report_date_ranges_by_country <- s_de_pib %>% 
    filter_(filter_countries) %>% 
    group_by(iso3c) %>% 
    summarise(nobs = n(),
              first_date = min(year),
              last_date = max(year)
    ) %>% 
    arrange(nobs)
  
  de_report_by_country <- de_pib %>% 
    filter_(filter_countries) %>% 
    group_by(iso3c) %>% arrange(year) %>% 
    summarise(ex_debt_mr = last(valor),
              ex_debt_last_3 = mean(tail(valor, n=3)),
              ex_debt_last_5 = mean(tail(valor, n=5))
              ) %>% 
    arrange(ex_debt_mr)
    
  
  op_report_date_ranges_by_country <- s_operaciones_pib %>% 
    group_by(operacion, cobertura, iso3c) %>% 
    summarise(nobs = n(),
              first_date = min(year),
              last_date = max(year)
              ) %>% 
    arrange(cobertura, operacion, nobs)
    
  op_report_date_ranges <- s_operaciones_pib %>% 
    group_by(operacion, cobertura) %>% 
    summarise(n_countries = length(unique(iso3c)),
              first_date = min(year),
              last_date = max(year)
              ) %>% 
    arrange(cobertura, operacion, n_countries)
  
  op_report_current_year <- s_operaciones_pib %>% 
    filter(year == c_year) %>% 
    filter(operacion %in% sel_ops) %>% 
    group_by(operacion, cobertura, iso3c) %>% 
    summarise(resultado = valor
              ) %>% 
    arrange(resultado)
  
  op_report <- s_operaciones_pib %>% 
    filter(operacion %in% sel_ops) %>% 
    group_by(operacion, cobertura, iso3c) %>% 
    summarise(valor_mr = last(valor),
              promedio_3_mr = mean(tail(valor, 3)),
              promedio_5_mr = mean(tail(valor, 5)),
              first_date = min(year),
              last_date = max(year)
    ) %>% 
    arrange(valor_mr)
 
    
  return (list(op_rep = op_report,
               op_cy = op_report_current_year, op_dates = op_report_date_ranges,
               op_dates_c = op_report_date_ranges_by_country,
               de_dates = de_report_date_ranges_by_country,
               de_valor_c = de_report_by_country))
}

sel_countries_1 = c("ARG", "CHL", "MEX")
sel_countries_2 = c("ARG", "BOL", "CHL", "MEX")
sel_countries_3 = c("ARG", "BOL", "CHL", "MEX", "VEN")
cepal_19 = cepal_19_countries$iso3c
# my_fr = fiscal_reporting(sel_countries_3)
my_fr = fiscal_reporting(countries = sel_countries_3,
                         ops_cobertura = c("gobierno_central",
                                           "gobierno_general"))
my_fr

op <-  my_fr[[1]]
op_cy <-  my_fr[[2]]

op_d <-  my_fr[[3]]
op_d_c <-  my_fr[[4]]

de_d <-  my_fr[[5]]
de_v <-  my_fr[[6]]


View(op)
View(op_cy)
View(op_d)
View(op_d_c)
View(de_d)
View(de_v)

# unique(operaciones_pib$operacion)
# [1] "adquisición_de_activos_de_capital_fijo"      "compras_de_bienes_y_servicios"              
# [3] "concesión_de_préstamos_menos_recuperaciones" "donaciones_externas"                        
# [5] "gasto_total_y_préstamo_neto"                 "gastos_corrientes"                          
# [7] "gastos_de_capital"                           "ingreso_total_y_donaciones"                 
# [9] "ingresos_corrientes"                         "ingresos_de_capital"                        
# [11] "ingresos_no_tributarios"                     "ingresos_tributarios"                       
# [13] "otros_gastos_corrientes"                     "otros_gastos_de_capital"                    
# [15] "pagos_de_intereses"                          "resultado_global"                           
# [17] "resultado_primario"                          "subsidios_y_transferencias_corrientes"      
# [19] "sueldos_y_salarios"                          "transferencias_de_capital"                  
# [21] "financiamiento_externo"                      "financiamiento_interno"                     
# [23] "financiamiento_otro"                         "financiamiento_total" 


