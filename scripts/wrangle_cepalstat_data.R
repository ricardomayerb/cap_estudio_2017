library(tidyverse)
library(stringr)
library(countrycode)
library(xts)
library(lubridate)

load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_20_countries")

load("./produced_data/cepalstat_desempleo")
load("./produced_data/cepalstat_empleo")
load("./produced_data/cepalstat_remuneraciones")
load("./produced_data/cepalstat_sector_publico")
load("./produced_data/cepalstat_sector_financiero_monetario")
load("./produced_data/cepalstat_sector_real_dolares_anual")
load("./produced_data/cepalstat_BP_anual")
load("./produced_data/cepalstat_BP_trimestral")
load("./produced_data/cepalstat_deuda_externa")
load("./produced_data/cepalstat_indicadores_derivados_de_la_BP")
load("./produced_data/cepalstat_comercio_intrarregional")
load("./produced_data/cepalstat_exp_imp_grandes_cat")
load("./produced_data/cepalstat_exp_imp_servicios")
load("./produced_data/cepalstat_exp_imp_totales_mensuales")
load("./produced_data/cepalstat_exp_prim_manuf")
load("./produced_data/cepalstat_indic_vol_precios_imp_exp")
load("./produced_data/cepalstat_tipo_de_cambio")
load("./produced_data/cepalstat_ipc_ipm_mensual")
load("./produced_data/cepalstat_ipc_ipm_anual")
load("./produced_data/cepalstat_agricultura")
load("./produced_data/cepalstat_mineria_manuf")
load("./produced_data/cepalstat_turismo")
load("./produced_data/cepalstat_precios_combustibles")
load("./produced_data/cepalstat_sector_real_mn_trimestral")

load("./produced_data/cepalstat_exp_imp_pro_ppal_part_1_of_2")
load("./produced_data/cepalstat_exp_imp_pro_ppal_part_2_of_2")

cepalstat_exp_imp_pro_ppal_part_1_of_2 <- cepalstat_exp_imp_pro_ppal_part_1_of_2 %>% 
  rename(indicador_old = indicador) %>% 
  separate(indicador_old, c("País", "indicador"), ":")

cepalstat_exp_imp_pro_ppal_part_2_of_2 <- cepalstat_exp_imp_pro_ppal_part_2_of_2 %>% 
  rename(indicador_old = indicador) %>% 
  separate(indicador_old, c("País", "indicador"), ":")

cepalstat_exp_imp_pro_ppal <- bind_rows(cepalstat_exp_imp_pro_ppal_part_1_of_2,
                                        cepalstat_exp_imp_pro_ppal_part_2_of_2)

rm(cepalstat_exp_imp_pro_ppal_part_2_of_2)
rm(cepalstat_exp_imp_pro_ppal_part_1_of_2)






# # experiment with a smaller dataframe, sampling 10% of the rows
# smaller_real <- cepalstat_sector_real_dolares_completo %>% 
#           sample_frac(0.1)
# 
# new_real = smaller_real %>% 
#   mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c"))
# 
# # Find our which countries has a missing observation (NA) associated to its iso3c
# not_with_iso3c_code <- new_real %>% filter(is.na(iso3c)) %>% select(País) %>% distinct()
# # Conclusion: it's OK, the only ones without a code are cointry aggregates
# # (i.e. "America Latina", "El Caribe" y "America Latina y El Caribe") and a row not referring to a country

# now, operateon the entire data frame, adding an iso3c column and droping observatios for country aggregates

nombres_mes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                 "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")


cs_real_dolares <- cepalstat_sector_real_dolares_completo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_financiero_monetario <- cepalstat_sector_financiero_monetario %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_financiero_monetario_anual <- cs_financiero_monetario %>% 
  filter(Periodo == "Anual")

cs_financiero_monetario_trimestral <- cs_financiero_monetario %>% 
  filter( str_detect(Periodo, "Trimestre")) %>% 
  mutate(quarter = str_replace(Periodo, "Trimestre ", "")) %>%
  unite(year_quarter, Años, quarter, remove = FALSE, sep = "-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format = "%Y-%q"),
         date = date(year_quarter)) %>%
  select(-Periodo)


cs_financiero_monetario_mensual <- cs_financiero_monetario %>% 
  filter( Periodo != "Anual" & !str_detect(Periodo, "Trimestre")) %>% 
  mutate(month = match(Periodo, nombres_mes)) %>% 
  unite(year_month, Años, month, remove = FALSE, sep = "-") %>%
  mutate(year_month = as.yearmon(year_month, format = "%Y-%m"),
         date = date(year_month)) %>%
  select(-Periodo)


cs_remuneraciones <- cepalstat_remuneraciones %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_remuneraciones_anual <- cs_remuneraciones %>% 
  filter(Trimestre == "n/a")  %>% 
  select(-Trimestre)

cs_remuneraciones_trimestral <- cs_remuneraciones %>% 
  filter(Trimestre != "n/a")  %>% 
  mutate(quarter = str_replace(Trimestre, "T", "")) %>%
  unite(year_quarter, Años, quarter, remove = FALSE, sep = "-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format = "%Y-%q"),
         date = date(year_quarter)) %>%
  select(-Trimestre)

cs_empleo <- cepalstat_empleo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_desempleo <- cepalstat_desempleo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_desempleo_anual <- cs_desempleo %>% 
  filter(indicador != "Tasa de desempleo trimestral" ) %>% 
  select(-Trimestre)

cs_desempleo_trimestral <- cs_desempleo %>% 
  filter(indicador == "Tasa de desempleo trimestral" ) %>% 
  mutate(quarter = str_replace(Trimestre, "T", "")) %>%
  unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-%q"),
         date = date(year_quarter)) %>%
  select(- Trimestre)


cs_bp_anual <- cepalstat_BP_anual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_bp_trimestral <- cepalstat_BP_trimestral %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País) %>% 
  mutate(quarter = str_replace(Trimestres, "Trimestre ", "Q")) %>%
  unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-Q%q"),
         date = date(year_quarter)) %>%
  select(- Trimestres)

cs_deuda_externa <- cepalstat_deuda_externa %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_indicadores_bp <- cepalstat_indicadores_derivados_de_la_BP %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_sector_publico <- cepalstat_sector_publico %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_comercio_intrarregional <- cepalstat_comercio_intrarregional %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_gran_cat <- cepalstat_exp_imp_grandes_cat %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_10_ppales <- cepalstat_exp_imp_pro_ppal %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_servicios <- cepalstat_exp_imp_servicios %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_total_mensual_trim <- cepalstat_exp_imp_totales_mensuales %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_total_mensual <- cs_x_m_total_mensual_trim %>% 
  filter(! `Trimestres y meses` %in% c("I","II","III","IV")) %>% 
  mutate(month = match(`Trimestres y meses`, nombres_mes)) %>% 
  unite(year_month, Años, month, remove = FALSE, sep = "-") %>%
  mutate(year_month = as.yearmon(year_month, format = "%Y-%m"),
         date = date(year_month)) %>% 
  select(-`Trimestres y meses`)

quarters_roman_num <- c("I", "II", "III", "IV")

cs_x_m_total_trimestral <- cs_x_m_total_mensual_trim %>% 
  filter( `Trimestres y meses` %in% c("I","II","III","IV")) %>% 
  mutate(quarter = match(`Trimestres y meses`, quarters_roman_num)) %>% 
  unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-%q"),
         date = date(year_quarter)) %>%
  select(-`Trimestres y meses`)


cs_x_prim_manuf <- cepalstat_exp_prim_manuf %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_tipo_cambio <- cepalstat_tipo_de_cambio %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País) 

cs_x_m_vol_precios <- cepalstat_indic_vol_precios_imp_exp %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País) %>% 
  select(-Meses)

cs_ipc_mensual <- cepalstat_ipc_ipm_mensual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)  %>% 
  mutate(month = match(Meses, nombres_mes)) %>% 
  unite(year_month, Años, month, remove = FALSE, sep = "-") %>%
  mutate(year_month = as.yearmon(year_month, format = "%Y-%m"),
         date = date(year_month)) %>% 
  select(-Meses)
  

cs_ipc_anual <- cepalstat_ipc_ipm_anual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_agricultura <- cepalstat_agricultura %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_mineria_manuf <- cepalstat_mineria_manuf %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_turismo <- cepalstat_turismo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_precios_combustibles <- cepalstat_precios_combustibles %>% 
  mutate(iso3c = countrycode(Países, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(Países, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = Países)


cepalstat_sector_real_mn_trimestral <- cepalstat_sector_real_mn_trimestral %>% 
  separate(col="País [Año base]", into=c("País", "Año base"), sep="\\[") %>% 
  select(-contains("base")) %>% mutate(País = str_trim(País))


gdp <- cepalstat_sector_real_mn_trimestral %>% 
  filter(Rubro == "Producto interno bruto (PIB)") %>% 
  arrange(Años, Trimestres)

gdp_cl <-  gdp %>% 
  filter(`País [Año base]`=="Chile [año base 2008]") %>% 
  arrange(Años, Trimestres)

cs_real_mn_trimestral <- cepalstat_sector_real_mn_trimestral %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País) %>% 
  mutate(quarter = str_replace(Trimestres, "Trimestre ", "Q")) %>%
  unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-Q%q"),
           date = date(year_quarter)) %>%
  select(- Trimestres)


# 
# gdp_currentlc_q <- cs_real_mn_trimestral_20 %>% 
#   filter(Rubro=="Producto interno bruto (PIB)") %>% 
#   select( -c(indicador, Rubro_1, notas, fuente)) %>% 
#   mutate(quarter = str_replace(Trimestres, "Trimestre ", "Q")) %>% 
#   unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>% 
#   mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-Q%q"),
#          date = date(year_quarter)) %>% 
#   select(- Trimestres)


cs_real_dolares_20 <- cs_real_dolares %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_financiero_monetario_anual_20 <- cs_financiero_monetario_anual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_financiero_monetario_trimestral_20 <- cs_financiero_monetario_trimestral %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_financiero_monetario_mensual_20 <- cs_financiero_monetario_mensual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_remuneraciones_anual_20 <- cs_remuneraciones_anual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_remuneraciones_trimestral_20 <- cs_remuneraciones_trimestral %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_empleo_20 <- cs_empleo %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_desempleo_anual_20 <- cs_desempleo_anual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_desempleo_trimestral_20 <- cs_desempleo_trimestral %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_bp_anual_20 <- cs_bp_anual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_bp_trimestral_20 <- cs_bp_trimestral %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_deuda_externa_20 <- cs_deuda_externa %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_indicadores_bp_20 <- cs_indicadores_bp %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_sector_publico_20 <- cs_sector_publico %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_comercio_intrarregional_20 <- cs_comercio_intrarregional %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_m_gran_cat_20 <- cs_x_m_gran_cat %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_m_10_ppales_20 <- cs_x_m_10_ppales %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_m_servicios_20 <- cs_x_m_servicios %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_m_total_mensual_20 <- cs_x_m_total_mensual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_m_total_trimestral_20 <- cs_x_m_total_trimestral %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_prim_manuf_20 <- cs_x_prim_manuf %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_tipo_cambio_20 <- cs_tipo_cambio %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_x_m_vol_precios_20 <- cs_x_m_vol_precios %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_ipc_mensual_20 <- cs_ipc_mensual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_ipc_anual_20 <- cs_ipc_anual %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_agricultura_20 <- cs_agricultura %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_mineria_manuf_20 <- cs_mineria_manuf %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_turismo_20 <- cs_turismo %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_precios_combustibles_20 <- cs_precios_combustibles %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])
cs_real_mn_trimestral_20 <- cs_real_mn_trimestral %>% filter(iso3c %in% cepal_20_countries[["iso3c"]])



save(cs_real_dolares, file = "./produced_data/cs_real_dolares")
save(cs_financiero_monetario_anual, file = "./produced_data/cs_financiero_monetario_anual")
save(cs_financiero_monetario_trimestral, file = "./produced_data/cs_financiero_monetario_trimestral")
save(cs_financiero_monetario_mensual, file = "./produced_data/cs_financiero_monetario_mensual")
save(cs_remuneraciones_anual, file = "./produced_data/cs_remuneraciones_anual")
save(cs_remuneraciones_trimestral, file = "./produced_data/cs_remuneraciones_trimestral")
save(cs_empleo, file = "./produced_data/cs_empleo")
save(cs_desempleo_anual, file = "./produced_data/cs_desempleo_anual")
save(cs_desempleo_trimestral, file = "./produced_data/cs_desempleo_trimestral")
save(cs_bp_anual, file = "./produced_data/cs_bp_anual")
save(cs_bp_trimestral, file = "./produced_data/cs_bp_trimestral")
save(cs_deuda_externa, file = "./produced_data/cs_deuda_externa")
save(cs_indicadores_bp, file = "./produced_data/cs_indicadores_bp")
save(cs_sector_publico, file = "./produced_data/cs_sector_publico")
save(cs_comercio_intrarregional, file = "./produced_data/cs_comercio_intrarregional")
save(cs_x_m_gran_cat, file = "./produced_data/cs_x_m_gran_cat")
save(cs_x_m_10_ppales, file = "./produced_data/cs_x_m_10_ppales")
save(cs_x_m_servicios, file = "./produced_data/cs_x_m_servicios")
save(cs_x_m_total_mensual, file = "./produced_data/cs_x_m_total_mensual")
save(cs_x_m_total_trimestral, file = "./produced_data/cs_x_m_total_trimestral")
save(cs_x_prim_manuf, file = "./produced_data/cs_x_prim_manuf")
save(cs_tipo_cambio, file = "./produced_data/cs_tipo_cambio")
save(cs_x_m_vol_precios, file = "./produced_data/cs_x_m_vol_precios")
save(cs_ipc_mensual, file = "./produced_data/cs_ipc_mensual")
save(cs_ipc_anual, file = "./produced_data/cs_ipc_anual")
save(cs_agricultura, file = "./produced_data/cs_agricultura")
save(cs_mineria_manuf, file = "./produced_data/cs_mineria_manuf")
save(cs_turismo, file = "./produced_data/cs_turismo")
save(cs_precios_combustibles, file = "./produced_data/cs_precios_combustibles")
save(cs_real_mn_trimestral, file = "./produced_data/cs_real_mn_trimestral")





save(cs_real_dolares_20, file = "./produced_data/cs_real_dolares_20")
save(cs_financiero_monetario_anual_20, file = "./produced_data/cs_financiero_monetario_anual_20")
save(cs_financiero_monetario_trimestral_20, file = "./produced_data/cs_financiero_monetario_trimestral_20")
save(cs_financiero_monetario_mensual_20, file = "./produced_data/cs_financiero_monetario_mensual_20")
save(cs_remuneraciones_anual_20, file = "./produced_data/cs_remuneraciones_anual_20")
save(cs_remuneraciones_trimestral_20, file = "./produced_data/cs_remuneraciones_trimestral_20")
save(cs_empleo_20, file = "./produced_data/cs_empleo_20")
save(cs_desempleo_anual_20, file = "./produced_data/cs_desempleo_anual_20")
save(cs_desempleo_trimestral_20, file = "./produced_data/cs_desempleo_trimestral_20")
save(cs_bp_anual_20, file = "./produced_data/cs_bp_anual_20")
save(cs_bp_trimestral_20, file = "./produced_data/cs_bp_trimestral_20")
save(cs_deuda_externa_20, file = "./produced_data/cs_deuda_externa_20")
save(cs_indicadores_bp_20, file = "./produced_data/cs_indicadores_bp_20")
save(cs_sector_publico_20, file = "./produced_data/cs_sector_publico_20")
save(cs_comercio_intrarregional_20, file = "./produced_data/cs_comercio_intrarregional_20")
save(cs_x_m_gran_cat_20, file = "./produced_data/cs_x_m_gran_cat_20")
save(cs_x_m_10_ppales_20, file = "./produced_data/cs_x_m_10_ppales_20")
save(cs_x_m_servicios_20, file = "./produced_data/cs_x_m_servicios_20")
save(cs_x_m_total_mensual_20, file = "./produced_data/cs_x_m_total_mensual_20")
save(cs_x_m_total_trimestral_20, file = "./produced_data/cs_x_m_total_trimestral_20")
save(cs_x_prim_manuf_20, file = "./produced_data/cs_x_prim_manuf_20")
save(cs_tipo_cambio_20, file = "./produced_data/cs_tipo_cambio_20")
save(cs_x_m_vol_precios_20, file = "./produced_data/cs_x_m_vol_precios_20")
save(cs_ipc_mensual_20, file = "./produced_data/cs_ipc_mensual_20")
save(cs_ipc_anual_20, file = "./produced_data/cs_ipc_anual_20")
save(cs_agricultura_20, file = "./produced_data/cs_agricultura_20")
save(cs_mineria_manuf_20, file = "./produced_data/cs_mineria_manuf_20")
save(cs_turismo_20, file = "./produced_data/cs_turismo_20")
save(cs_precios_combustibles_20, file = "./produced_data/cs_precios_combustibles_20")
save(cs_real_mn_trimestral_20, file = "./produced_data/cs_real_mn_trimestral_20")


