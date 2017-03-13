library(tidyverse)
library(stringr)
library(countrycode)
library(xts)
library(lubridate)
library(seasonal)
library(lubridate)
library(purrr)

source('~/GitHub/cap_estudio_2017/functions/add_iso.R')
quiet_iso <- purrr::quietly(add_iso)
# quiet_iso2 <- function(purrr::quietly(add_iso)[["result"]]

load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_20_countries")

load("./produced_data/cepalstat_sector_publico")


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

nombres_mes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                 "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

cs_real_dolares <- quiet_iso(df = cepalstat_sector_real_dolares_completo,
                  names_col = "País", dict = cepal_33_countries, rm_nf = TRUE)$result 

cs_financiero_monetario <- quiet_iso(df = cepalstat_sector_financiero_monetario,
                  names_col = "País", dict = cepal_33_countries, rm_nf = TRUE)$result 

cs_remuneraciones <- quiet_iso(df = cepalstat_remuneraciones, names_col = "País",
                               dict = cepal_33_countries, rm_nf = TRUE)$result
  
cs_empleo <- quiet_iso(df = cepalstat_empleo, names_col = "País",
                       dict = cepal_33_countries, rm_nf = TRUE)$result

cs_desempleo <- quiet_iso(df = cepalstat_desempleo, names_col = "País",
                          dict = cepal_33_countries, rm_nf = TRUE)$result

cs_bp_anual <- quiet_iso(df = cepalstat_BP_anual, names_col = "País",
                         dict = cepal_33_countries, rm_nf = TRUE)$result

cs_deuda_externa <- quiet_iso(cepalstat_deuda_externa, names_col = "País",
                              dict = cepal_33_countries, rm_nf = TRUE)$result

cs_indicadores_bp <- quiet_iso(cepalstat_indicadores_derivados_de_la_BP,
            names_col = "País", dict = cepal_33_countries, rm_nf = TRUE)$result

cs_sector_publico <- quiet_iso(df = cepalstat_sector_publico, names_col = "País",
                               dict = cepal_33_countries, rm_nf = TRUE)$result

cs_comercio_intrarregional <- cepalstat_comercio_intrarregional %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)


# cs_comercio_intrarregional <- quiet_iso(def = cepalstat_comercio_intrarregional, 
#                   names_col = "País", dict = cepal_33_countries, rm_nf = TRUE)$result


cs_x_m_gran_cat <- quiet_iso(df = cepalstat_exp_imp_grandes_cat, names_col = "País",
                             dict = cepal_33_countries, rm_nf = TRUE)$result

cs_x_m_10_ppales <- quiet_iso(cepalstat_exp_imp_pro_ppal, names_col = "País",
                              dict = cepal_33_countries, rm_nf = TRUE)$result

cs_x_m_servicios <- quiet_iso(cepalstat_exp_imp_servicios, names_col = "País",
                              dict = cepal_33_countries, rm_nf = TRUE)$result

cs_x_m_total_mensual_trim <- quiet_iso(cepalstat_exp_imp_totales_mensuales,
                names_col = "País", dict = cepal_33_countries, rm_nf = TRUE)$result


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

cs_x_prim_manuf <- quiet_iso(cepalstat_exp_prim_manuf, names_col = "País", 
                             dict = cepal_33_countries, rm_nf = TRUE)$result

cs_tipo_cambio <- quiet_iso(cepalstat_tipo_de_cambio, names_col = "País", 
                            dict = cepal_33_countries, rm_nf = TRUE)$result

cs_x_m_vol_precios <- quiet_iso(cepalstat_indic_vol_precios_imp_exp, names_col = "País", 
                                dict = cepal_33_countries, rm_nf = TRUE)$result %>% 
  select(-Meses)

cs_x_m_vol_precios <- quiet_iso(cepalstat_indic_vol_precios_imp_exp, names_col = "País", 
                                dict = cepal_33_countries, rm_nf = TRUE)$result %>% 
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


cs_ipc_anual <- quiet_iso(cepalstat_ipc_ipm_anual, names_col = "País", 
                          dict = cepal_33_countries, rm_nf = TRUE)$result


cs_agricultura <- quiet_iso(cepalstat_agricultura , names_col = "País", 
dict = cepal_33_countries, rm_nf = TRUE)$result


cs_mineria_manuf <- quiet_iso(cepalstat_mineria_manuf , names_col = "País", 
dict = cepal_33_countries, rm_nf = TRUE)$result


cs_turismo <- quiet_iso(cepalstat_turismo , names_col = "País", 
dict = cepal_33_countries, rm_nf = TRUE)$result


cs_precios_combustibles <- quiet_iso(cepalstat_precios_combustibles , names_col = "Países", 
dict = cepal_33_countries, rm_nf = TRUE)$result


cs_bp_trimestral <- cepalstat_BP_trimestral %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País) %>% 
  mutate(quarter = str_replace(Trimestres, "Trimestre ", "Q")) %>%
  unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-Q%q"),
         date = date(year_quarter)) %>%
  select(- Trimestres)


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




cepalstat_sector_real_mn_trimestral <- cepalstat_sector_real_mn_trimestral %>% 
  separate(col="País [Año base]", into=c("País", "Año base"), sep="\\[") %>% 
  select(-contains("base")) %>% mutate(País = str_trim(País))


# gdp <- cepalstat_sector_real_mn_trimestral %>% 
#   filter(Rubro == "Producto interno bruto (PIB)") %>% 
#   arrange(Años, Trimestres)
# 
# gdp_cl <-  gdp %>% 
#   filter(`País [Año base]`=="Chile [año base 2008]") %>% 
#   arrange(Años, Trimestres)

cs_real_mn_trimestral <- cepalstat_sector_real_mn_trimestral %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_33_countries, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País) %>% 
  mutate(quarter = str_replace(Trimestres, "Trimestre ", "Q")) %>%
  unite(year_quarter, Años, quarter, remove=FALSE, sep="-") %>%
  mutate(year_quarter = as.yearqtr(year_quarter, format="%Y-Q%q"),
         date = date(year_quarter)) %>%
  select(- Trimestres)

cs_gdp_currentlc_q_gasto <- cs_real_mn_trimestral %>% 
  filter(Rubro == "Producto interno bruto (PIB)" ) %>% 
  filter(indicador ==
           "Producto interno bruto trimestral por objeto del gasto a precios corrientes") %>% 
  select( -c(indicador, Rubro_1, notas, fuente)) %>% 
  dplyr::rename(gdp = valor) %>% 
  arrange(iso2c, date) 

cs_gdp_constantlc_q_gasto <- cs_real_mn_trimestral %>% 
  filter(Rubro == "Producto interno bruto (PIB)" ) %>% 
  filter(indicador ==
           "Producto interno bruto trimestral por objeto del gasto a precios constantes") %>% 
  select( -c(indicador, Rubro_1, notas, fuente)) %>% 
  dplyr::rename(gdp = valor) %>% 
  arrange(iso2c, date) 

cs_gdp_currentlc_q_activ <- cs_real_mn_trimestral %>% 
  filter(Rubro_1 == "Producto interno bruto (PIB)" ) %>% 
  filter(indicador ==
           "Producto interno bruto trimestral por clase de actividad económica a precios corrientes") %>% 
  select( -c(indicador, Rubro, notas, fuente)) %>% 
  dplyr::rename(gdp = valor) %>% 
  arrange(iso2c, date) 

cs_gdp_constantlc_q_activ <- cs_real_mn_trimestral %>% 
  filter(Rubro_1 == "Producto interno bruto (PIB)" ) %>% 
  filter(indicador ==
           "Producto interno bruto trimestral por clase de actividad económica a precios constantes") %>% 
  select( -c(indicador, Rubro, notas, fuente)) %>% 
  dplyr::rename(gdp = valor) %>% 
  arrange(iso2c, date) 

gdp_iso <- unique(cs_gdp_currentlc_q_gasto$iso2c)
cs_gdp_currentlc_q_gasto$gdp_sa_seas <- NA
cs_gdp_currentlc_q_gasto$gdp_sa_stl <- NA

for (country in gdp_iso) {
  idx <- which(cs_gdp_currentlc_q_gasto$iso2c == country)
  alldates = cs_gdp_currentlc_q_gasto$date[idx]
  fdate = first(alldates)
  
  ystart = year(fdate)
  mstart = month(fdate)
  sgdp = ts(cs_gdp_currentlc_q_gasto$gdp[idx], start = c(ystart, mstart), frequency = 4)
  
  gdp_decom = decompose(sgdp)$trend
  gdp_seas = final(seas(sgdp))
  gdp_stl_list = stl(sgdp, s.window = "periodic")
  gdp_stl = gdp_stl_list$time.series[ , 2]
  cs_gdp_currentlc_q_gasto$gdp_sa_seas[idx] <- gdp_seas
  cs_gdp_currentlc_q_gasto$gdp_sa_stl[idx] <- gdp_stl
}



gdp_iso <- unique(cs_gdp_constantlc_q_gasto$iso2c)
cs_gdp_constantlc_q_gasto$gdp_sa_seas <- NA
cs_gdp_constantlc_q_gasto$gdp_sa_stl <- NA

for (country in gdp_iso) {
  idx <- which(cs_gdp_constantlc_q_gasto$iso2c == country)
  alldates = cs_gdp_constantlc_q_gasto$date[idx]
  fdate = first(alldates)
  
  ystart = year(fdate)
  mstart = month(fdate)
  sgdp = ts(cs_gdp_constantlc_q_gasto$gdp[idx], start = c(ystart, mstart), frequency = 4)
  
  gdp_decom = decompose(sgdp)$trend
  gdp_seas = final(seas(sgdp))
  gdp_stl_list = stl(sgdp, s.window = "periodic")
  gdp_stl = gdp_stl_list$time.series[ , 2]
  cs_gdp_constantlc_q_gasto$gdp_sa_seas[idx] <- gdp_seas
  cs_gdp_constantlc_q_gasto$gdp_sa_stl[idx] <- gdp_stl
}

###
gdp_iso <- unique(cs_gdp_currentlc_q_activ$iso2c)
cs_gdp_currentlc_q_activ$gdp_sa_seas <- NA
cs_gdp_currentlc_q_activ$gdp_sa_stl <- NA

for (country in gdp_iso) {
  idx <- which(cs_gdp_currentlc_q_activ$iso2c == country)
  alldates = cs_gdp_currentlc_q_activ$date[idx]
  fdate = first(alldates)
  
  ystart = year(fdate)
  mstart = month(fdate)
  sgdp = ts(cs_gdp_currentlc_q_activ$gdp[idx], start = c(ystart, mstart), frequency = 4)
  
  gdp_decom = decompose(sgdp)$trend
  gdp_seas = final(seas(sgdp))
  gdp_stl_list = stl(sgdp, s.window = "periodic")
  gdp_stl = gdp_stl_list$time.series[ , 2]
  cs_gdp_currentlc_q_activ$gdp_sa_seas[idx] <- gdp_seas
  cs_gdp_currentlc_q_activ$gdp_sa_stl[idx] <- gdp_stl
}



gdp_iso <- unique(cs_gdp_constantlc_q_activ$iso2c)
cs_gdp_constantlc_q_activ$gdp_sa_seas <- NA
cs_gdp_constantlc_q_activ$gdp_sa_stl <- NA

for (country in gdp_iso) {
  idx <- which(cs_gdp_constantlc_q_activ$iso2c == country)
  alldates = cs_gdp_constantlc_q_activ$date[idx]
  fdate = first(alldates)
  
  ystart = year(fdate)
  mstart = month(fdate)
  sgdp = ts(cs_gdp_constantlc_q_activ$gdp[idx], start = c(ystart, mstart), frequency = 4)
  
  gdp_decom = decompose(sgdp)$trend
  gdp_seas = final(seas(sgdp))
  gdp_stl_list = stl(sgdp, s.window = "periodic")
  gdp_stl = gdp_stl_list$time.series[ , 2]
  cs_gdp_constantlc_q_activ$gdp_sa_seas[idx] <- gdp_seas
  cs_gdp_constantlc_q_activ$gdp_sa_stl[idx] <- gdp_stl
}




###


rm(list = ls(pattern = "cepalstat"))


# rm(cepalstat)

svdir <- "./produced_data/data_with_basic_wrangling/"



save(cs_real_dolares, file = paste0(svdir,"cs_real_dolares"))
save(cs_financiero_monetario_anual, file = paste0(svdir,"cs_financiero_monetario_anual"))
save(cs_financiero_monetario_trimestral, file = paste0(svdir,"cs_financiero_monetario_trimestral"))
save(cs_financiero_monetario_mensual, file = paste0(svdir,"cs_financiero_monetario_mensual"))
save(cs_remuneraciones_anual, file = paste0(svdir,"cs_remuneraciones_anual"))
save(cs_remuneraciones_trimestral, file = paste0(svdir,"cs_remuneraciones_trimestral"))
save(cs_empleo, file = paste0(svdir,"cs_empleo"))
save(cs_desempleo_anual, file = paste0(svdir,"cs_desempleo_anual"))
save(cs_desempleo_trimestral, file = paste0(svdir,"cs_desempleo_trimestral"))
save(cs_bp_anual, file = paste0(svdir,"cs_bp_anual"))
save(cs_bp_trimestral, file = paste0(svdir,"cs_bp_trimestral"))
save(cs_deuda_externa, file = paste0(svdir,"cs_deuda_externa"))
save(cs_indicadores_bp, file = paste0(svdir,"cs_indicadores_bp"))
save(cs_sector_publico, file = paste0(svdir,"cs_sector_publico"))
save(cs_comercio_intrarregional, file = paste0(svdir,"cs_comercio_intrarregional"))
save(cs_x_m_gran_cat, file = paste0(svdir,"cs_x_m_gran_cat"))
save(cs_x_m_10_ppales, file = paste0(svdir,"cs_x_m_10_ppales"))
save(cs_x_m_servicios, file = paste0(svdir,"cs_x_m_servicios"))
save(cs_x_m_total_mensual, file = paste0(svdir,"cs_x_m_total_mensual"))
save(cs_x_m_total_trimestral, file = paste0(svdir,"cs_x_m_total_trimestral"))
save(cs_x_prim_manuf, file = paste0(svdir,"cs_x_prim_manuf"))
save(cs_tipo_cambio, file = paste0(svdir,"cs_tipo_cambio"))
save(cs_x_m_vol_precios, file = paste0(svdir,"cs_x_m_vol_precios"))
save(cs_ipc_mensual, file = paste0(svdir,"cs_ipc_mensual"))
save(cs_ipc_anual, file = paste0(svdir,"cs_ipc_anual"))
save(cs_agricultura, file = paste0(svdir,"cs_agricultura"))
save(cs_mineria_manuf, file = paste0(svdir,"cs_mineria_manuf"))
save(cs_turismo, file = paste0(svdir,"cs_turismo"))
save(cs_precios_combustibles, file = paste0(svdir,"cs_precios_combustibles"))
save(cs_real_mn_trimestral, file = paste0(svdir,"cs_real_mn_trimestral"))

save(cs_gdp_currentlc_q_gasto, file = paste0(svdir,"cs_gdp_currentlc_q_gasto"))
save(cs_gdp_constantlc_q_gasto, file = paste0(svdir,"cs_gdp_constantlc_q_gasto"))
save(cs_gdp_currentlc_q_activ, file = paste0(svdir,"cs_gdp_currentlc_q_activ"))
save(cs_gdp_constantlc_q_activ, file = paste0(svdir,"cs_gdp_constantlc_q_activ"))


