#collect financial stress related variables
library(tidyverse)
library(stringr)
library(stringi)
library(DT)
library(lubridate)
library(ineq)

load("./produced_data/cepal_19_countries")
load("./produced_data/cepal_33_countries")

load("./produced_data/data_with_basic_wrangling/cs_deuda_externa")
load("./produced_data/data_with_basic_wrangling/cs_sector_publico")
load("./produced_data/data_with_basic_wrangling/cs_gdp_currentlc_q_gasto")
load("./produced_data/data_with_basic_wrangling/cs_gdp_currentlc_q_activ")
load("./produced_data/data_with_basic_wrangling/monetary_fin_tidy")
load("./produced_data/data_with_basic_wrangling/pib_anual_usd")


load("./produced_data/bop_edd_tidy")

load("./produced_data/data_with_basic_wrangling/x_m_locations_products_tidy")

load("./produced_data/data_with_basic_wrangling/cs_x_m_10_ppales")
load("./produced_data/data_with_basic_wrangling/cs_x_m_gran_cat")

## coi := countries of interest
# coi <-  cepal_19_countries$iso3c
coi <- c("BRA", "CHL", "CRI")

# total external debt as percentage of gdp (source: cepalstat)
dext_to_gdp <- cs_deuda_externa %>% 
  filter(iso3c %in% coi) %>% 
  filter(str_detect(indicador, "porcentaje")) %>% 
  rename(deuda_ext_porc_pib = valor,
         year = Años) 

sp_data <- cs_sector_publico %>% 
  filter(iso3c %in% coi) %>% 
  rename(year = Años)

sp_operaciones_porc_pib <- sp_data %>%
  filter( str_detect(indicador, "porcentajes")) %>% 
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

# members_operaciones_pib <- unique(sp_operaciones_porc_pib$operacion)

# (public sector's) operations of interest
opeoi <- c("resultado_global", "resultado_primario", "pagos_de_interes")

sp_opeoi_porc_pib <- sp_operaciones_porc_pib %>% 
  filter(operacion %in% opeoi) %>% 
  filter(!duplicated(.[ , 1:5])) %>% 
  spread(operacion, valor)


sp_deuda_porc_pib <- sp_data %>% 
  filter(`Cobertura institucional_2` != "n/a") %>%
  filter( str_detect(indicador, "porcentajes")) %>% 
  rename(cobertura =  `Cobertura institucional_2`) %>% 
  select(-c(`Tipo de persona`, Periodo, `Tasa mínima/máxima`, Periodo_1,
            `Clasificación económica Operaciones del gobierno`, 
            `Cobertura institucional_1`, `Cobertura institucional`,
            Clasificacion_impuestos)) %>% 
  rename(deuda_publica_porc_pib = valor, clasificacion_deuda = Clasificación_deuda) %>% 
  mutate(clasificacion_deuda = str_to_lower(clasificacion_deuda) %>% 
           str_replace_all(" ", "_") %>% stri_trans_general("Latin-ASCII") %>% 
           str_replace_all("\\(", "_") %>% str_replace_all("\\)", "_")
  ) %>% 
  filter(!duplicated(.[ , 1:5])) %>%  
  spread(clasificacion_deuda, deuda_publica_porc_pib) %>%
  rename(total_deuda_publica = total_deuda_publica__clasificacion_por_residencia_,
         deuda_externa_publica = deuda_externa,
         deuda_interna_publica = deuda_interna)

  
cv_no_na <- cartera_vencida_33_tidy %>% 
  filter(iso3c %in% coi) %>% 
  filter( !is.na(cartera_vencida_percent) )


cartera_vencida_qtr <- cs_gdp_currentlc_q_gasto %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( cartera_vencida_33_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(cartera_vencida_percent) ) %>% 
  filter(iso3c %in% coi) 


credito_interno_qtr <- cs_gdp_currentlc_q_gasto %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( credito_interno_33_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  filter(iso3c %in% coi) %>% 
  mutate(total_to_gdp_seas = total/gdp_sa_seas,
         al_spub_gdp_seas = al.sector.público/gdp_sa_seas,
         al_spriv_gdp_seas = al.sector.privado/gdp_sa_seas,
         al_gob_gdp_seas = gobierno/gdp_sa_seas,
         a_otros_gdp_seas = otros/gdp_sa_seas,
         total_to_gdp_stl = total/gdp_sa_stl,
         al_spub_gdp_stl = al.sector.público/gdp_sa_stl,
         al_spriv_gdp_stl = al.sector.privado/gdp_sa_stl,
         al_gob_gdp_stl = gobierno/gdp_sa_stl,
         a_otros_gdp_stl = otros/gdp_sa_stl)

prestamos_bancarios_qtr <- cs_gdp_currentlc_q_gasto %>% 
    select(-c(iso3c, nombre_pais, Rubro)) %>% 
    left_join(prestamos_bancarios_33_tidy,  by = c("iso2c", "date")) %>% 
    filter( !is.na(total) ) %>% 
    filter(iso3c %in% coi) %>% 
    mutate(total_to_gdp_stl = total/gdp_sa_stl,
           consumo_gdp_stl = consumo/gdp_sa_stl,
           hipotecario_gdp_stl = hipotecario/gdp_sa_stl,
           industrial_gdp_stl = industrial/gdp_sa_stl,
           comercial_gdp_stl = comercial/gdp_sa_stl)

tab_ci_qrt_gdp <- credito_interno_qtr %>% 
  group_by(iso2c, year) %>%
  summarise(nobs = n(), 
            num_countries = length(unique(credito_interno_qtr$iso2c)),
            avg_spu_gdp = mean(al_spub_gdp_stl, rm.na = TRUE),
            avg_spr_gdp = mean(al_spriv_gdp_stl, rm.na = TRUE),
            avg_tot_gdp = mean(total_to_gdp_stl, rm.na = TRUE),
            avg_gob_gdp = mean(al_gob_gdp_stl, rm.na = TRUE),
            n_gob_gdp = sum(!is.na(al_gob_gdp_stl)),
            n_tot_gdp = sum(!is.na(total_to_gdp_stl)),
            n_spub_gdp = sum(!is.na(al_spub_gdp_stl)),
            n_spr_gdp = sum(!is.na(al_spriv_gdp_stl))
  )


credito_interno_qtr <- cs_gdp_currentlc_q_gasto %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( credito_interno_20_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  mutate(total_to_gdp_seas = total/gdp_sa_seas,
         al_spub_gdp_seas = al.sector.público/gdp_sa_seas,
         al_spriv_gdp_seas = al.sector.privado/gdp_sa_seas,
         al_gob_gdp_seas = gobierno/gdp_sa_seas,
         a_otros_gdp_seas = otros/gdp_sa_seas,
         total_to_gdp_stl = total/gdp_sa_stl,
         al_spub_gdp_stl = al.sector.público/gdp_sa_stl,
         al_spriv_gdp_stl = al.sector.privado/gdp_sa_stl,
         al_gob_gdp_stl = gobierno/gdp_sa_stl,
         a_otros_gdp_stl = otros/gdp_sa_stl)

tab_pb_qtr <- prestamos_bancarios_qtr %>% group_by(iso2c, year) %>% 
  group_by(iso2c, year) %>%
  filter(iso2c != "CO") %>% 
  summarise(nobs = n(), 
            num_countries = length(unique(prestamos_bancarios_qtr$iso2c)),
            avg_tot_to_gdp = mean(total_to_gdp_stl),
            avg_hip_to_gdp = mean(hipotecario_gdp_stl, rm.na=TRUE),
            avg_con_to_gdp = mean(consumo_gdp_stl, rm.na=TRUE),
            n_hip_to_gdp = sum(!is.na(hipotecario_gdp_stl)),
            n_con_to_gdp = sum(!is.na(consumo_gdp_stl))
  )


imp_by_prod_to_join <- imp_by_prod_tidy %>% 
  rename(producto_m = producto) 

exp_by_prod_to_join <- exp_by_prod_tidy %>% 
  rename(producto_x = producto) %>% 
  mutate(producto_x = stri_trans_general(producto_x, "Latin-ASCII") %>% 
           str_to_lower() %>% str_replace_all(" ", "_"))

exp_by_prod_wide <- exp_by_prod_to_join %>% 
  spread(producto_x, value) %>% 
  mutate(agro_agropec_share = 100*productos_agricolas_y_agropecuarios/total,
         mineria_y_petro_share = 100*mineria_y_petroleo/total,
         manufacturas_share = 100*manufacturas/total) %>% 
  select(-c(productos_agricolas_y_agropecuarios,mineria_y_petroleo,
            manufacturas))

exp_by_prod_wide_anual <- exp_by_prod_wide %>% 
  mutate(year = year(date)) %>% 
  group_by(iso3c, year) %>% 
  mutate(avg_share_agr = mean(agro_agropec_share, rm.na = TRUE),
         avg_share_min_pet = mean(mineria_y_petro_share, rm.na = TRUE),
         avg_share_manuf = mean(manufacturas_share, rm.na = TRUE)) %>% 
  arrange(year, desc(avg_share_min_pet))





exp_10_ppales <- cs_x_m_10_ppales %>% 
  unite(productos_principales, contains("rincipales")) %>% 
  mutate(productos_principales = str_replace_all(productos_principales,
                                                 "_", "") %>% 
           str_replace_all("n/a", "") %>% 
           str_replace_all("NA", "")) %>% 
  rename(year = Años)

exp_10_ppales_by_iso <- exp_10_ppales %>% 
  group_by(iso3c, year) %>% 
  summarise(concentracion = conc(valor, type = "Herfindahl")) %>% 
  arrange(iso3c, year)


# imp_by_prod_shares <- exp_by_prod_to_join %>% 
#   group_by(iso3c, date) %>% 
  # summarise(agri_agropec_share = product)


# ing_trib <- sp_data %>% 
#   filter( str_detect(indicador, "porcentajes")) %>% 
#   filter(`Cobertura institucional` != "n/a") %>% 
#   rename(cobertura =  `Cobertura institucional`) %>% 
#   select(-c(`Tipo de persona`, Periodo, `Tasa mínima/máxima`, Periodo_1,
#             `Clasificación económica Operaciones del gobierno`, 
#             `Cobertura institucional_1`, `Cobertura institucional_2`,
#             Clasificación_deuda))
