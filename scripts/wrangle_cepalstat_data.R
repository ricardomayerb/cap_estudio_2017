library(tidyverse)
library(countrycode)

load("./output/cepalstat_desempleo")
load("./output/cepalstat_empleo")
load("./output/cepalstat_remuneraciones")
load("./output/cepalstat_sector_financiero_monetario")
load("./output/cepalstat_sector_real_dolares_anual")
load("./output/sector_real_dolares_anual_cepalstat")
load("./output/cepalstat_BP_anual")
load("./output/cepalstat_BP_trimestral")
load("./output/cepalstat_deuda_externa")
load("./output/cepalstat_indicadores_derivador_de_la_BP")


load("./output/cepal_33_countries")

# custom dictionary to convert spanish coutry names to english or iso3c or iso2c
# Necessary because coutrycode does not support yet country.name.es as valid origin, only as destination
cepal_names_es_en = countrycode_data %>% 
                    filter(iso3c %in% cepal_33_countries[["iso3c"]]) %>% 
                    select(iso2c, iso3c, country.name.en, country.name.es)

# Cepal uses Trinidad y Tabago and not Trinidad y Tobago, as in countrycodes data frame
tto_logical <- cepal_names_es_en$country.name.es == "Trinidad yTobago"
cepal_names_es_en[tto_logical, ] <- c("TT", "TTO", "Trinidad and Tobago", "Trinidad y Tabago")

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
cs_real_dolares <- cepalstat_sector_real_dolares_completo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_financiero_monetario <- cepalstat_sector_financiero_monetario %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_remuneraciones <- cepalstat_remuneraciones %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_empleo <- cepalstat_empleo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_desempleo <- cepalstat_desempleo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_bp_anual <- cepalstat_BP_anual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_bp_trimestral <- cepalstat_BP_trimestral %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_deuda_externa <- cepalstat_deuda_externa %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_indicadores_bp <- cepalstat_indicadores_derivador_de_la_BP %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_sector_publico <- cepalstat_sector_publico2 %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_comercio_intrarregional <- cepalstat_comercio_intrarregional %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_gran_cat <- cepalstat_exp_imp_grandes_cat %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_10_ppales_1_of_2 <- cepalstat_exp_imp_pro_ppal_part_1_of_2 %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_10_ppales_2_of_2 <- cepalstat_exp_imp_pro_ppal_part_2_of_2 %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_servicios <- cepalstat_exp_imp_servicios %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_total_mensual <- cepalstat_exp_imp_totales_mensuales %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_prim_manuf <- cepalstat_exp_prim_manuf %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_tipo_cambio <- cepalstat_tipo_de_cambio %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_vol_precios <- cepalstat_indic_vol_precios_imp_exp %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 

cs_x_m_10_ppales_1_of_2 <- cepalstat_ %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) 



carib_minus_dom = c("ATG", "BHS", "BRB", "DMA", "GRD", 
                    "HTI", "KNA", "LCA", "TTO", "VCT")

other_to_drop = c("GUY", "SUR", "BLZ") 

# without caribbean (except cuba and dom rep) and without belize, suriname and guyana
cepal_20 = cepal_33_countries %>% 
            filter(!iso3c %in% carib_minus_dom) %>% 
            filter(!iso3c %in% other_to_drop)


cs_real_dolares_20 <- cs_real_dolares %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_financiero_monetario_20 <- cs_financiero_monetario %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_remuneraciones_20 <- cs_remuneraciones %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_empleo_20 <- cs_empleo_20 %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_desempleo_20 <- cs_desempleo_20 %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_bp_anual_20 <- cs_bp_anual %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_bp_trimestral_20 <- cs_bp_trimestral %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_deuda_externa_20 <- cs_deuda_externa %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_indicadores_bp_20 <- cs_indicadores_bp %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_sector_publico <- cs_sector_publico %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_comercio_intrarregional_20 <- cs_comercio_intrarregional %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_gran_cat_20 <- cs_x_m_gran_cat %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_10_ppales_1_of_2_20 <- cs_x_m_10_ppales_1_of_2 %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_10_ppales_2_of_2_20 <- cs_x_m_10_ppales_2_of_2 %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_servicios_20 <- cs_x_m_servicios %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_total_mensual_20 <- cs_x_m_total_mensual %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_prim_manuf_20 <- cs_x_prim_manuf %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_tipo_cambio_20 <- cs_tipo_cambio %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_vol_precios_20 <- cs_x_m_vol_precios %>% filter(iso3c %in% cepal_20[["iso3c"]])

