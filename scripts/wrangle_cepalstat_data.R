library(tidyverse)
library(countrycode)

load("./produced_data/cepal_33_countries")

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




# custom dictionary to convert spanish coutry names to english or iso3c or iso2c
# Necessary because coutrycode does not support yet country.name.es as valid origin, only as destination
cepal_names_es_en = countrycode_data %>% 
                    filter(iso3c %in% cepal_33_countries[["iso3c"]]) %>% 
                    select(iso2c, iso3c, country.name.en, country.name.es)

# Cepal uses Trinidad y Tabago and not Trinidad y Tobago, as in countrycodes data frame
tto_logical <- cepal_names_es_en$country.name.es == "Trinidad yTobago"
cepal_names_es_en[tto_logical, ] <- c("TT", "TTO", "Trinidad and Tobago", "Trinidad y Tabago")

save(cepal_names_es_en, file = "./produced_data/cepal_names_es_en")

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
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_financiero_monetario <- cepalstat_sector_financiero_monetario %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_remuneraciones <- cepalstat_remuneraciones %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_empleo <- cepalstat_empleo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_desempleo <- cepalstat_desempleo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_bp_anual <- cepalstat_BP_anual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_bp_trimestral <- cepalstat_BP_trimestral %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_deuda_externa <- cepalstat_deuda_externa %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_indicadores_bp <- cepalstat_indicadores_derivados_de_la_BP %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_sector_publico <- cepalstat_sector_publico %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_comercio_intrarregional <- cepalstat_comercio_intrarregional %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_gran_cat <- cepalstat_exp_imp_grandes_cat %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_10_ppales <- cepalstat_exp_imp_pro_ppal %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_servicios <- cepalstat_exp_imp_servicios %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_m_total_mensual <- cepalstat_exp_imp_totales_mensuales %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_x_prim_manuf <- cepalstat_exp_prim_manuf %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c))  %>% rename(nombre_pais = País)

cs_tipo_cambio <- cepalstat_tipo_de_cambio %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País) 

cs_x_m_vol_precios <- cepalstat_indic_vol_precios_imp_exp %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)






cs_ipc_mensual <- cepalstat_ipc_ipm_mensual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)

cs_ipc_anual <- cepalstat_ipc_ipm_anual %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_agricultura <- cepalstat_agricultura %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_mineria_manuf <- cepalstat_mineria_manuf %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_turismo <- cepalstat_turismo %>% 
  mutate(iso3c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(País, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = País)


cs_precios_combustibles <- cepalstat_precios_combustibles %>% 
  mutate(iso3c = countrycode(Países, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso3c")) %>% 
  mutate(iso2c = countrycode(Países, custom_dict = cepal_names_es_en, origin = "country.name.es", destination = "iso2c")) %>% 
  filter(!is.na(iso3c)) %>% rename(nombre_pais = Países)




carib_minus_dom = c("ATG", "BHS", "BRB", "DMA", "GRD", 
                    "HTI", "KNA", "LCA", "TTO", "VCT")

other_to_drop = c("GUY", "SUR", "BLZ") 

# without caribbean (except cuba and dom rep) and without belize, suriname and guyana
cepal_20 = cepal_33_countries %>% 
            filter(!iso3c %in% carib_minus_dom) %>% 
            filter(!iso3c %in% other_to_drop)

cepal_names_es_en_20 <- cepal_names_es_en %>% 
  filter(!iso3c %in% carib_minus_dom) %>% 
  filter(!iso3c %in% other_to_drop)
save(cepal_names_es_en_20, file="./produced_data/cepal_names_es_en_20")



cs_real_dolares_20 <- cs_real_dolares %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_financiero_monetario_20 <- cs_financiero_monetario %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_remuneraciones_20 <- cs_remuneraciones %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_empleo_20 <- cs_empleo %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_desempleo_20 <- cs_desempleo %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_bp_anual_20 <- cs_bp_anual %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_bp_trimestral_20 <- cs_bp_trimestral %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_deuda_externa_20 <- cs_deuda_externa %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_indicadores_bp_20 <- cs_indicadores_bp %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_sector_publico_20 <- cs_sector_publico %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_comercio_intrarregional_20 <- cs_comercio_intrarregional %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_gran_cat_20 <- cs_x_m_gran_cat %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_10_ppales_20 <- cs_x_m_10_ppales %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_servicios_20 <- cs_x_m_servicios %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_total_mensual_20 <- cs_x_m_total_mensual %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_prim_manuf_20 <- cs_x_prim_manuf %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_tipo_cambio_20 <- cs_tipo_cambio %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_x_m_vol_precios_20 <- cs_x_m_vol_precios %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_ipc_mensual <- cs_ipc_mensual %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_ipc_anual <- cs_ipc_anual %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_agricultura <- cs_agricultura %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_mineria_manuf <- cs_mineria_manuf %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_turismo <- cs_turismo %>% filter(iso3c %in% cepal_20[["iso3c"]])
cs_precios_combustibles <- cs_precios_combustibles %>% filter(iso3c %in% cepal_20[["iso3c"]])

save(cs_real_dolares_20, file = "./produced_data/cs_real_dolares_20")
save(cs_financiero_monetario_20, file = "./produced_data/cs_financiero_monetario_20")
save(cs_remuneraciones_20, file = "./produced_data/cs_remuneraciones_20")
save(cs_empleo_20, file = "./produced_data/cs_empleo_20")
save(cs_desempleo_20, file = "./produced_data/cs_desempleo_20")
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
save(cs_x_prim_manuf_20, file = "./produced_data/cs_x_prim_manuf_20")
save(cs_tipo_cambio_20, file = "./produced_data/cs_tipo_cambio_20")
save(cs_x_m_vol_precios_20, file = "./produced_data/cs_x_m_vol_precios_20")
save(cs_ipc_mensual, file = "./produced_data/cs_ipc_mensual")
save(cs_ipc_anual, file = "./produced_data/cs_ipc_anual")
save(cs_agricultura, file = "./produced_data/cs_agricultura")
save(cs_mineria_manuf, file = "./produced_data/cs_mineria_manuf")
save(cs_turismo, file = "./produced_data/cs_turismo")
save(cs_precios_combustibles, file = "./produced_data/cs_precios_combustibles")


