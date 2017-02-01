library(tidyverse)
library(countrycode)

load("./output/cepalstat_desempleo")
load("./output/cepalstat_empleo")
load("./output/cepalstat_remuneraciones")
load("./output/cepalstat_sector_financiero_monetario")
# load("./output/cepalstat_sector_real_dolares_anual")
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

top_16_gdp_countries = c("BRA", "MEX", "ARG", "VEN", 
                         "COL", "CHL", "PER", "ECU",
                         "DOM", "GTM", "CRI", "PAN",
                         "URY", "BOL", "PRY", "SLV")

carib_minus_dom = c("ATG", "BHS", "BRB", "DMA", "GRD", 
                    "HTI", "KNA", "LCA", "TTO", "VCT")

other_to_drop = c("GUY", "SUR", "BLZ")

latin_20 = cepal_33_countries %>% 
            filter(!iso3c %in% carib_minus_dom) %>% 
            filter(!iso3c %in% other_to_drop)




