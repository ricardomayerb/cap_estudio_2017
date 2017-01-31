library(tidyverse)
library(countrycode)

load("./output/cepalstat_desempleo")
load("./output/cepalstat_empleo")
load("./output/cepalstat_remuneraciones")
load("./output/cepalstat_sector_financiero_monetario")
load("./output/sector_real_dolares_anual_cepalstat")

 load("./output/cepal_33_countries")


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

spanish_to_iso3c = list("Antigua y Barbuda" = "ATG", "Argentina" = "ARG",
                        "Bahamas" = "BHS", "Barbados" = "BRB", "Belice" = "BLZ",
                        "Bolivia (Estado plurinacional de)" = "BOL", "Brasil" = "BRA", 
                        "Chile" = "CHL", "Colombia" =  "COL", "República Dominicana" = "DOM",
                        "Ecuador" = "ECU", "El Salvador" = "SLV", "Guatemala" = "GTM",
                        "Honduras" = "HND", "Jamaica" = "JAM", "México" = "MEX", 
                        "Nicaragua" = "NIC", "Panama" = "PAN", "Paraguay" = "PRY",
                        "Perú" = "PER", "Suriname" = "SUR", "Trinidad y Tobago" = "TTO",
                        "Uruguay" = "URY", "Venezuela (República Bolivariana de)")

                        
iso3c_to_spanish = list("BRA" = "Brasil", "DOM" = "República Dominicana")

foo = c("BRA", "DOM", "BOL")
moo = c("Brasil",   "República Dominicana", "Bolivia (Estado plurinacional de)")

spanish_to_iso3c[moo]




