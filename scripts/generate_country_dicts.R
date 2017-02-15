library(countrycode)
library(tidyverse)

# lac_wb = wb_cachelist$countries  %>% filter(regionID=="LCN") %>% select(iso2c, iso3c, country)




ca_sa_region = c("Central America", "South America")
sa_not_cepal = c("FLK", "GUF")
cepal_caribbean = c("ATG","BHS", "BRB", "CUB", "DMA",
                    "DOM", "GRD", "HTI", "JAM", "KNA",
                    "LCA", "VCT", "TTO")

cepal_33_countries <- countrycode_data %>% 
                select(iso2c, iso3c, country.name.en, country.name.es, region) %>% 
                filter(region %in% ca_sa_region | iso3c %in% cepal_caribbean) %>% 
                filter(! iso3c  %in%  sa_not_cepal) 
                

# fix the Trinidad yTobago entry and change it to Trinidad y Tabago
cepal_33_countries$country.name.es[cepal_33_countries$iso3c == "TTO"] <- "Trinidad y Tabago"


carib_minus_dom = c("ATG", "BHS", "BRB", "DMA", "GRD", 
                    "HTI", "KNA", "LCA", "TTO", "VCT")

other_to_drop = c("GUY", "SUR", "BLZ") 

# without caribbean (except cuba and dom rep) and without belize, suriname and guyana
cepal_20_countries = cepal_33_countries %>% 
  filter(!iso3c %in% carib_minus_dom) %>% 
  filter(!iso3c %in% other_to_drop)


save(cepal_33_countries, file = "./produced_data/cepal_33_countries")
save(cepal_20_countries, file = "./produced_data/cepal_20_countries")




