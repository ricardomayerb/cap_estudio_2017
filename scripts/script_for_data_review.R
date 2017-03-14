# this script contains all relevant code for data review document


## to emulate parametrized reports I will define a params df

params = data.frame(end_year = 2015, start_year = 1990)

end_year_for_filter = rep(params$start_year,2)


# prelimnary steps: libraries and dictionaries and country selection
library(tidyverse)
library(stringr)
library(stringi)
library(DT)
library(lubridate)
library(ineq)
library(knitr)


options(digits = 3)
load("./produced_data/cepal_19_countries")
load("./produced_data/cepal_33_countries")
# coi <-  cepal_19_countries$iso3c
coi <-  c("ARG", "BRA", "CHL")


# prepare tables re external debt
## make the tibble
load("./produced_data/data_with_basic_wrangling/cs_deuda_externa")
dext_to_gdp <- cs_deuda_externa %>% 
  filter(iso3c %in% coi) %>% 
  rename(deuda_ext_porc_pib = valor,
         year = Años) %>% 
  filter(year <= params$end_year) %>% 
  filter(str_detect(indicador, "porcentaje")) 


mr_3_dext_to_gdp <- dext_to_gdp %>% 
  select(iso3c, year, deuda_ext_porc_pib) %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  summarise(year_mr = max(year),
            deuda_ext_mr = last(deuda_ext_porc_pib),
            avg_mr_3 = mean(tail(deuda_ext_porc_pib, 3) , rm.na = TRUE)
  ) %>% 
  arrange(desc(deuda_ext_mr)) %>% 
  mutate(rnk_mr_dext = min_rank(-deuda_ext_mr),
         rnk_mr_3_dext = min_rank(-avg_mr_3)) %>% 
  arrange(iso3c)

rm(cs_deuda_externa)

## make the table
kb_mr_3_dext_to_gdp <- kable(mr_3_dext_to_gdp)

kb_dext_to_gdp <- kable(narr_dext_to_gdp)

dt_dext_to_gdp <- datatable(narr_dext_to_gdp)

## present the table
# dt_dext_to_gdp
kb_mr_3_dext_to_gdp


# more on debt data: public sector
load("./produced_data/data_with_basic_wrangling/cs_sector_publico")

sp_data <- cs_sector_publico %>% 
  filter(iso3c %in% coi) %>% 
  rename(year = Años)

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
  rename(total_p = total_deuda_publica__clasificacion_por_residencia_,
         externa_p = deuda_externa,
         interna_p = deuda_interna)

gcentral_deuda_pib <- sp_deuda_porc_pib %>% 
  filter(cobertura == "Gobierno central"  ) %>% 
  select(iso3c, year, externa_p, interna_p, total_p)

sectorpublico_deuda_pib <- sp_deuda_porc_pib %>% 
  filter(cobertura == "Sector público") %>% 
  select(iso3c, year, externa_p, interna_p, total_p)

gsubnac_deuda_pib <- sp_deuda_porc_pib %>% 
  filter(cobertura == "Gobiernos subnacionales") %>% 
  select(iso3c, year, externa_p, interna_p, total_p)

gspnf_deuda_pib <- sp_deuda_porc_pib %>% 
  filter(cobertura == "Sector público no financiero") %>% 
  select(iso3c, year, externa_p, interna_p, total_p)

mr_3_gcentral <- gcentral_deuda_pib %>% 
  select(iso3c, year, externa_p, interna_p) %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  summarise( year_mr = max(year),
            deuda_p_ext_mr = last(externa_p),
            avg_ext_p_mr_3 = mean(tail(externa_p, 3) , rm.na = TRUE),
            deuda_p_int_mr = last(interna_p),
            avg_int_p_mr_3 = mean(tail(interna_p, 3) , rm.na = TRUE)
  ) %>% 
  arrange(desc(deuda_p_ext_mr)) %>% 
  mutate(rnk_mr_ext_p = min_rank(-deuda_p_ext_mr),
         rnk_mr_3_ext_p = min_rank(-avg_ext_p_mr_3)) %>% 
  arrange(iso3c)


mr_3_desepub <- sectorpublico_deuda_pib %>% 
  select(iso3c, year, externa_p, interna_p) %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  summarise( year_mr = max(year),
             deuda_p_ext_mr = last(externa_p),
             avg_ext_p_mr_3 = mean(tail(externa_p, 3) , rm.na = TRUE),
             deuda_p_int_mr = last(interna_p),
             avg_int_p_mr_3 = mean(tail(interna_p, 3) , rm.na = TRUE)
  ) %>% 
  arrange(desc(deuda_p_ext_mr)) %>% 
  mutate(rnk_mr_ext_p = min_rank(-deuda_p_ext_mr),
         rnk_mr_3_ext_p = min_rank(-avg_ext_p_mr_3)) %>% 
  arrange(iso3c)

mr_3_gsubn <- gsubnac_deuda_pib %>% 
  select(iso3c, year, externa_p, interna_p) %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  summarise( year_mr = max(year),
             deuda_p_ext_mr = last(externa_p),
             avg_ext_p_mr_3 = mean(tail(externa_p, 3) , rm.na = TRUE),
             deuda_p_int_mr = last(interna_p),
             avg_int_p_mr_3 = mean(tail(interna_p, 3) , rm.na = TRUE)
  ) %>% 
  arrange(desc(deuda_p_ext_mr)) %>% 
  mutate(rnk_mr_ext_p = min_rank(-deuda_p_ext_mr),
         rnk_mr_3_ext_p = min_rank(-avg_ext_p_mr_3)) %>% 
  arrange(iso3c)


mr_3_spnf <- gspnf_deuda_pib %>% 
  select(iso3c, year, externa_p, interna_p) %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  summarise( year_mr = max(year),
             deuda_p_ext_mr = last(externa_p),
             avg_ext_p_mr_3 = mean(tail(externa_p, 3) , rm.na = TRUE),
             deuda_p_int_mr = last(interna_p),
             avg_int_p_mr_3 = mean(tail(interna_p, 3) , rm.na = TRUE)
  ) %>% 
  arrange(desc(deuda_p_ext_mr)) %>% 
  mutate(rnk_mr_ext_p = min_rank(-deuda_p_ext_mr),
         rnk_mr_3_ext_p = min_rank(-avg_ext_p_mr_3)) %>% 
  arrange(iso3c)

## make the tables
kb_mr_3_desepub <- kable(mr_3_desepub)
kb_mr_3_gcentral <- kable(mr_3_gcentral)
kb_mr_3_gsubn <- kable(mr_3_gsubn)
kb_mr_3_spnf <- kable(mr_3_spnf)

dt_mr_3_desepub <- datatable(mr_3_desepub)
dt_mr_3_gcentral <- datatable(mr_3_gcentral)
dt_mr_3_gsubn <- datatable(mr_3_gsubn)
dt_mr_3_spnf <- datatable(mr_3_spnf)

## present the table
kb_mr_3_desepub 
kb_mr_3_gcentral
kb_mr_3_gsubn 
kb_mr_3_spnf 

dt_mr_3_desepub 
dt_mr_3_gcentral 
dt_mr_3_gsubn 
dt_mr_3_spnf 


## prepare tables for non performing loans
load("./produced_data/data_with_basic_wrangling/monetary_fin_tidy")

cv_no_na <- cartera_vencida_33_tidy %>% 
  filter(iso3c %in% coi) %>% 
  filter( !is.na(cartera_vencida_percent) )

cv_december <- cartera_vencida_33_tidy %>% 
  filter(iso3c %in% coi) %>% 
  filter( month == 12 )

cartera_vencida_qtr <- cs_gdp_currentlc_q_gasto %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( cartera_vencida_33_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(cartera_vencida_percent) ) %>% 
  filter(iso3c %in% coi) 


