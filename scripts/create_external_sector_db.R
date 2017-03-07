library(stringr)
library(lubridate)
library(tidyverse)

load("./produced_data/cs_bp_anual")
load("./produced_data/cs_bp_anual_20")
load("./produced_data/cs_bp_trimestral")
load("./produced_data/cs_bp_trimestral_20")

load("./produced_data/bop_edd_tidy")

load("./produced_data/x_m_locations_products_tidy")

load("./produced_data/debt_data_20_tidy")

load("./produced_data/cs_deuda_externa")
load("./produced_data/cs_deuda_externa_20")

# load("./produced_data/bis_gap_tc_dsr")
load("./produced_data/bis_tidy")

common_variables_a = c("iso2c", "indicador", "rubro", "year", "value",
                       "file_source")

cs_bp_anual_20_to_join <- cs_bp_anual_20 %>% 
  mutate(file_source = "cepalstat") %>% 
  rename(year = Años, rubro = Rubro, value = valor) %>% 
  select(c(iso2c, indicador, rubro, year, value,
           file_source))

bop_tidy_20_to_join <- bop_tidy_20 %>% 
  mutate(file_source = "edd", indicador = rubro) %>% 
  select(c(iso2c, indicador, rubro, year, value,
           file_source))
  
# bop_dde_cs <- full_join(x = bop_tidy_20, y = cs_bp_anual_20, 
#                         by = c("iso2c", "file_source", "rubro", "year"),
#                         suffix = c(".dde", ".cs")) %>% 
#   arrange(rubro, iso2c, year, file_source )

cs_deuda_externa_to_join <- cs_deuda_externa %>% 
  rename(year = Años, value = valor) %>% 
  mutate(rubro = indicador) %>% 
  mutate(file_source = "edd") %>% 
  select(c(iso2c, indicador, rubro, year, value,
           file_source))


bop_de_20 <- bind_rows(bop_tidy_20_to_join, cs_deuda_externa_to_join)


imp_by_prod_to_join <- imp_by_prod_tidy %>% 
  rename(producto_m = producto)

exp_by_prod_to_join <- exp_by_prod_tidy %>% 
  rename(producto_x = producto)
