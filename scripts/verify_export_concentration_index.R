# verify herfindahl calculations
library(tidyverse)
library(stringr)
library(ineq)


pre_path <- "~/GitHub/cap_estudio_2017/"
# pre_path <- 'V:/USR/RMAYER/cw/cap_estudio_2017/'
load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_x_m_10_ppales"))


cs_x_m_10_ppales_indiv <- cs_x_m_10_ppales %>%
  filter(!str_detect(`Productos principales`, "Total")) %>%
  filter(!str_detect(`Productos principales_1`, "Total")) %>%
  filter(!str_detect(`Productos principales_2`, "Total")) %>%
  filter(!str_detect(`Productos principales_3`, "Total")) %>%
  filter(!str_detect(`Productos principales_4`, "Total")) %>%
  filter(!str_detect(`Productos principales_5`, "Total")) %>%
  filter(!str_detect(`Productos principales_6`, "Total")) %>%
  filter(!str_detect(`Productos principales_7`, "Total")) %>%
  filter(!str_detect(`Productos principales_8`, "Total")) %>%
  filter(!str_detect(`Productos principales_9`, "Total")) %>%
  filter(!str_detect(`Productos principales_10`, "Total")) %>%
  filter(!str_detect(`Productos principales_11`, "Total")) %>%
  filter(!str_detect(`Productos principales_12`, "Total")) %>%
  filter(!str_detect(`Productos principales_13`, "Total")) %>%
  filter(!str_detect(`Productos principales_14`, "Total")) %>%
  filter(!str_detect(`Productos principales`, "Todos")) %>%
  filter(!str_detect(`Productos principales_1`, "Todos")) %>%
  filter(!str_detect(`Productos principales_2`, "Todos")) %>%
  filter(!str_detect(`Productos principales_3`, "Todos")) %>%
  filter(!str_detect(`Productos principales_4`, "Todos")) %>%
  filter(!str_detect(`Productos principales_5`, "Todos")) %>%
  filter(!str_detect(`Productos principales_6`, "Todos")) %>%
  filter(!str_detect(`Productos principales_7`, "Todos")) %>%
  filter(!str_detect(`Productos principales_8`, "Todos")) %>%
  filter(!str_detect(`Productos principales_9`, "Todos")) %>%
  filter(!str_detect(`Productos principales_10`, "Todos")) %>%
  filter(!str_detect(`Productos principales_11`, "Todos")) %>%
  filter(!str_detect(`Productos principales_12`, "Todos")) %>%
  filter(!str_detect(`Productos principales_13`, "Todos")) %>%
  filter(!str_detect(`Productos principales_14`, "Todos")) 
  

cs_x_m_10_herf <-  cs_x_m_10_ppales_indiv %>% 
  group_by(iso3c, year) %>% 
  summarise(concentracion = conc(valor, type = "Herfindahl"))


# exp_chl <- cs_x_m_10_ppales_indiv %>% 
#   filter(iso3c == "CHL") %>% 
#   group_by(year) %>% 
#   summarise(concentracion = conc(valor, type = "Herfindahl"))
# 
# exp_bra <- cs_x_m_10_ppales %>% 
#   filter(iso3c == "BRA") 
# 
# by_country <- cs_x_m_10_ppales %>% 
#   group_by(iso3c) %>% arrange()
#   nest()
