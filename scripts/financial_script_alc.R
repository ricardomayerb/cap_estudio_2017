# use read_chunk to insert this in a Rmd doc


# temporary place for functions



# preliminary chunks --------------------------------------------------------

## ---- libraries
library(printr)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(gridBase)
library(gridExtra)
library(stringr)
library(stringi)
library(lubridate)
library(ineq)
library(xts)
library(countrycode)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(width = 60)
options(digits = 3)


## ---- constants_and_functions
default_time_break <- as.Date("2005-12-31", format = "%Y-%m-%d") 

# time_breaks = 2001

# pre_path <- params$path_prefix

pre_path <- "~/GitHub/cap_estudio_2017/"
# pre_path <- 'V:/USR/RMAYER/cw/cap_estudio_2017/'

source(paste0(pre_path, "functions/funcs_for_cap_2017.R"))

load(paste0(pre_path, "produced_data/cepal_19_countries"))
load(paste0(pre_path, "produced_data/cepal_33_countries"))

coi_19 <- cepal_19_countries$iso3c
coi_18 <- coi_19[-which(coi_19 %in% c("CUB"))]
coi_18_minus_br_mx_ven <-  coi_18[-which(coi_18 %in% c("BRA", "MEX", "VEN"))]
coi_sa <-  c("ARG", "BOL" ,"BRA", "CHL", "COL", "ECU",  "PER", "PRY", "URY", "VEN")
coi_sa_minus_BRA_VEN <- coi_sa[-which(coi_sa %in% c("BRA","VEN"))]
coi_ca <-  c("CRI", "DOM", "HND" ,"GTM", "PAN", "NIC", "SLV", "MEX")
coi_ca_minus_mex <- coi_ca[-which(coi_ca %in% c("MEX"))]

coi <- coi_sa_minus_BRA_VEN



## ---- load_data_sets
load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/monetary_fin_tidy"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_gdp_currentlc_q_gasto"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_deuda_externa"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_sector_publico"))


load(paste0(pre_path,
            "produced_data/data_with_basic_wrangling/WDI_Data_19_tidy"))


load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_reservish_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_interests_dfs"))




# credit related chunks ------------------------------------------------------
## ---- credit_to_private_sector
dcfinsec_to_gdp <- dom_cred_providd_by_finsec_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y"))



dcfinsec_to_gdp_pct4 <- cate_gen(dcfinsec_to_gdp,
                            time_breaks = default_time_break,
                            is_med = FALSE, is_pct4 = TRUE,
                            value_col_name = "value", dating_col_name = "date")

dcfinsec_to_gdp_infi <- add_baselines(dcfinsec_to_gdp)


  



