# use read_chunk to insert this in a Rmd doc


# temporary place for functions
add_baselines <- function(df, value_colname = "value", date_colname = "date",
                          init_date = as.Date("2007", format = "%Y"),
                          final_date = as.Date("2016", format = "%Y"),
                          init_window = 3, final_window = 3) {
  
  wrapr::let(alias = list(value_col = value_colname, date_col = date_colname),
             expr = {
    
               in_win = year(init_date) - init_window + 1
               fi_win = year(final_date) - final_window + 1
               
               print(in_win)
               print(fi_win)
                          
    df_init <- df %>%
      filter(year(date_col) >= in_win & year(date_col) <= year(init_date)) %>% 
      group_by(iso3c) %>% 
      summarise(init_avg = mean(value, na.rm = TRUE),
                init_val = dplyr::last(value))
    
    df_final <- df %>%
      filter(year(date_col) >= fi_win & year(date_col) <= year(final_date)) %>% 
      group_by(iso3c) %>% 
      summarise(final_avg = mean(value, na.rm = TRUE),
                final_val = dplyr::last(value))
    
    df_infi <- left_join(df_init, df_final, by = "iso3c") %>% 
      mutate(dif_values = final_val - init_val,
             dif_avgs = final_avg - init_avg)
               
             })
  
}




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

# pre_path <- "~/GitHub/cap_estudio_2017/"
pre_path <- 'V:/USR/RMAYER/cw/cap_estudio_2017/'

source(paste0(pre_path, "functions/cate_dext.R"))

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
                                    is_med = FALSE, 
                                    is_pct4 = TRUE,
                                    value_col_name = "value",
                                    dating_col_name = "date")


dcfinsec_to_gdp_init <- dcfinsec_to_gdp %>% 
  filter(year(date) > 2004 & year(date) < 2008) %>% 
  group_by(iso3c) %>% 
  summarise(init_avg = mean(value, na.rm = TRUE),
            init_val = dplyr::last(value))


dcfinsec_to_gdp_final <- dcfinsec_to_gdp %>% 
  filter(year(date) > (2016-3+1) & year(date) < 2016) %>% 
  group_by(iso3c) %>% 
  summarise(final_avg = mean(value, na.rm = TRUE),
            final_val = dplyr::last(value))


dcfinsec_to_gdp_infi <- left_join(dcfinsec_to_gdp_init,
                                  dcfinsec_to_gdp_final,
                                  by = "iso3c") %>% 
  mutate(dif_values = final_val - init_val,
         dif_avgs = final_avg - init_avg)

foo <- dcfinsec_to_gdp_infi
moo <- add_baselines(dcfinsec_to_gdp)





