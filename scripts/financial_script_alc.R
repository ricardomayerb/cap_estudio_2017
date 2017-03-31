# use read_chunk to insert this in a Rmd doc


# temporary place for functions


# preliminary chunks --------------------------------------------------------

## ---- libraries
library(printr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggthemes)
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
library(dplyr)


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


# load(paste0(pre_path,
#             "produced_data/data_with_basic_wrangling/WDI_Data_19_tidy"))


load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_reservish_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_interests_dfs"))




# credit related chunks ------------------------------------------------------
## ---- credit_by_the_financial_sector_dfs
dcfs_gdp <- dom_cred_providd_by_finsec_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcfs_gdp <- add_baselines(dcfs_gdp)
dcfs_gdp <- add_ts_filters(dcfs_gdp)

dcfs_gdp_pct4 <- cate_gen(dcfs_gdp,
                          is_med = FALSE, is_pct4 = TRUE,
                          value_col_name = "final_avg",
                          dating_col_name = "date") %>% 
  filter(!is.na(gen_group)) %>% 
  arrange(desc(final_avg))


## ---- credit_to_private_sector_by_banks_dfs
dcpsb_gdp <- dom_credit_to_priv_sec_by_banks_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcpsb_gdp <- add_baselines(dcpsb_gdp)
dcpsb_gdp <- add_ts_filters(dcpsb_gdp)

dcpsb_gdp_pct4 <- cate_gen(dcpsb_gdp,
                          is_med = FALSE, is_pct4 = TRUE,
                          value_col_name = "final_avg",
                          dating_col_name = "date") %>% 
  filter(!is.na(gen_group)) %>% 
  arrange(desc(final_avg))


## ---- cprisbks_plots
by_gen_groups <- dcpsb_gdp_pct4 %>% 
  arrange(desc(final_avg)) %>% 
  group_by(gen_group) %>% 
  nest() 

plots_dcpsb_pct4 <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                                              y=value, col=iso3c)) +
                      geom_line()  + theme_tufte() + 
                      geom_hline(yintercept = 0) +           
                      ggtitle("Domestic credit to private sector",
                      subtitle = "By the financial sector, as % of GDP"))) 

multiplot(plotlist = plots_dcpsb_pct4$gruplot, cols = 2)



xtimelim = c(as.Date("2005", format = "%Y"), as.Date("2016", format = "%Y"))
ylimc = c(-25, 30)

plots_dcpsb_pct4_m2007 <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                                                            y=value_m_val, col=iso3c)) +
                                geom_line()  + theme_tufte() + 
                                geom_hline(yintercept = 0) +
                                coord_cartesian(xlim = xtimelim, ylim = ylimc)  +            
                                ggtitle("Domestic credit to private sector",
                                        subtitle = "By the financial sector, as % of GDP"))) 

multiplot(plotlist = plots_dcpsb_pct4_m2007$gruplot, cols = 2)


p_hpc <- ggplot(dcpsb_gdp, aes(x=date, y=hp_cycle, col = iso3c)) + 
  theme_tufte() + geom_hline(yintercept = 0) +
geom_line() + coord_cartesian(xlim = xtimelim, ylim = c(-10, 25)) 
p_hpc


ylimc = c(-6, 10)
plots_dcpsb_pct4_bf <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                                       y=hp_cycle, col=iso3c)) +
                        geom_line() +  theme_tufte() +
                        geom_hline(yintercept = 0) +
                        coord_cartesian(xlim = xtimelim, ylim = ylimc)  +            
                        ggtitle("Cycle of DC to private sector",
                          subtitle = "By the financial sector, as % of GDP"))) 


multiplot(plotlist = plots_dcpsb_pct4_bf$gruplot, cols = 2)


# credit related chunks ------------------------------------------------------
## ---- credit_to_private_sector_dfs

dcps_gdp <- dom_credit_to_priv_sec_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcps_gdp <- add_baselines(dcps_gdp)
dcps_gdp <- add_ts_filters(dcps_gdp)

dcps_gdp_pct4 <- cate_gen(dcps_gdp,
                          is_med = FALSE, is_pct4 = TRUE,
                          value_col_name = "final_avg",
                          dating_col_name = "date") %>% 
  filter(!is.na(gen_group)) %>% 
  arrange(desc(final_avg))


dcps_gdp_gd <- dom_credit_to_priv_sec_to_gdp.GD %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcps_gdp_gd <- add_baselines(dcps_gdp_gd)
dcps_gdp_gd <- add_ts_filters(dcps_gdp_gd)

dcps_gdp_pct4_gd <- cate_gen(dcps_gdp_gd,
                          is_med = FALSE, is_pct4 = TRUE,
                          value_col_name = "final_avg",
                          dating_col_name = "date") %>% 
  filter(!is.na(gen_group)) %>% 
  arrange(desc(final_avg))



## ---- cpri_plots
by_gen_groups <- dcps_gdp_pct4 %>% 
  arrange(desc(final_avg)) %>% 
  group_by(gen_group) %>% 
  nest() 

plots_dcps_pct4 <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                                                            y=value, col=iso3c)) +
                                geom_line()  + theme_tufte() + 
                                geom_hline(yintercept = 0) +           
                                ggtitle("Domestic credit to private sector",
                                        subtitle = " % of GDP "))) 

multiplot(plotlist = plots_dcps_pct4$gruplot, cols = 2)




## ---- comparison_fin_vs_tot

arg_cp <- dcps_gdp %>% filter(iso3c == "ARG") %>% arrange(date)
arg_cpfs <- dcfs_gdp %>% filter(iso3c == "ARG") %>% arrange(date)
arg_cp_gd <- dcps_gdp_gd %>% filter(iso3c == "ARG") %>% arrange(date)

arg_cp$value_gd <- NA

arg_cp$value_fs <- arg_cpfs$value
arg_cp$value_gd[1:55] <- arg_cp_gd$value

p_arg <- ggplot(data = arg_cp, aes(x = date, y = value)) + geom_line() + 
  geom_line(aes(y = value_fs, col = "red")) +
  geom_line(aes(y = value_gd, col = "blue")) 
  

p_arg



