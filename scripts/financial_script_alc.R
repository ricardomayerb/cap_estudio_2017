 # use read_chunk to insert this in a Rmd doc


# preliminary chunks --------------------------------------------------------

## ---- libraries
library(printr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(gridBase)
library(gridExtra)
library(scales)
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
library(wbstats)

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
# coi_18_minus_br_mx_ven <-  coi_18[-which(coi_18 %in% c("BRA", "MEX", "VEN"))]
coi_sa <-  c("ARG", "BOL" ,"BRA", "CHL", "COL", "ECU",
             "PER", "PRY", "URY", "VEN")
# coi_sa_minus_BRA_VEN <- coi_sa[-which(coi_sa %in% c("BRA","VEN"))]
coi_ca <-  c("CRI", "DOM", "HND" ,"GTM", "PAN", "NIC", "SLV", "MEX")
# coi_ca_minus_mex <- coi_ca[-which(coi_ca %in% c("MEX"))]

# coi <- coi_sa_minus_BRA_VEN



## ---- load_data_sets
load(paste0(pre_path, 
            "produced_data/peak_trough_dates"))

load(paste0(pre_path, 
            "produced_data/cs_tipo_cambio_nominal"))

load(paste0(pre_path, 
            "produced_data/cs_tipo_cambio_real_efectivo_mensual"))

load(paste0(pre_path, 
            "produced_data/cs_tipo_cambio_real_efectivo_promedio_anual"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/monetary_fin_tidy"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_gdp_currentlc_q_gasto"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_deuda_externa"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_sector_publico"))


load(paste0(pre_path,
             "produced_data/data_with_basic_wrangling/WDI_33_selected_all"))
wdi_indic <- WDI_33_selected_vars %>% 
  select(indicator_name, indicator_code) %>% distinct()

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_reservish_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_interests_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_x_m_10_ppales"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/x_m_locations_products_tidy"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_sector_publico_clean_names"))


# financial related chunks dfs -----------------------------------------
## prestamos bancarios dfs -----------------  

## ---- prestamos_bancarios_qtr_from_dde_dfs
prestamos_bancarios_qtr <- cs_gdp_currentlc_q_gasto %>% 
  select(-c(iso3c, nombre_pais, Rubro, year)) %>% 
  left_join(prestamos_bancarios_33_tidy,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  mutate(total_to_gdp_seas = total/gdp_sa_seas,
         consumo_gdp_seas = consumo/gdp_sa_seas,
         hipotecario_gdp_seas = hipotecario/gdp_sa_seas,
         industrial_gdp_seas = industrial/gdp_sa_seas,
         comercial_gdp_seas = comercial/gdp_sa_seas)

pb <-  prestamos_bancarios_qtr %>% 
  filter(iso3c %in% coi_18) %>% 
  filter( iso3c != "COL") # remove colombia until fix in ci or gdp data (ratio is off by 3 orders of magnitude)

pb_tot <- pb %>% select(iso3c, date, total_to_gdp_seas)
pb_tot <- add_ts_filters(pb_tot, value_colname = "total_to_gdp_seas",
                         data_periodicity = "quarterly")
pb_tot <- add_diffrank(pb_tot, valuecol_name = "total_to_gdp_seas")


pb_hip <- pb %>% select(iso3c, date, hipotecario_gdp_seas) 
pb_hip <- add_ts_filters(pb_hip, value_colname = "hipotecario_gdp_seas",
                         data_periodicity = "quarterly")
pb_hip <- add_diffrank(pb_hip, valuecol_name = "hipotecario_gdp_seas")

pb_con <- pb %>% select(iso3c, date, consumo_gdp_seas)
pb_con <- add_ts_filters(pb_con, value_colname = "consumo_gdp_seas",
                         data_periodicity = "quarterly")
pb_con <- add_diffrank(pb_con, valuecol_name = "consumo_gdp_seas")


# combine credit dfs
pb_tot_tm <- pb_tot %>% 
  select(iso3c, date, total_to_gdp_seas, ranking, quartile, half,
         hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
  rename(value_tot = total_to_gdp_seas, 
         ranking_tot = ranking, quartile_tot = quartile, half_tot = half,
         hp_cycle_pct_tot = hp_cycle_pct, hp_trend_tot = hp_trend,
         diff_lastval_tot = diff_lastval, diff_avg3_tot = diff_avg3)

pb_con_tm <- pb_con %>% 
  select(iso3c, date, consumo_gdp_seas, ranking, quartile, half, 
         hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
  rename(value_con = consumo_gdp_seas, 
         ranking_con = ranking, quartile_con = quartile, half_con = half,
         hp_cycle_pct_con = hp_cycle_pct, hp_trend_con = hp_trend, 
         diff_lastval_con = diff_lastval, diff_avg3_con = diff_avg3)

pb_hip_tm <- pb_hip %>% 
  select(iso3c, date, hipotecario_gdp_seas, ranking, quartile, half,
         hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
  rename(value_hip = hipotecario_gdp_seas, 
         ranking_hip = ranking, quartile_hip = quartile, half_hip = half,
         hp_cycle_pct_hip = hp_cycle_pct, hp_trend_hip = hp_trend,
         diff_lastval_hip = diff_lastval, diff_avg3_hip = diff_avg3)

bankloans_totconhip <-  left_join(pb_con_tm, pb_hip_tm,
                              by = c("iso3c", "date"))
bankloans_totconhip <- left_join(bankloans_totconhip, pb_tot_tm,
                             by = c("iso3c", "date"))

## domestic credit dfs -----------------  

## ---- dom_credit_dfs_anual_wb
dcfs_gdp <- make_df_diff_hp(dom_cred_providd_by_finsec_to_gdp)
dcpsbk_gdp <- make_df_diff_hp(dom_credit_to_priv_sec_by_banks_to_gdp) 
dcps_gdp <- make_df_diff_hp(dom_credit_to_priv_sec_to_gdp)

# combine credit dfs
# dcfs_gdp_tm <- dcfs_gdp %>% 
#   select(iso3c, date, value, ranking, quartile, half,
#          hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
#   rename(value_fs = value, 
#          ranking_fs = ranking, quartile_fs = quartile, half_fs = half,
#          hp_cycle_pct_fs = hp_cycle_pct, hp_trend_fs = hp_trend,
#          diff_lastval_fs = diff_lastval, diff_avg3_fs = diff_avg3)

# dcps_gdp_tm <- dcps_gdp %>% 
#   select(iso3c, date, value, ranking, quartile, half, 
#          hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
#   rename(value_ps = value, 
#          ranking_ps = ranking, quartile_ps = quartile, half_ps = half,
#          hp_cycle_pct_ps = hp_cycle_pct, hp_trend_ps = hp_trend, 
#          diff_lastval_ps = diff_lastval, diff_avg3_ps = diff_avg3)
# 
# dcpsbk_gdp_tm <- dcpsbk_gdp %>% 
#   select(iso3c, date, value, ranking, quartile, half,
#          hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
#   rename(value_psbk = value, 
#          ranking_psbk = ranking, quartile_psbk = quartile, half_psbk = half,
#          hp_cycle_pct_psbk = hp_cycle_pct, hp_trend_psbk = hp_trend,
#          diff_lastval_psbk = diff_lastval, diff_avg3_psbk = diff_avg3)




dcfs_gdp_tm <- prepare_tm(dcfs_gdp, suffix = "fs")
dcps_gdp_tm <- prepare_tm(dcfs_gdp, suffix = "ps")
dcpsbk_gdp_tm <- prepare_tm(dcpsbk_gdp, suffix = "psbk")


credit_pspsbkfs <-  left_join(dcps_gdp_tm, dcpsbk_gdp_tm,
                              by = c("iso3c", "date"))
credit_pspsbkfs <- left_join(credit_pspsbkfs, dcfs_gdp_tm,
                             by = c("iso3c", "date"))

dcps_2006_tables <- make_tables_present_last(dcps_gdp_tm, 2006, suffix = "ps")
dcps_2007_tables <- make_tables_present_last(dcps_gdp_tm, 2007, suffix = "ps")


## ---- claims_on_gov_and_other_sectors
claims_on_gov <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "FS.AST.CGOV.GD.ZS") %>% 
  rename(date = year)
claim_gov <- make_df_diff_hp(claims_on_gov)


claims_on_otherdomestsec <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "FS.AST.DOMO.GD.ZS") %>% 
  rename(date = year)
claim_other <- make_df_diff_hp(claims_on_otherdomestsec)



### bank related indicators -------

## ---- NPL_bank_ratios_money_make_dfs
npl <- make_df_diff_hp(nplns_to_total)

bksum <- bank_cap_to_bank_ass %>% 
  group_by(iso2c) %>% 
  summarise(nobs = n())

bk_cap_to_ass <- make_df_diff_hp(bank_cap_to_bank_ass %>% 
                                   filter(iso2c != "NI"))

bk_liqres_to_ass <- make_df_diff_hp(bank_liq_res_to_bank_ass)

## ---- broad_money_growth
# Broad money growth (annual %), FM.LBL.BMNY.ZG
broad_money_growth_annual <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "FM.LBL.BMNY.ZG") %>% 
  rename(date = year)
b_money_growth_a <- make_df_diff_hp(broad_money_growth_annual)

### external debt indicators dfs -----------

## ---- risk_premium_on_lending

risk_premium_on_lending <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "FM.AST.CGOV.ZG.M3") %>% 
  rename(date = year)
dom_risk_premium_wdi <- make_df_diff_hp(risk_premium_on_lending)

## ---- new_external_debt_maturity_interests_dfs
avg_int_new_extde_off    <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.INR.OFFT") %>% 
  rename(date = year)
int_new_extde_off <- make_df_diff_hp(avg_int_new_extde_off)


avg_int_new_extde_priv    <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.INR.PRVT") %>% 
  rename(date = year)
int_new_extde_priv <- make_df_diff_hp(avg_int_new_extde_priv)


avg_mat_new_extde_off    <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.MAT.OFFT") %>% 
  rename(date = year)
mat_new_extde_off <- make_df_diff_hp(avg_mat_new_extde_off)


avg_mat_new_extde_priv    <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.MAT.PRVT") %>% 
  rename(date = year)
mat_new_extde_priv <- make_df_diff_hp(avg_mat_new_extde_priv)


## ---- composition_and_relative_size_ext_debt_dfs 
# External debt stocks, short-term (DOD, current US$)
ext_debt_short_usd   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.DOD.DSTC.CD") %>% 
  rename(date = year)
edebt_short_usd <- make_df_diff_hp(ext_debt_short_usd)


# External debt stocks, long-term (DOD, current US$)
ext_debt_long_usd   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.DOD.DLXF.CD") %>% 
  rename(date = year)
edebt_long_usd <- make_df_diff_hp(ext_debt_long_usd)


# External debt stocks, total (DOD, current US$)
ext_debt_total_usd   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.DOD.DECT.CD") %>% 
  rename(date = year)
edebt_total_usd <- make_df_diff_hp(ext_debt_total_usd)


# External debt stocks, variable rate (DOD, current US$)
ext_debt_variable_usd   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.DOD.VTOT.CD") %>% 
  rename(date = year)
edebt_variab_usd <- make_df_diff_hp(ext_debt_variable_usd)


# External debt stocks (% of GNI)
# ext_debt_total_gni   <- WDI_33_selected_vars %>% 
#   filter(indicator_code ==  "DT.DOD.DECT.GN.ZS") %>% 
#   rename(date = year)
ext_debt_total_gni   <- wb(country = coi_18,
                           indicator = "DT.DOD.DECT.GN.ZS")


edebt_total_gni <- make_df_diff_hp(ext_debt_total_gni)


# Interest payments on external debt, short-term (INT, current US$)
int_payment_on_short_ext_usd   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.INT.DSTC.CD") %>% 
  rename(date = year)
int_pay_short_ext_usd <- make_df_diff_hp(int_payment_on_short_ext_usd)


# Short-term debt (% of total external debt)
# DT.DOD.DSTC.ZS
# ext_debt_short_to_total   <- WDI_33_selected_vars %>% 
#   filter(indicator_code ==  "DT.DOD.DSTC.ZS") %>% 
#   rename(date = year)

ext_debt_short_to_total   <- wb(country = coi_18,
                                indicator = "DT.DOD.DSTC.ZS")  
edebt_short_to_total <- make_df_diff_hp(ext_debt_short_to_total)

# Short-term debt (% of total reserves)
# DT.DOD.DSTC.IR.ZS

ext_debt_short_to_reserves <- wb(country = coi_18,
                                     indicator = "DT.DOD.DSTC.IR.ZS")  
  

# ext_debt_short_to_reserves   <- WDI_33_selected_vars %>% 
#   filter(indicator_code ==  "DT.DOD.DSTC.IR.ZS") %>% 
#   rename(date = year)

edebt_short_to_res <- make_df_diff_hp(ext_debt_short_to_reserves)


# Short-term debt (% of exports of goods, services and primary income)
# DT.DOD.DSTC.XP.ZS

# ext_debt_short_to_expo   <- WDI_33_selected_vars %>% 
#   filter(indicator_code ==  "DT.DOD.DSTC.XP.ZS") %>% 
#   rename(date = year)

ext_debt_short_to_export <- wb(country = coi_18,
                                 indicator = "DT.DOD.DSTC.XP.ZS")  

edebt_short_to_expo <- make_df_diff_hp(ext_debt_short_to_export)


# Debt service on external debt, total (TDS, current US$)
# DT.TDS.DECT.CD
ext_debt_short_to_reserves   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "DT.DOD.DSTC.IR.ZS") %>% 
  rename(date = year)
edebt_short_to_res <- make_df_diff_hp(ext_debt_short_to_reserves)

# Total reserves (% of total external debt)
# FI.RES.TOTL.DT.ZS
total_reserves_as_pct_total_ext_debt   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "FI.RES.TOTL.DT.ZS") %>% 
  rename(date = year)
reserves_to_edebt <- make_df_diff_hp(total_reserves_as_pct_total_ext_debt)

## ---- joining_financial_dfs

dom_cred_to_spriv_tm <- prepare_tm(dcps_gdp, "dcpri")
npl_tm <- prepare_tm(npl, "npl")
bk_cap_to_ass_tm <- prepare_tm(bk_cap_to_ass, "bca")
dom_risk_premium_wdi_tm <- prepare_tm(dom_risk_premium_wdi, "drp")
bk_liqres_to_ass_tm = prepare_tm(bk_liqres_to_ass, "bliq")

dom_fin_df <- left_join(dom_cred_to_spriv_tm, npl_tm, by = c("iso3c", "date")) %>%
  left_join(bk_cap_to_ass_tm, by = c("iso3c", "date")) %>%
  left_join(dom_risk_premium_wdi_tm, by = c("iso3c", "date")) %>% 
  left_join(bk_liqres_to_ass_tm,  by = c("iso3c", "date"))



edebt_short_to_res_tm <- prepare_tm(edebt_short_to_res, "shre")
edebt_short_to_total_tm <- prepare_tm(edebt_short_to_total, "sht")
edebt_short_to_expo_tm <- prepare_tm(edebt_short_to_expo, "shexp")

edebt_join_dfs <- left_join(edebt_short_to_res_tm, 
                            edebt_short_to_total_tm, 
                            edebt_short_to_expo_tm ,
                            by = c("iso3c", "date") )


extshort_res_2007 = make_tables_present_last(edebt_join_dfs , 2007, "shre")
extshort_tot_2007 = make_tables_present_last(edebt_join_dfs , 2007, "sht")
# extshort_exp_2007 = make_tables_present_last(edebt_join_dfs , 2007, "shexp")

extshort_res_2007$level_vars
extshort_tot_2007$level_vars
edebt_short_to_expo %>% filter( year(date) == 2015) %>% select(iso3c,  value)


bkcap_2007 = make_tables_present_last(dom_fin_df, 2007, "bca")
bkcap_2007$level_vars

bkliq_2007 =  make_tables_present_last(dom_fin_df, 2007, "bliq")
bkliq_2007$level_vars


# real sector variables dfs ----

### consumption and investment dfs ---------
## ---- capital_formation_and_consumption_make_dfs

# formerly private consumption
final_consum_households  <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.CON.PETC.ZS") %>% 
  rename(date = year)
consum_hh <- make_df_diff_hp(final_consum_households)

# Household final consumption expenditure, etc. (annual % growth) NE.CON.PETC.KD.ZG
growth_final_consum_households  <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.CON.PETC.KD.ZG") %>% 
  rename(date = year)
growth_consum_hh <- make_df_diff_hp(growth_final_consum_households)


gross_capital_form   <- WDI_33_selected_vars %>%
  filter(indicator_code ==  "NE.GDI.TOTL.ZS") %>%
  rename(date = year)
gcapf <- make_df_diff_hp(gross_capital_form)

# Gross capital formation (annual % growth) NE.GDI.TOTL.KD.ZG  
growth_gross_capital_form   <- WDI_33_selected_vars %>%
  filter(indicator_code ==  "NE.GDI.TOTL.KD.ZG") %>%
  rename(date = year)
growth_gcapf <- make_df_diff_hp(growth_gross_capital_form)


gross_fixed_capital_form   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.GDI.FTOT.ZS") %>% 
  rename(date = year)
gfixcapf <- make_df_diff_hp(gross_fixed_capital_form)

# Gross fixed capital formation (annual % growth) NE.GDI.FTOT.KD.ZG
growth_gross_fixed_capital_form   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.GDI.FTOT.KD.ZG") %>% 
  rename(date = year)
growth_gfixcapf <- make_df_diff_hp(growth_gross_fixed_capital_form)



gross_fixed_capital_form_priv_sect   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.GDI.FPRV.ZS") %>% 
  rename(date = year)
gfixcapf_priv <- make_df_diff_hp(gross_fixed_capital_form_priv_sect)




# Household final consumption expenditure (annual % growth)
# NE.CON.PRVT.KD.ZG
# 20
# Household final consumption expenditure per capita growth (annual %)
# NE.CON.PRVT.PC.KD.ZG
# 21


# GDP growth (annual %)
# NY.GDP.MKTP.KD.ZG
gdp_growth_annual   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NY.GDP.MKTP.KD.ZG") %>% 
  rename(date = year)
gdp_growth_a <- make_df_diff_hp(gdp_growth_annual)

### trade dimensions -----------

## ---- trade_share_of_gdp_make_dfs
# Trade (% of GDP)
# NE.TRD.GNFS.ZS
trade_as_pct_of_gdp   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.TRD.GNFS.ZS") %>% 
  rename(date = year)
trade_to_gdp <- make_df_diff_hp(trade_as_pct_of_gdp)

# Exports of goods and services (% of GDP) NE.EXP.GNFS.ZS
xgs_as_pct_of_gdp   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.EXP.GNFS.ZS") %>% 
  rename(date = year)
exp_g_s_gdp <- make_df_diff_hp(xgs_as_pct_of_gdp )

# Exports of goods and services (annual % growth) NE.EXP.GNFS.KD.ZG
xgs_growth   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.EXP.GNFS.KD.ZG") %>% 
  rename(date = year)
exp_g_s_growth <- make_df_diff_hp(xgs_growth)

# External balance on goods and services (% of GDP) NE.RSB.GNFS.ZS
external_balance_goods_service <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "NE.RSB.GNFS.ZS") %>% 
  rename(date = year)
trade_balance <- make_df_diff_hp(external_balance_goods_service)

## ---- current_account_dfs
current_account_balance_as_pct_of_gdp   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "BN.CAB.XOKA.GD.ZS") %>% 
  rename(date = year)
curr_acc_to_gdp <- make_df_diff_hp(current_account_balance_as_pct_of_gdp)


## ---- trade_concentration_index_make_dfs
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


cs_x_m_10_concen <-  cs_x_m_10_ppales_indiv %>% 
  group_by(iso3c, year) %>% 
  summarise(value = conc(valor, type = "Herfindahl")
  ) %>% 
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c")) %>% 
  rename(date = year) %>% 
  ungroup()

cs_x_m_10_herf <- make_df_diff_hp(cs_x_m_10_concen)


## ---- trade_shares_3_places_3_categories_dfs
imp_by_prod_to_join <- imp_by_prod_tidy %>% 
  filter(iso3c %in% coi_18) %>% 
  rename(producto_m = producto) 

exp_by_prod_to_join <- exp_by_prod_tidy %>% 
  filter(iso3c %in% coi_18) %>% 
  rename(producto_x = producto) %>% 
  mutate(producto_x = stri_trans_general(producto_x, "Latin-ASCII") %>% 
           str_to_lower() %>% str_replace_all(" ", "_"))

exp_by_prod_wide <- exp_by_prod_to_join %>% 
  filter(iso3c %in% coi_18) %>% 
  spread(producto_x, value) %>% 
  mutate(agro_agropec_share = productos_agricolas_y_agropecuarios/total,
         mineria_y_petro_share = mineria_y_petroleo/total,
         manufacturas_share = manufacturas/total) %>% 
  select(-c(productos_agricolas_y_agropecuarios,mineria_y_petroleo,
            manufacturas))

exp_by_prod_wide_anual <- exp_by_prod_wide %>% 
  mutate(year = year(date)) %>% 
  group_by(iso3c, year) %>% 
  mutate(avg_share_agr = mean(agro_agropec_share, rm.na = TRUE),
         avg_share_min_pet = mean(mineria_y_petro_share, rm.na = TRUE),
         avg_share_manuf = mean(manufacturas_share, rm.na = TRUE)) %>% 
  arrange(year, desc(avg_share_min_pet))


exp_by_prod_shares <- exp_by_prod_wide_anual %>% 
  rename(agro_pec=agro_agropec_share, mineria_y_petro=mineria_y_petro_share,
         manuf=manufacturas_share) %>% 
  gather(key = product, value = share, agro_pec, mineria_y_petro, manuf ) %>% 
  select(-c(avg_share_agr, avg_share_min_pet, avg_share_manuf, total)) %>% 
  arrange(iso3c, year, product)

exp_by_prod_shares_anual_avg <- exp_by_prod_shares %>% 
  mutate(year_as_date = as.Date(as.character(year), format = "%Y")) %>% 
  group_by(iso3c, year_as_date, product) %>% 
  summarise(share_anual = mean(share, rm.na=TRUE)) 

exp_agro_pec_shares_anual_avg <- exp_by_prod_shares_anual_avg %>% 
  filter(product == "agro_pec") %>% 
  mutate(date = year(year_as_date),
         value = share_anual) %>% 
  ungroup() %>% 
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c")) %>% 
  select(-year_as_date)
exp_agro_pec_shares <- make_df_diff_hp(exp_agro_pec_shares_anual_avg)

exp_manuf_shares_anual_avg <- exp_by_prod_shares_anual_avg %>% 
  filter(product == "manuf") %>% 
  mutate(date = year(year_as_date),
         value = share_anual) %>% 
  ungroup() %>% 
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c")) %>% 
  select(-year_as_date)
exp_manuf_shares <- make_df_diff_hp(exp_manuf_shares_anual_avg)

exp_mineria_y_petro_pec_shares_anual_avg <- exp_by_prod_shares_anual_avg %>% 
  filter(product == "mineria_y_petro") %>% 
  mutate(date = year(year_as_date),
         value = share_anual) %>% 
  ungroup() %>% 
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c")) %>% 
  select(-year_as_date)
exp_minepet_shares <- make_df_diff_hp(exp_mineria_y_petro_pec_shares_anual_avg)


## ---- exchange_rates_dfs

tipo_de_cambio_nominal <- cs_tipo_cambio_nominal %>% 
  filter(iso3c %in% coi_18) %>% 
  rename(year = `Años`) %>% 
  select(-notas, -fuente)
tcn_cs <- make_df_diff_hp(tipo_de_cambio_nominal, type = "cs")
  
tipo_de_cambio_real_efectivo_mensual <- cs_tipo_cambio_real_efectivo_mensual %>% 
  filter(iso3c %in% coi_18) %>% 
  rename(year = `Años`) %>% 
  select(-notas, -fuente)
tcre_m_cs <- make_df_diff_hp(tipo_de_cambio_real_efectivo_mensual, type = "cs")

tipo_de_cambio_real_efectivo_promedio_anual <- cs_tipo_cambio_real_efectivo_promedio_anual %>% 
  filter(iso3c %in% coi_18) %>% 
  rename(year = `Años`) %>% 
  select(-notas, -fuente)
tcre_pan_cs <- make_df_diff_hp(tipo_de_cambio_real_efectivo_promedio_anual, type = "cs")


### government variables   ----------------

## ---- government_operations_and_debt_dfs

# Revenue, excluding grants (% of GDP)
# GC.REV.XGRT.GD.ZS
revenue_excluding_grants_pct_gdp   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "GC.REV.XGRT.GD.ZS") %>% 
  rename(date = year)
revenue_to_gdp <- make_df_diff_hp(revenue_excluding_grants_pct_gdp)

# Tax revenue (% of GDP)
# GC.TAX.TOTL.GD.ZS
tax_revenue_as_pct_gdp   <- WDI_33_selected_vars %>% 
  filter(indicator_code ==  "GC.TAX.TOTL.GD.ZS") %>% 
  rename(date = year)
tax_revenue_to_gdp <- make_df_diff_hp(tax_revenue_as_pct_gdp)


ingtrib_gdp <- cs_sector_publico %>% 
  filter(indicador == 
           "ingresos_tributarios_por_tipo_de_impuestos_en_porcentajes_del_pib_america_latina" ) %>% 
  select(-c(6:13))

ingtrib_gdp_gobcen <- ingtrib_gdp %>% 
  filter(cobertura_institucional == "gobierno_central")
ingtrib_gdp_gobgen <- ingtrib_gdp %>%
  filter(cobertura_institucional == "gobierno_general")

opegob_gdp <- cs_sector_publico %>% 
  filter(indicador == 
           "operaciones_del_gobierno_clasificacion_economica_en_porcentajes_del_pib" ) %>% 
  select(-c(3, 4, 6, 7, 8, 9, 12, 13))

opegob_gdp_cen <- opegob_gdp %>% 
  filter(cobertura_institucional_1 == "gobierno_central" )

opegob_gdp_gen <- opegob_gdp %>% 
  filter(cobertura_institucional_1 == "gobierno_general" )
  
opegob_gdp_spnf <- opegob_gdp %>% 
  filter(cobertura_institucional_1 == "sector_publico_no_financiero" )

pagos_int_gcen <- opegob_gdp_cen %>% 
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "pagos_de_intereses" )

pagos_int_ggen <- opegob_gdp_gen %>% 
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "pagos_de_intereses" )

pagos_int_spnf <- opegob_gdp_spnf %>% 
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "pagos_de_intereses"  )

pay_int_gcen <- make_df_diff_hp(pagos_int_gcen, type = "cs")
pay_int_ggen <- make_df_diff_hp(pagos_int_ggen, type = "cs")
pay_int_spnf <- make_df_diff_hp(pagos_int_spnf, type = "cs")

resul_prim_gcen <- opegob_gdp_cen %>%
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "resultado_primario" )

resul_prim_ggen <- opegob_gdp_gen %>%
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "resultado_primario" )

resul_prim_spnf <- opegob_gdp_spnf %>%
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "resultado_primario" )

resul_prim_gcen <- make_df_diff_hp(resul_prim_gcen, type = "cs")
resul_prim_ggen <- make_df_diff_hp(resul_prim_ggen, type = "cs")
resul_prim_spnf <- make_df_diff_hp(resul_prim_spnf, type = "cs")

resul_global_gcen <- opegob_gdp_cen %>%
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "resultado_global" )

resul_global_ggen <- opegob_gdp_gen %>%
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "resultado_global" )

resul_global_spnf <- opegob_gdp_spnf %>%
  filter(clasificacion_economica_operaciones_del_gobierno ==
           "resultado_global" )

resul_global_gcen <- make_df_diff_hp(resul_global_gcen, type = "cs")
resul_global_ggen <- make_df_diff_hp(resul_global_ggen, type = "cs")
resul_global_spnf <- make_df_diff_hp(resul_global_spnf, type = "cs")
#
#
saldo_deuda_pub <- cs_sector_publico %>%
  filter(indicador == "saldo_de_la_deuda_publica_en_porcentajes_del_pib") %>%
  select(-c(3,4,6:11))
#
saldo_deuda_pub_cen <- saldo_deuda_pub %>%
  filter(cobertura_institucional_2 == "gobierno_central" )

saldo_deuda_pub_sp <- saldo_deuda_pub %>%
  filter(cobertura_institucional_2 == "sector_publico" ) 

saldo_deuda_pub_spnf <- saldo_deuda_pub %>%
  filter(cobertura_institucional_2 == "sector_publico_no_financiero" )
  
saldo_deuda_pub_gsub <- saldo_deuda_pub %>%
  filter(cobertura_institucional_2 == "gobiernos_subnacionales"  )

saldo_deuda_pub_cen_tot <- saldo_deuda_pub_cen %>%
  filter(clasificacion_deuda == "total_deuda_publica_clasificacion_por_residencia")
saldo_deuda_pub_cen_ext <- saldo_deuda_pub_cen %>%
  filter(clasificacion_deuda == "deuda_externa")
saldo_deuda_pub_cen_dom <- saldo_deuda_pub_cen %>%
  filter(clasificacion_deuda == "deuda_interna")
#
#
saldo_deuda_pub_sp_tot <- saldo_deuda_pub_sp %>%
  filter(clasificacion_deuda == "total_deuda_publica_clasificacion_por_residencia")
saldo_deuda_pub_sp_ext <- saldo_deuda_pub_sp %>%
  filter(clasificacion_deuda == "deuda_externa")
saldo_deuda_pub_sp_dom <- saldo_deuda_pub_sp %>%
  filter(clasificacion_deuda == "deuda_interna")
#
#
saldo_deuda_pub_spnf_tot <- saldo_deuda_pub_spnf %>%
  filter(clasificacion_deuda == "total_deuda_publica_clasificacion_por_residencia")
saldo_deuda_pub_spnf_ext <- saldo_deuda_pub_spnf %>%
  filter(clasificacion_deuda == "deuda_externa")
saldo_deuda_pub_spnf_dom <- saldo_deuda_pub_spnf %>%
  filter(clasificacion_deuda == "deuda_interna")
#
#
saldo_deuda_pub_gsub_tot <- saldo_deuda_pub_gsub %>%
  filter(clasificacion_deuda == "total_deuda_publica_clasificacion_por_residencia")
saldo_deuda_pub_gsub_ext <- saldo_deuda_pub_gsub %>%
  filter(clasificacion_deuda == "deuda_externa")
saldo_deuda_pub_gsub_dom <- saldo_deuda_pub_gsub %>%
  filter(clasificacion_deuda == "deuda_interna")

deuda_pub_cen_tot <- make_df_diff_hp(saldo_deuda_pub_cen_tot, type = "cs")
deuda_pub_cen_dom <- make_df_diff_hp(saldo_deuda_pub_cen_dom, type = "cs")
deuda_pub_cen_ext <- make_df_diff_hp(saldo_deuda_pub_cen_ext, type = "cs")

deuda_pub_sp_tot <- make_df_diff_hp(saldo_deuda_pub_sp_tot, type = "cs")
deuda_pub_sp_dom <- make_df_diff_hp(saldo_deuda_pub_sp_dom, type = "cs")
deuda_pub_sp_ext <- make_df_diff_hp(saldo_deuda_pub_sp_ext, type = "cs")

deuda_pub_spnf_tot <- make_df_diff_hp(saldo_deuda_pub_spnf_tot, type = "cs")
deuda_pub_spnf_dom <- make_df_diff_hp(saldo_deuda_pub_spnf_dom, type = "cs")
deuda_pub_spnf_ext <- make_df_diff_hp(saldo_deuda_pub_spnf_ext, type = "cs")

deuda_pub_gsub_tot <- make_df_diff_hp(saldo_deuda_pub_gsub_tot, type = "cs")
deuda_pub_gsub_dom <- make_df_diff_hp(saldo_deuda_pub_gsub_dom, type = "cs")
deuda_pub_gsub_ext <- make_df_diff_hp(saldo_deuda_pub_gsub_ext, type = "cs")






## ---- joining_real_sector_dfs

trade_to_gdp_tm <- prepare_tm(trade_to_gdp, "trade")
trade_balance_tm <- prepare_tm(trade_balance, "trbal")
curr_acc_to_gdp_tm <- prepare_tm(curr_acc_to_gdp, "cacc")
cs_x_m_10_herf_tm <- prepare_tm(cs_x_m_10_herf, "xherf")
exp_minepet_shares_tm <- prepare_tm(exp_minepet_shares, "minsh")
exp_manuf_shares_tm <- prepare_tm(exp_manuf_shares, "mansh")
exp_agro_pec_shares_tm <- prepare_tm(exp_agro_pec_shares, "agrsh")

trade_cacc_join <- left_join(trade_to_gdp_tm, trade_balance_tm,
                             by = c("iso3c", "date")) %>% 
  left_join(curr_acc_to_gdp_tm, by = c("iso3c", "date")) %>% 
  left_join(cs_x_m_10_herf_tm, by = c("iso3c", "date")) %>% 
  left_join(exp_minepet_shares_tm, by = c("iso3c", "date")) %>% 
  left_join(exp_manuf_shares_tm, by = c("iso3c", "date")) %>% 
  left_join(exp_agro_pec_shares_tm, by = c("iso3c", "date")) 

xmanuf_06 <-   make_tables_present_last(trade_cacc_join, 2006, "mansh")
xminpet_06 <-  make_tables_present_last(trade_cacc_join, 2006, "minsh")
xagro_06 <-   make_tables_present_last(trade_cacc_join, 2006, "agrsh") 


x_hh_06 <- make_tables_present_last(trade_cacc_join, 2006, "xherf")

cacc_06 <- make_tables_present_last(trade_cacc_join, 2006, "xherf")

deuda_pub_cen_tot_tm <- prepare_tm(deuda_pub_cen_tot, "dcen_tot") 
deuda_pub_cen_dom_tm <- prepare_tm(deuda_pub_cen_dom, "dcen_dom")
deuda_pub_cen_ext_tm <- prepare_tm(deuda_pub_cen_ext, "dcen_ext") 
deuda_pub_spnf_tot_tm <- prepare_tm(deuda_pub_spnf_tot, "dspnf_tot") 
deuda_pub_spnf_dom_tm <- prepare_tm(deuda_pub_spnf_dom, "dspnf_dom")
deuda_pub_spnf_ext_tm <- prepare_tm(deuda_pub_spnf_ext, "dspnf_ext") 
resul_prim_gcen_tm <- prepare_tm(resul_prim_gcen, "rp_cen") 
resul_global_gcen_tm <- prepare_tm(resul_global_gcen, "rp_cen") 
revenue_to_gdp_tm  <- prepare_tm(revenue_to_gdp, "rev")
tax_revenue_to_gdp_tm  <- prepare_tm(tax_revenue_to_gdp, "taxrev")
pay_int_gcen_tm  <- prepare_tm(pay_int_gcen, "intpay")

public_sect_join <- left_join(deuda_pub_cen_tot_tm, deuda_pub_cen_dom_tm,
                             by = c("iso3c", "date")) %>% 
  left_join(deuda_pub_cen_ext_tm, by = c("iso3c", "date")) %>% 
  left_join(deuda_pub_spnf_tot_tm, by = c("iso3c", "date")) %>% 
  left_join(deuda_pub_spnf_dom_tm , by = c("iso3c", "date")) %>% 
  left_join(deuda_pub_spnf_ext_tm, by = c("iso3c", "date")) %>% 
  left_join(resul_prim_gcen_tm, by = c("iso3c", "date")) %>% 
  left_join(resul_global_gcen_tm, by = c("iso3c", "date")) %>% 
  left_join(revenue_to_gdp_tm , by = c("iso3c", "date")) %>% 
  left_join(tax_revenue_to_gdp_tm , by = c("iso3c", "date")) %>% 
  left_join(pay_int_gcen_tm , by = c("iso3c", "date")) 



gdp_growth_a_tm <- prepare_tm(gdp_growth_a, "gdp_gr")
gfixcapf_priv_tm <- prepare_tm(gfixcapf_priv, "gfcf_pri")
gfixcapf_tm <- prepare_tm(gfixcapf, "gfcf")
growth_gfixcapf_tm <- prepare_tm(growth_gfixcapf, "gfcf_gr")
growth_gcapf_tm <- prepare_tm(growth_gcapf, "gcf_gr") 
gcapf_tm <- prepare_tm(gcapf, "gcf")
growth_consum_hh_tm <- prepare_tm(growth_consum_hh,"chh_gr") 
consum_hh_tm <- prepare_tm(consum_hh, "chh")


# finance related chunks plots --------------------------------------------------

## plot function definitions  ----------------------
## ---- plot_quick_dirty_fns
plot_1_lines <- function(var1,
                         le1 = "Dom cred to priv",
                         main_data = "credit_pspsbkfs", 
                         h_line_pos = 0, main = "", ylim= NULL,
                         dxlim = c(ymd("1989-12-31"), ymd("2015-12-31")),
                         coi = coi_18) {
  
  wrapr::let(alias = list(v1 = var1, pdata = main_data),
             expr = {
               ggplot(data = pdata %>% filter(iso3c %in% coi)) + 
                 geom_line(aes(x = date, y = v1, col = le1)) + 
                 geom_hline(yintercept = h_line_pos) + 
                 geom_rect(data = fin_dates_df,
                           aes(xmin = fn_peak, xmax = fn_trough, ymin = -Inf, ymax = +Inf),
                           fill = 'pink', alpha = 0.2) +
                 geom_rect(data = nber_dates_df,
                           aes(xmin = nb_peak, xmax = nb_trough, ymin = -Inf, ymax = +Inf),
                           fill = 'green', alpha = 0.1) +
                 coord_cartesian(xlim = dxlim,  
                                 ylim = ylim)   + 
                 facet_wrap( ~ iso3c, ncol = 3) +
                 ggtitle(main) + 
                 theme_tufte() 
             })
  
}



plot_2_lines <- function(var1 , var2,
                         le1 = "Dom cred to priv",
                         le2 = "Dcps by banks",
                         main_data = "credit_pspsbkfs", 
                         h_line_pos = 0, main = "", ylim= NULL,
                         dxlim = c(ymd("1989-12-31"), ymd("2015-12-31")),
                         coi = coi_18) {
  
  wrapr::let(alias = list(v1 = var1, v2 = var2, pdata = main_data),
             expr = {
               q_cred_lines <- ggplot(data = pdata %>% filter(iso3c %in% coi)) + 
                 geom_line(aes(x = date, y = v1, col = le1)) + 
                 geom_line(aes(x = date, y = v2, col = le2)) + 
                 geom_hline(yintercept = h_line_pos) + 
                 geom_rect(data = fin_dates_df,
                           aes(xmin = fn_peak, xmax = fn_trough, ymin = -Inf, ymax = +Inf),
                           fill='pink', alpha=0.2) +
                 geom_rect(data = nber_dates_df,
                           aes(xmin = nb_peak, xmax = nb_trough, ymin = -Inf, ymax = +Inf),
                           fill='green', alpha=0.1) +
                 coord_cartesian(xlim = dxlim,  
                                 ylim = ylim)   + 
                 facet_wrap( ~ iso3c, ncol = 3) +
                 ggtitle(main) + 
                 theme_tufte() 
             })
  
}


plot_cred_lines <- function(var1 , var2 , var3,
                            le1 = "Dom cred to priv",
                            le2 = "Dcps by banks",
                            le3 = "Dom cred by fin sec",
                            main_data = "credit_pspsbkfs", 
                            h_line_pos = 0, main = "", ylim= NULL,
                            dxlim = c(ymd("1989-12-31"), ymd("2015-12-31")),
                            coi = coi_18) {
  
  wrapr::let(alias = list(v1 = var1, v2 = var2, v3 = var3, pdata = main_data),
             expr = {
               g_cred_lines <- ggplot(data = pdata %>% filter(iso3c %in% coi)) + 
                 geom_line(aes(x = date, y = v1, col = le1)) + 
                 geom_line(aes(x = date, y = v2, col = le2)) + 
                 geom_line(aes(x = date, y = v3, col = le3)) + 
                 geom_hline(yintercept = h_line_pos) + 
                 geom_rect(data = fin_dates_df,
                           aes(xmin = fn_peak, xmax = fn_trough, ymin = -Inf, ymax = +Inf),
                           fill='pink', alpha=0.2) +
                 geom_rect(data = nber_dates_df,
                           aes(xmin = nb_peak, xmax = nb_trough, ymin = -Inf, ymax = +Inf),
                           fill='green', alpha=0.1) +
                 coord_cartesian(xlim = dxlim,  
                                 ylim = ylim)   + 
                 facet_wrap( ~ iso3c, ncol = 3) +
                 ggtitle(main) + 
                 theme_tufte() 
             })
  
}

## create credit plots ------------
## ---- combined_credit_plots_make
g_credit_pspsbkfs_ranking <- plot_cred_lines("ranking_ps", "ranking_psbk",
                                             "ranking_fs", main = "ranking",
                                             h_line_pos = 9)

g_credit_pspsbkfs_quartiles <- plot_cred_lines("quartile_ps", "quartile_psbk",
                                             "quartile_fs", main = "quartiles",
                                             h_line_pos = 2.5)

g_credit_pspsbkfs_halves <- plot_cred_lines("half_ps", "half_psbk", "half_fs", 
                                            main = "Upper and lower halves",
                                            h_line_pos = 1.5)

g_credit_pspsbkfs_cycle_pct <- plot_cred_lines("hp_cycle_pct_ps", "hp_cycle_pct_psbk",
                                               "hp_cycle_pct_fs", 
                                               main = "Deviation from trend, %",
                                               h_line_pos = 0,
                                               ylim = c(-20, 20))

g_credit_pspsbkfs_trend <- plot_cred_lines("hp_trend_ps", "hp_trend_psbk",
                                               "hp_trend_fs", 
                                               main = "Trend (HP)",
                                               h_line_pos = 0,
                                               ylim = c(0, 108))

g_credit_lastval_bar <- ggplot(data = credit_pspsbkfs %>% filter(year(date) == 2006), 
                    aes(x = iso3c, y = diff_lastval_ps,
                        label = format(diff_lastval_ps, digits = 1))) + 
  geom_bar(aes(fill = diff_lastval_ps), stat = "identity") + 
  geom_label(size=3, hjust = -0.5)  +
  coord_flip()

g_credit_lastavg_bar <- ggplot(data = credit_pspsbkfs %>% filter(year(date) == 2006),
                    aes(x = iso3c, y = diff_avg3_ps,
                        label = format(diff_avg3_ps, digits = 1 ))) + 
  geom_bar(aes(fill = diff_avg3_ps),
           stat = "identity") +  geom_label(size = 3, hjust = -0.5)  +
  coord_flip() 

## create bank loans plots ------------
## ---- pb_plots_make 

g_pb_totconhip_ranking <- plot_2_lines(
  "ranking_con", "ranking_hip", main = "ranking",
  le1 = "consumption", le2 = "mortages",
  h_line_pos = 6.5, main_data = "bankloans_totconhip",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))

g_pb_totconhip_quartiles <- plot_2_lines(
  "quartile_con", "quartile_hip", main = "quartiles",
   le1 = "consumption", le2 = "mortages",
   h_line_pos = 2.5, main_data = "bankloans_totconhip",
   dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))

g_pb_totconhip_halves <- plot_2_lines(
  "half_con", "half_hip", main = "halves",
  le1 = "consumption", le2 = "mortages",
  h_line_pos = 1.5, main_data = "bankloans_totconhip",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))

g_pb_totconhip_cycle_pct <- plot_2_lines(
  "hp_cycle_pct_con", "hp_cycle_pct_hip", main = "Cycle %",
  le1 = "consumption", le2 = "mortages",
  h_line_pos = 0, main_data = "bankloans_totconhip",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")),
  ylim = c(-11, 11))

g_pb_totconhip_trend <- plot_2_lines(
  "hp_trend_con", "hp_trend_hip", main = "Trend",
  le1 = "consumption", le2 = "mortages",
  h_line_pos = 0, main_data = "bankloans_totconhip",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))

g_pb_lastval_bar <- ggplot(data = bankloans_totconhip %>% filter(date == ymd("2006-10-01")), 
                               aes(x = iso3c, y = diff_lastval_con,
                                   label = format(diff_lastval_con, digits = 1))) + 
  geom_bar(aes(fill = diff_lastval_con), stat = "identity") + 
  geom_label(size = 3, hjust = -0.5)  +
  coord_flip()

g_pb_lastavg_bar <- ggplot(data = bankloans_totconhip %>% filter(date == ymd("2006-10-01")),
                               aes(x = iso3c, y = diff_avg3_con,
                                   label = format(diff_avg3_con, digits = 1 ))) + 
  geom_bar(aes(fill = diff_avg3_con),
           stat = "identity") +  geom_label(size = 3, hjust = -0.5)  +
  coord_flip() 



## create npl plots -------------

## ---- pb_plots_make 

g_npl_rankings <- plot_1_lines(
  "ranking", main = "ranking",
  le1 = "NPL % of total loans",
  h_line_pos = 9, main_data = "npl",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))

g_npl_quartiles <- plot_1_lines(
  "quartile", main = "quartiles",
  le1 = "NPL % of total loans",
  h_line_pos = 2.5, main_data = "npl",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))

g_npl_halves <- plot_1_lines(
  "half", main = "halves",
  le1 = "NPL % of total loans",
  h_line_pos = 1.5, main_data = "npl",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")))


g_npl_cycle_pct <- plot_1_lines(
  "hp_cycle_pct", main = "Cycle %",
  le1 = "NPL %",
  h_line_pos = 0, main_data = "npl",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")),
  ylim = c(-25, 25))

g_npl_trend <- plot_1_lines(
  "hp_trend", main = "trend (HP)",
  le1 = "trend",
  h_line_pos = 0, main_data = "npl",
  dxlim = c(ymd("1999-12-31"), ymd("2015-12-31")) )

g_npl_lastval_bar <- ggplot(data = npl %>% filter(date == ymd("2006-12-31")), 
                           aes(x = iso3c, y = diff_lastval,
                               label = format(diff_lastval, digits = 1))) + 
  geom_bar(aes(fill = diff_lastval), stat = "identity") + 
  geom_label(size = 3, hjust = -0.5)  +
  coord_flip()

g_npl_lastavg_bar <- ggplot(data = npl %>% filter(date == ymd("2006-12-31")),
                           aes(x = iso3c, y = diff_avg3,
                               label = format(diff_avg3, digits = 1 ))) + 
  geom_bar(aes(fill = diff_avg3),
           stat = "identity") +  geom_label(size = 3, hjust = -0.5)  +
  coord_flip() 

## print credit plots ----------

## ---- credit_plots_show

g_credit_pspsbkfs_ranking
g_credit_pspsbkfs_quartiles
g_credit_pspsbkfs_halves
g_credit_pspsbkfs_cycle_pct
g_credit_pspsbkfs_trend
g_credit_lastavg_bar
g_credit_lastval_bar

## print bank loans plots ----------

## ---- pb_plots_show

g_pb_totconhip_ranking
g_pb_totconhip_quartiles
g_pb_totconhip_halves
g_pb_totconhip_cycle_pct
g_pb_totconhip_trend
g_pb_lastval_bar
g_pb_lastavg_bar



## print non performing loans loans plots ----------
## ---- npl_plots_show

g_npl_rankings
g_npl_quartiles
g_npl_halves
g_npl_cycle_pct
g_npl_trend
g_npl_lastval_bar
g_npl_lastavg_bar

