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


## ---- constants_and_functions
default_time_break <- as.Date("2005-12-31", format = "%Y-%m-%d") 

# time_breaks = 2001

# pre_path <- params$path_prefix

# pre_path <- "~/GitHub/cap_estudio_2017/"
pre_path <- 'V:/USR/RMAYER/cw/cap_estudio_2017/'

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
            "produced_data/data_with_basic_wrangling/monetary_fin_tidy"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_gdp_currentlc_q_gasto"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_deuda_externa"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/cs_sector_publico"))


load(paste0(pre_path,
             "produced_data/data_with_basic_wrangling/WDI_33_selected_all"))


load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_reservish_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_interests_dfs"))

load(paste0(pre_path, 
            "produced_data/peak_trough_dates"))


# credit related chunks dfs ----------------------------------------------------

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
pb_tot <- add_diffrank(pb_tot, valuecol_name = "total_to_gdp_seas")
pb_tot <- add_ts_filters(pb_tot, value_colname = "total_to_gdp_seas",
                         hp_freq = 4)


pb_hip <- pb %>% select(iso3c, date, hipotecario_gdp_seas) 
pb_hip <- add_diffrank(pb_hip, valuecol_name = "hipotecario_gdp_seas")
pb_hip <- add_ts_filters(pb_hip, value_colname = "hipotecario_gdp_seas",
                         hp_freq = 4)

pb_con <- pb %>% select(iso3c, date, consumo_gdp_seas)
pb_con <- add_diffrank(pb_con, valuecol_name = "consumo_gdp_seas")
pb_con <- add_ts_filters(pb_con, value_colname = "consumo_gdp_seas",
                         hp_freq = 4)


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


## ---- dom_credit_dfs_anual_wb
dcfs_gdp <- dom_cred_providd_by_finsec_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcfs_gdp <- add_diffrank(dcfs_gdp)
dcfs_gdp <- add_ts_filters(dcfs_gdp)


dcpsbk_gdp <- dom_credit_to_priv_sec_by_banks_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcpsbk_gdp <- add_diffrank(dcpsbk_gdp)
dcpsbk_gdp <- add_ts_filters(dcpsbk_gdp)


dcps_gdp <- dom_credit_to_priv_sec_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcps_gdp <- add_diffrank(dcps_gdp)
dcps_gdp <- add_ts_filters(dcps_gdp)

# combine credit dfs
dcfs_gdp_tm <- dcfs_gdp %>% 
  select(iso3c, date, value, ranking, quartile, half,
         hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
  rename(value_fs = value, 
         ranking_fs = ranking, quartile_fs = quartile, half_fs = half,
         hp_cycle_pct_fs = hp_cycle_pct, hp_trend_fs = hp_trend,
         diff_lastval_fs = diff_lastval, diff_avg3_fs = diff_avg3)

dcps_gdp_tm <- dcps_gdp %>% 
  select(iso3c, date, value, ranking, quartile, half, 
         hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
  rename(value_ps = value, 
         ranking_ps = ranking, quartile_ps = quartile, half_ps = half,
         hp_cycle_pct_ps = hp_cycle_pct, hp_trend_ps = hp_trend, 
         diff_lastval_ps = diff_lastval, diff_avg3_ps = diff_avg3)

dcpsbk_gdp_tm <- dcpsbk_gdp %>% 
  select(iso3c, date, value, ranking, quartile, half,
         hp_cycle_pct, hp_trend, diff_lastval, diff_avg3) %>% 
  rename(value_psbk = value, 
         ranking_psbk = ranking, quartile_psbk = quartile, half_psbk = half,
         hp_cycle_pct_psbk = hp_cycle_pct, hp_trend_psbk = hp_trend,
         diff_lastval_psbk = diff_lastval, diff_avg3_psbk = diff_avg3)

credit_pspsbkfs <-  left_join(dcps_gdp_tm, dcpsbk_gdp_tm,
                              by = c("iso3c", "date"))
credit_pspsbkfs <- left_join(credit_pspsbkfs, dcfs_gdp_tm,
                             by = c("iso3c", "date"))



# credit related chunks plots --------------------------------------------------

## ---- combined_credit_plots_make

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
    q_cred_lines <- ggplot(data = pdata %>% filter(iso3c %in% coi)) + 
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
           stat = "identity") +  geom_label(size=3, hjust = -0.5)  +
  coord_flip() 


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
  geom_label(size=3, hjust = -0.5)  +
  coord_flip()

g_pb_lastavg_bar <- ggplot(data = bankloans_totconhip %>% filter(date == ymd("2006-10-01")),
                               aes(x = iso3c, y = diff_avg3_con,
                                   label = format(diff_avg3_con, digits = 1 ))) + 
  geom_bar(aes(fill = diff_avg3_con),
           stat = "identity") +  geom_label(size=3, hjust = -0.5)  +
  coord_flip() 

## ---- credit_plots_show

g_credit_pspsbkfs_ranking
g_credit_pspsbkfs_quartiles
g_credit_pspsbkfs_halves
g_credit_pspsbkfs_cycle_pct
g_credit_pspsbkfs_trend
g_credit_lastavg_bar
g_credit_lastval_bar

## ---- pb_plots_show

g_pb_totconhip_ranking
g_pb_totconhip_quartiles
g_pb_totconhip_halves
g_pb_totconhip_cycle_pct
g_pb_totconhip_trend
g_pb_lastval_bar
g_pb_lastavg_bar

# 
# ## ---- comparison_fin_vs_tot

# plot_ps_bk_fs <- function(iso3country) {
#   co_cp <- dcps_gdp %>% filter(iso3c == iso3country) %>% arrange(date)
#   co_fs <- dcfs_gdp %>% filter(iso3c == iso3country) %>% arrange(date)
#   co_cpbk <- dcpsbk_gdp %>% filter(iso3c == iso3country) %>% arrange(date)
#   
#   p_co <- ggplot(data = co_cp, aes(x = date, y = value, col = "to ps")) + 
#     geom_line() + 
#     geom_line(data = co_cpbk, 
#               aes(x = date, y = value, col = "to ps, by banks" )) + 
#     geom_line(data = co_fs, 
#               aes(x = date, y = value, col = "by fin sec"))  +
#     ggtitle(label = iso3country)
#   p_co   
# }
# 
# # p_chl <- plot_ps_bk_fs(iso3country = "CHL")
# # p_chl
# 
# 
# coi_pb <- unique(pb_tot$iso3c)
# 
# p_iso_list_cred = purrr::map(coi_18, plot_ps_bk_fs)
# 
# plot_pb_t_c_h <- function(iso3country) {
#   co_t <- pb_tot %>% filter(iso3c == iso3country) %>% arrange(date)
#   co_c <- pb_con %>% filter(iso3c == iso3country) %>% arrange(date)
#   co_h <- pb_hip %>% filter(iso3c == iso3country) %>% arrange(date)
#   
#   p_co <- ggplot(data = co_t, aes(x = date, y = total_to_gdp_seas,
#                                   col = "bk ln, total")) + geom_line() + 
#     geom_line(data = co_c, 
#               aes(x = date, y = consumo_gdp_seas, col = "bk ln, consum" )) + 
#     geom_line(data = co_h, 
#               aes(x = date, y = hipotecario_gdp_seas, col = "bk ln, mort"))  +
#     ggtitle(label = iso3country)
#   p_co   
# }
# 
# p_iso_list_pb = purrr::map(coi_pb, plot_pb_t_c_h)
# 
# p_iso_list_pb
