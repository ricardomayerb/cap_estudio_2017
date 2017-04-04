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


load(paste0(pre_path,
             "produced_data/data_with_basic_wrangling/WDI_33_selected_all"))


load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_reservish_dfs"))

load(paste0(pre_path, 
            "produced_data/data_with_basic_wrangling/wb_interests_dfs"))




# credit related chunks dfs ----------------------------------------------------

## ---- bk_loans_consum_dfs
prestamos_bancarios_qtr <- cs_gdp_currentlc_q_gasto %>% 
  select(-c(iso3c, nombre_pais, Rubro, year)) %>% 
  left_join(prestamos_bancarios_33_tidy,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  mutate(total_to_gdp_seas = total/gdp_sa_seas,
         consumo_gdp_seas = consumo/gdp_sa_seas,
         hipotecario_gdp_seas = hipotecario/gdp_sa_seas,
         industrial_gdp_seas = industrial/gdp_sa_seas,
         comercial_gdp_seas = comercial/gdp_sa_seas)



tab_pb_qtr <- prestamos_bancarios_qtr %>% group_by(iso2c, year) %>% 
  group_by(iso2c, year) %>%
  filter(iso2c != "CO") %>% 
  summarise(nobs = n(), 
            num_countries = length(unique(prestamos_bancarios_qtr$iso2c)),
            avg_tot_to_gdp = mean(total_to_gdp_seas),
            avg_hip_to_gdp = mean(hipotecario_gdp_seas, rm.na=TRUE),
            avg_con_to_gdp = mean(consumo_gdp_seas, rm.na=TRUE),
            n_hip_to_gdp = sum(!is.na(hipotecario_gdp_seas)),
            n_con_to_gdp = sum(!is.na(consumo_gdp_seas))
  )

# pb_time_break <- as.Date("2004-10-01", format = "%Y-%m-%d") # 4th quarter of 2004

pb <- prestamos_bancarios_qtr %>% 
  filter(iso3c %in% coi_18) %>% 
  filter( iso3c != "COL") # remove colombia until fix in ci or gdp data (ratio is off by 3 orders of magnitude)

pb_tot <- pb %>% select(iso3c, date, total_to_gdp_seas) %>% 
  add_baselines(value_colname = "total_to_gdp_seas")


pb_hip <- pb %>% select(iso3c, date, hipotecario_gdp_seas) %>% 
  add_baselines(value_colname = "hipotecario_gdp_seas")

pb_con <- pb %>% select(iso3c, date, consumo_gdp_seas) %>% 
  add_baselines(value_colname = "consumo_gdp_seas")



pb_tot_pct4 <- cate_gen(df = pb_tot, value_col_name = "total_to_gdp_seas",
                        dating_col_name = "date",
                        is_med = FALSE,  is_pct4 = TRUE)

pb_hip_pct4 <- cate_gen(df = pb_hip, value_col_name = "hipotecario_gdp_seas",
                        dating_col_name = "date",
                        is_med = FALSE,  is_pct4 = TRUE)

pb_con_pct4 <- cate_gen(df = pb_con, value_col_name = "consumo_gdp_seas",
                        dating_col_name = "date",
                        is_med = FALSE,  is_pct4 = TRUE)

pb_tot_pct4_countries = make_country_lists_by_quant(pb_tot_pct4)
pb_hip_pct4_countries = make_country_lists_by_quant(pb_hip_pct4)
pb_con_pct4_countries = make_country_lists_by_quant(pb_con_pct4)




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

dcfs_gdp_pct4_countries = make_country_lists_by_quant(dcfs_gdp_pct4)


## ---- credit_to_private_sector_by_banks_dfs
dcpsbk_gdp <- dom_credit_to_priv_sec_by_banks_to_gdp %>% 
  filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
  arrange(iso2c, date) %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
         date = as.Date(as.character(date), format = "%Y")) %>% 
  mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                        ordered = TRUE))

dcpsbk_gdp <- add_baselines(dcpsbk_gdp)
dcpsbk_gdp <- add_ts_filters(dcpsbk_gdp)

dcpsbk_gdp_pct4 <- cate_gen(dcpsbk_gdp,
                          is_med = FALSE, is_pct4 = TRUE,
                          value_col_name = "final_avg",
                          dating_col_name = "date") %>% 
  filter(!is.na(gen_group)) %>% 
  arrange(desc(final_avg))

dcpsbk_gdp_pct4_countries = make_country_lists_by_quant(dcpsbk_gdp_pct4)

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

dcps_gdp_pct4_countries = make_country_lists_by_quant(dcps_gdp_pct4)



print(as.vector(dcps_gdp_pct4_countries[["q_4"]]$iso3c))
print(as.vector(dcpsbk_gdp_pct4_countries[["q_4"]]$iso3c))
print(as.vector(dcfs_gdp_pct4_countries[["q_4"]]$iso3c))

print(as.vector(dcps_gdp_pct4_countries[["q_3"]]$iso3c))
print(as.vector(dcpsbk_gdp_pct4_countries[["q_3"]]$iso3c))
print(as.vector(dcfs_gdp_pct4_countries[["q_3"]]$iso3c))

print(as.vector(dcps_gdp_pct4_countries[["q_2"]]$iso3c)) 
print(as.vector(dcpsbk_gdp_pct4_countries[["q_2"]]$iso3c))
print(as.vector(dcfs_gdp_pct4_countries[["q_2"]]$iso3c))

print(as.vector(dcps_gdp_pct4_countries[["q_1"]]$iso3c))
print(as.vector(dcpsbk_gdp_pct4_countries[["q_1"]]$iso3c))
print(as.vector(dcfs_gdp_pct4_countries[["q_1"]]$iso3c))



# credit related chunks plots --------------------------------------------------

## ---- pb_plots

### total pb
by_gen_groups <- pb_tot_pct4 %>% 
  arrange(desc(final_avg)) %>% 
  group_by(gen_group) %>% 
  nest() 

plots_pb_tot_pct4 <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                                y=total_to_gdp_seas, col=iso3c)) +
                                geom_line()  + theme_tufte() + 
                                geom_hline(yintercept = 0) +           
                                ggtitle("Bank loans, total",
                                        subtitle = "% of SA GDP,"))) 

multiplot(plotlist = plots_pb_tot_pct4$gruplot, cols = 2)

### con pb
by_gen_groups <- pb_con_pct4 %>% 
  arrange(desc(final_avg)) %>% 
  group_by(gen_group) %>% 
  nest() 

plots_pb_con_pct4 <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                                y=consumo_gdp_seas, col=iso3c)) +
                                geom_line()  + theme_tufte() + 
                                geom_hline(yintercept = 0) +           
                                ggtitle("Bank loans, consumption",
                                        subtitle = "% of SA GDP,"))) 

multiplot(plotlist = plots_pb_con_pct4$gruplot, cols = 2)


### hip pb
by_gen_groups <- pb_hip_pct4 %>% 
  arrange(desc(final_avg)) %>% 
  filter(!is.na(gen_group)) %>% 
  group_by(gen_group) %>% 
  nest() 

plots_pb_hip_pct4 <- by_gen_groups %>% 
  mutate(gruplot = purrr::map(data , ~ ggplot(data = ., aes(x=date,
                              y=hipotecario_gdp_seas, col=iso3c)) +
                                geom_line()  + theme_tufte() + 
                                geom_hline(yintercept = 0) +           
                                ggtitle("Bank loans, mortages",
                                        subtitle = "% of SA GDP,"))) 

multiplot(plotlist = plots_pb_hip_pct4$gruplot, cols = 2)




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


plot_ps_bk_fs <- function(iso3country) {
  co_cp <- dcps_gdp %>% filter(iso3c == iso3country) %>% arrange(date)
  co_fs <- dcfs_gdp %>% filter(iso3c == iso3country) %>% arrange(date)
  co_cpbk <- dcpsbk_gdp %>% filter(iso3c == iso3country) %>% arrange(date)
  
  p_co <- ggplot(data = co_cp, aes(x = date, y = value, col = "to ps")) + 
    geom_line() + 
    geom_line(data = co_cpbk, 
              aes(x = date, y = value, col = "to ps, by banks" )) + 
    geom_line(data = co_fs, 
              aes(x = date, y = value, col = "by fin sec"))  +
    ggtitle(label = iso3country)
  p_co   
}

# p_chl <- plot_ps_bk_fs(iso3country = "CHL")
# p_chl


coi_pb <- unique(pb_tot$iso3c)

p_iso_list_cred = purrr::map(coi_18, plot_ps_bk_fs)

plot_pb_t_c_h <- function(iso3country) {
  co_t <- pb_tot %>% filter(iso3c == iso3country) %>% arrange(date)
  co_c <- pb_con %>% filter(iso3c == iso3country) %>% arrange(date)
  co_h <- pb_hip %>% filter(iso3c == iso3country) %>% arrange(date)
  
  p_co <- ggplot(data = co_t, aes(x = date, y = total_to_gdp_seas,
                                  col = "bk ln, total")) + geom_line() + 
    geom_line(data = co_c, 
              aes(x = date, y = consumo_gdp_seas, col = "bk ln, consum" )) + 
    geom_line(data = co_h, 
              aes(x = date, y = hipotecario_gdp_seas, col = "bk ln, mort"))  +
    ggtitle(label = iso3country)
  p_co   
}

p_iso_list_pb = purrr::map(coi_pb, plot_pb_t_c_h)

