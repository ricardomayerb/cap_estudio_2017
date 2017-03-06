# summary of monetary and financial data
library(broom)
library(plotly)
library(ggiraph)
library(ggiraphExtra)
library(stringr)
library(xts)
library(gridExtra)
library(seasonal)
library(tidyverse)
library(lubridate)

load("./produced_data/monetary_fin_tidy")
load("./produced_data/cs_gdp_constantlc_q_20")
load("./produced_data/cs_gdp_currentlc_q_20")

cv_no_na <- cartera_vencida_20_tidy %>% 
  filter( !is.na(cartera_vencida_percent) )


cartera_vencida_qtr <- cs_gdp_currentlc_q_20 %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( cartera_vencida_20_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(cartera_vencida_percent) ) %>% 
  select(-Años)

tab_cv <- cv_no_na %>% group_by(iso2c, year) %>%
    summarise(nobs=n(), 
              avg_cv = mean(cartera_vencida_percent),
              num_countries = length(unique(cv_no_na$iso3c)))

p_cv <- ggplot(tab_cv, aes(x=year, y=avg_cv, col=iso2c)) + geom_line()
p_cv

p_cv_ly <- ggplotly(p_cv)
p_cv_ly

credito_interno_qtr <- cs_gdp_currentlc_q_20 %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( credito_interno_20_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  select(-Años) %>% 
  mutate(total_to_gdp_seas = total/gdp_sa_seas,
         al_spub_gdp_seas = al.sector.público/gdp_sa_seas,
         al_spriv_gdp_seas = al.sector.privado/gdp_sa_seas,
         al_gob_gdp_seas = gobierno/gdp_sa_seas,
         a_otros_gdp_seas = otros/gdp_sa_seas,
         total_to_gdp_stl = total/gdp_sa_stl,
         al_spub_gdp_stl = al.sector.público/gdp_sa_stl,
         al_spriv_gdp_stl = al.sector.privado/gdp_sa_stl,
         al_gob_gdp_stl = gobierno/gdp_sa_stl,
         a_otros_gdp_stl = otros/gdp_sa_stl)

p_tot_to_gdp_seas_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("BR", "CL",  "CR")) %>% 
  ggplot(aes(x = date, y = total_to_gdp_seas, color = iso2c)) + 
    geom_line()

p_tot_to_gdp_stl_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("BR", "CL",  "CR")) %>% 
  ggplot(aes(x = date, y = total_to_gdp_stl, color = iso2c)) + 
  geom_line()

p_tot_to_gdp_seas_stl_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("BR", "CL",  "CR")) %>% 
  ggplot(aes(x = date, y = value, color = iso2c)) + 
  geom_line(aes(y = total_to_gdp_stl)) + 
  geom_line(aes(y = total_to_gdp_seas))

p_tot_to_gdp_seas_stl_qtr

p_tot_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("BR", "CL",  "CR")) %>% 
  ggplot(aes(x=date, y=total, color=iso2c)) + 
  geom_line()





tab_ci <- credito_interno_20_tidy %>% 
  group_by(iso2c, year) %>%
  summarise(nobs = n(), 
            num_countries = length(unique(credito_interno_20_tidy$iso2c)),
            avg_spu = mean(al.sector.público, rm.na = TRUE),
            avg_spr = mean(al.sector.privado, rm.na = TRUE),
            avg_tot = mean(total, rm.na = TRUE),
            avg_gob = mean(gobierno, rm.na = TRUE),
            n_gob = sum(!is.na(gobierno)),
            n_tot = sum(!is.na(total)),
            n_spub = sum(!is.na(al.sector.público)),
            n_spr = sum(!is.na(al.sector.privado))
  )


tab_ci_qrt_gdp <- credito_interno_qtr %>% 
  group_by(iso2c, year) %>%
  summarise(nobs = n(), 
            num_countries = length(unique(credito_interno_qtr$iso2c)),
            avg_spu_gdp = mean(al_spub_gdp_stl, rm.na = TRUE),
            avg_spr_gdp = mean(al_spriv_gdp_stl, rm.na = TRUE),
            avg_tot_gdp = mean(total_to_gdp_stl, rm.na = TRUE),
            avg_gob_gdp = mean(al_gob_gdp_stl, rm.na = TRUE),
            n_gob_gdp = sum(!is.na(al_gob_gdp_stl)),
            n_tot_gdp = sum(!is.na(total_to_gdp_stl)),
            n_spub_gdp = sum(!is.na(al_spub_gdp_stl)),
            n_spr_gdp = sum(!is.na(al_spriv_gdp_stl))
  )


prestamos_bancarios_qtr <- cs_gdp_currentlc_q_20 %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join(prestamos_bancarios_20_tidy,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  select(-Años) %>% 
  mutate(total_to_gdp_stl = total/gdp_sa_stl,
         consumo_gdp_stl = consumo/gdp_sa_stl,
         hipotecario_gdp_stl = hipotecario/gdp_sa_stl,
         industrial_gdp_stl = industrial/gdp_sa_stl,
         comercial_gdp_stl = comercial/gdp_sa_stl)

tab_pb_qtr <- prestamos_bancarios_qtr %>% group_by(iso2c, year) %>% 
  group_by(iso2c, year) %>%
  filter(iso2c != "CO") %>% 
  summarise(nobs = n(), 
            num_countries = length(unique(prestamos_bancarios_qtr$iso2c)),
            avg_tot_to_gdp = mean(total_to_gdp_stl),
            avg_hip_to_gdp = mean(hipotecario_gdp_stl, rm.na=TRUE),
            avg_con_to_gdp = mean(consumo_gdp_stl, rm.na=TRUE),
            n_hip_to_gdp = sum(!is.na(hipotecario_gdp_stl)),
            n_con_to_gdp = sum(!is.na(consumo_gdp_stl))
            )




p_pb_t <- ggplot(filter(tab_pb, year >= 2000),
                 aes(x=year, y=avg_tot_to_gdp, col=iso2c)) + 
          geom_line()
p_pb_t
p_pb_t_ly <- ggplotly(p_pb_t)
p_pb_t_ly



tab_tpm <- tpm_20_tidy %>% 
  group_by(iso3c, year) %>% 
  summarise(
    n_tpm = sum(!is.na(tasa_politica_monetaria)),
    avg_tpm = mean(tasa_politica_monetaria, rm.na = TRUE)
  ) %>% 
  filter(n_tpm > 0)


tab_meta <- meta_inf_tidy %>% 
  group_by(iso2c, year) %>% 
  summarise(
    n_meta_center = sum(!is.na(meta_inf)),
    n_meta_low = sum(!is.na(meta_inf_low)),
    n_meta_hi = sum(!is.na(meta_inf_hi)),
    avg_meta_center = mean(meta_inf, rm.na = TRUE),
    avg_meta_low = mean(meta_inf_low, rm.na = TRUE),
    avg_meta_high = mean(meta_inf_hi, rm.na = TRUE)
  ) %>% 
  filter(n_meta_center + n_meta_low + n_meta_hi != 0)

