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

load("./produced_data/monetary_fin_tidy")

load("./produced_data/cs_real_mn_trimestral_20")

cv_no_na <- cartera_vencida_20_tidy %>% 
  filter( !is.na(cartera_vencida_percent) )

gdp_currentlc_q <- cs_real_mn_trimestral_20 %>% 
    filter(Rubro=="Producto interno bruto (PIB)") %>% 
    select( -c(indicador, Rubro_1, notas, fuente)) %>% 
    dplyr::rename(gdp = valor) 

cs_real_mn_trimestral_cl <-  cs_real_mn_trimestral_20 %>% 
  filter(iso2c == "CL") %>% 
  arrange(Rubro, date)

gdp_date <- gdp_currentlc_q %>% 
  filter(iso2c == "CL") %>% 
  select(gdp, date)

gdp_cl <- gdp_currentlc_q %>% 
  filter(iso2c == "CL") 
     


cartera_vencida_qtr <- gdp_currentlc_q %>% 
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

# tab_cv$tooltip <- tab_cv$iso2c
# p_cv_ig <- ggplot(tab_cv, aes(x=year, y=avg_cv, col=iso2c)) + 
#               geom_line_interactive()
# ggiraph(code = print(p_cv_ig), width = 0.7)


credito_interno_qtr <- gdp_currentlc_q %>% 
  select(-c(iso3c, nombre_pais, Rubro)) %>% 
  left_join( credito_interno_20_tidy ,  by = c("iso2c", "date")) %>% 
  filter( !is.na(total) ) %>% 
  select(-Años) %>% 
  mutate(total_to_gdp = total/gdp,
         al_spub_gdp = al.sector.público/gdp,
         al_spriv_gdp = al.sector.privado/gdp,
         al_gob_gdp = gobierno/gdp,
         a_otros_gdp = otros/gdp)

p_tot_to_gdp_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("AR", "BR", "CL",  "MX", "UY")) %>% 
  ggplot(aes(x=date, y=total_to_gdp, color=iso2c)) + 
    geom_line()

p_tot_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("AR", "BR", "CL",  "MX", "UY")) %>% 
  ggplot(aes(x=date, y=total, color=iso2c)) + 
  geom_line()

p_gdp_qtr <- credito_interno_qtr %>% 
  filter(iso2c %in% c("AR", "BR", "CL",  "MX", "UY")) %>% 
  ggplot(aes(x=date, y=gdp, color=iso2c)) + 
  geom_line()

grid.arrange(p_tot_to_gdp_qtr, p_tot_qtr, p_gdp_qtr, ncol=2, nrow=2)


ci_no_na <- credito_interno_20_tidy %>% 
  filter( !is.na(al.sector.privado))

tab_ci <- ci_no_na %>% group_by(iso2c, year) %>%
  summarise(nobs=n(), 
            num_countries = length(unique(ci_no_na$iso2c)),
            avg_spu = mean(al.sector.público),
            avg_spr = mean(al.sector.privado),
            avg_tot = mean(total),
            avg_gob = mean(gobierno, rm.na=TRUE),
            n_gob = sum(!is.na(gobierno)))


ci_al_gob_no_na <- ci_no_na %>% filter( !is.na(gobierno))





pb_no_na <- prestamos_bancarios_20_tidy %>% 
  filter(!is.na(total))
unique(pb_no_na$iso3c)

chl_pb_t <- pb_no_na %>% 
              filter(iso2c == "CL") 

p_chl_pb_t <- ggplot(chl_pb_t, aes(x=date, y=total)) + geom_line()
p_chl_pb_t

tab_pb <- pb_no_na %>% group_by(iso2c, year) %>% 
  group_by(iso2c, year) %>%
  summarise(nobs=n(), 
            num_countries = length(unique(pb_no_na$iso2c)),
            avg_tot = mean(total),
            avg_hip = mean(hipotecario, rm.na=TRUE),
            avg_con = mean(consumo, rm.na=TRUE),
            n_hip = sum(!is.na(hipotecario)),
            n_con = sum(!is.na(consumo))
            )


p_pb_t <- ggplot(filter(tab_pb, year >= 2000), aes(x=year, y=log(avg_tot), col=iso2c)) + geom_line()
p_pb_t
p_pb_t_ly <- ggplotly(p_t_pb)
p_pb_t_ly

p_pb_h <- ggplot(filter(tab_pb, n_hip >= 4 & year >= 2000), aes(x=year, y=log(avg_hip), col=iso2c)) + geom_line()
p_pb_h


pb_hip_con_no_na <- pb_no_na %>% 
  filter(!is.na(hipotecario) & !is.na(consumo))
unique(pb_hip_con_no_na$iso3c)

pb_hip_con_come_no_na <- pb_hip_con_no_na %>% 
  filter(!is.na(comercial) )
unique(pb_hip_con_come_no_na$iso3c)

pb_hip_con_come_ind_no_na <- pb_hip_con_come_no_na %>% 
  filter(!is.na(industrial) )
unique(pb_hip_con_come_ind_no_na$iso3c)

pb_hip_con_come_ind_mic_no_na <- pb_hip_con_come_ind_no_na %>% 
  filter(!is.na(microcrédito))
unique(pb_hip_con_come_ind_mic_no_na$iso3c)




