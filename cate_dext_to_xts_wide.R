library(xts)
library(dplyr)
library(tidyr)

cate_dext_to_xts_wide <- function(cate_dext_df) {
  df_sp_am <-  cate_dext_df %>% 
    filter(dext_group == "above_median") %>% 
    spread(key = iso3c, value = deuda_ext_porc_pib)
  
  df_am_xts <- df_sp_am %>% 
    select(-year) %>% 
    xts(order.by = df_sp_am$year)
  
  df_sp_bm <-  cate_dext_df %>% 
    filter(dext_group == "below_median") %>% 
    spread(key = iso3c, value = deuda_ext_porc_pib)
  
  df_bm_xts <- df_sp_bm %>% 
    select(-year) %>% 
    xts(order.by = df_sp_bm$year)
  
  return(list(below_median = df_bm_xts, above_median = df_am_xts))
  
}