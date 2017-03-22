library(dplyr)
library(tidyr)
library(xts)
library(replyr)
library(wrapr)


cate_dext <- function(df, time_breaks = NULL, level_breaks = NULL,
                      is_med = TRUE, is_pct3 = FALSE, is_pct4 = FALSE) {
  
  if (is.null(time_breaks)) {
    if (is_med == TRUE) {
      new_df <- df %>% arrange(iso3c, year) %>% 
        mutate(median_of_avg = median(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
        group_by(iso3c) %>% 
        mutate(avg_debt = mean(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
        mutate(dext_group = if_else(avg_debt > median_of_avg, "above_median",
                                    "below_median"))
      return(new_df)
    } else {
      if (is_pct3) {
        new_df <- df  %>% arrange(iso3c, year) %>% 
          mutate(pct33_of_avg = quantile(deuda_ext_porc_pib, probs = 0.33,
                                         na.rm = TRUE),
                 pct66_of_avg = quantile(deuda_ext_porc_pib, probs = 0.66,
                                         na.rm = TRUE)
          ) %>% 
          group_by(iso3c) %>% 
          mutate(avg_debt = mean(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
          mutate(dext_group = if_else(avg_debt > pct66_of_avg, "above_pct66",
                                      if_else(avg_debt > pct33_of_avg,
                                              "pct33_to_66",
                                              "below_pct33")
          )
          
          )
        return(new_df)
      } else {
        new_df <- df %>% 
          mutate(pct25_of_avg = quantile(deuda_ext_porc_pib, probs = 0.25,
                                         na.rm = TRUE),
                 pct50_of_avg = quantile(deuda_ext_porc_pib, probs = 0.50,
                                         na.rm = TRUE),
                 pct75_of_avg = quantile(deuda_ext_porc_pib, probs = 0.75,
                                         na.rm = TRUE)
          ) %>% 
          group_by(iso3c) %>% 
          mutate(avg_debt = mean(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
          mutate(dext_group = if_else(avg_debt > pct75_of_avg, "above_pct75",
                                      if_else(avg_debt > pct50_of_avg, "pct50_to_75",
                                              if_else(avg_debt > pct25_of_avg,
                                                      "pct25_to_50",
                                                      "below_pct25"))
          )
          
          )
        return(new_df)
      }
    }
    
  } else {
    # some time break used: e.g. 2001. Creates 1990-2001 and 2002, 2015
    if (is_med) {
      new_df <- df %>%  arrange(iso3c, year) %>% 
        mutate(period = if_else(year(year) <= time_breaks, "period_1", "period_2")) %>% 
        arrange(period) %>% 
        group_by(period) %>% 
        mutate(median_of_avg = median(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
        group_by(period, iso3c) %>% 
        mutate(avg_debt = mean(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(iso3c) %>% 
        mutate(dext_group = if_else(avg_debt > median_of_avg, "above_median",
                                    "below_median"))
      
      new_df_period_1 <- new_df %>% 
        filter(period == "period_1")
      
      new_df_period_2 <- new_df %>% 
        filter(period == "period_2")
      
      return( list(df_period_1 = new_df_period_1,
                   df_period_2 = new_df_period_2, df_full =  new_df))
    } else {
      if (is_pct3) {
        new_df <- df %>%  arrange(iso3c, year) %>% 
          mutate(period = if_else(year(year) <= time_breaks, "period_1", "period_2")) %>% 
          arrange(period) %>% 
          group_by(period) %>% 
          mutate(pct33_of_avg = quantile(deuda_ext_porc_pib, probs = 0.33,
                                         na.rm = TRUE),
                 pct66_of_avg = quantile(deuda_ext_porc_pib, probs = 0.66,
                                         na.rm = TRUE)
          ) %>% 
          group_by(period, iso3c) %>% 
          mutate(avg_debt = mean(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
          ungroup() %>% 
          group_by(iso3c) %>% 
          mutate(dext_group = if_else(avg_debt > pct66_of_avg, "above_pct66",
                                      if_else(avg_debt > pct33_of_avg,
                                              "pct33_to_66",
                                              "below_pct33")
          )
          
          )
        new_df_period_1 <- new_df %>% 
          filter(period == "period_1")
        
        new_df_period_2 <- new_df %>% 
          filter(period == "period_2")
        
        return( list(df_period_1 = new_df_period_1,
                     df_period_2 = new_df_period_2, df_full =  new_df))
      } else {
        new_df <- df %>%  arrange(iso3c, year) %>% 
          mutate(period = if_else(year(year) <= time_breaks, "period_1", "period_2")) %>% 
          arrange(period) %>% 
          group_by(period) %>% 
          mutate(pct25_of_avg = quantile(deuda_ext_porc_pib, probs = 0.25,
                                         na.rm = TRUE),
                 pct50_of_avg = quantile(deuda_ext_porc_pib, probs = 0.50,
                                         na.rm = TRUE),
                 pct75_of_avg = quantile(deuda_ext_porc_pib, probs = 0.75,
                                         na.rm = TRUE)
          ) %>% 
          group_by(period, iso3c) %>% 
          mutate(avg_debt = mean(deuda_ext_porc_pib, na.rm = TRUE)) %>% 
          ungroup() %>% 
          group_by(iso3c) %>% 
          mutate(dext_group = if_else(avg_debt > pct75_of_avg, "above_pct75",
                                      if_else(avg_debt > pct50_of_avg, "pct50_to_75",
                                              if_else(avg_debt > pct25_of_avg,
                                                      "pct25_to_50",
                                                      "below_pct25"))
          )
          
          )
        new_df_period_1 <- new_df %>% 
          filter(period == "period_1")
        
        new_df_period_2 <- new_df %>% 
          filter(period == "period_2")
        
        return( list(df_period_1 = new_df_period_1,
                     df_period_2 = new_df_period_2, df_full =  new_df))
        
      }
    }
  }
  
}


cate_dext_to_xts_wide <- function(cate_dext_df, is_med = TRUE,
                                  is_pct3 = FALSE, is_pct4 = FALSE,
                                  is_full = FALSE) {
  
  if(is_full){
    df_sp <-  cate_dext_df %>% 
      select(iso3c, year, deuda_ext_porc_pib) %>% 
      spread(key = iso3c, value = deuda_ext_porc_pib)
    
    df_xts <- df_sp %>% 
      select(-year) %>% 
      xts(order.by = df_sp$year)
    
    return(df_xts)
  }
  
  
  if(is_med){
    df_sp_am <-  cate_dext_df %>% 
      filter(dext_group == "above_median") %>% 
      select(iso3c, year, deuda_ext_porc_pib) %>% 
      spread(key = iso3c, value = deuda_ext_porc_pib)
    
    df_am_xts <- df_sp_am %>% 
      select(-year) %>% 
      xts(order.by = df_sp_am$year)
    
    df_sp_bm <-  cate_dext_df %>% 
      filter(dext_group == "below_median") %>% 
      select(iso3c, year, deuda_ext_porc_pib) %>% 
      spread(key = iso3c, value = deuda_ext_porc_pib)
    
    df_bm_xts <- df_sp_bm %>% 
      select(-year) %>% 
      xts(order.by = df_sp_bm$year)
    
    return(list(below_median = df_bm_xts, above_median = df_am_xts))
  } else {
    if (is_pct3) {
      df_sp_p33 <-  cate_dext_df %>% 
        filter(dext_group == "below_pct33") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p33_xts <- df_sp_p33 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p33$year)
      
      df_sp_p33_to_66 <-  cate_dext_df %>% 
        filter(dext_group == "pct33_to_66") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p33_to_66_xts <- df_sp_p33_to_66 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p33_to_66$year)
      
      df_sp_p66 <-  cate_dext_df %>% 
        filter(dext_group == "above_pct66") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p66_xts <- df_sp_p66 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p66$year)
      
      
      return(list(below_p33 = df_p33_xts, p33_to_66 = df_p33_to_66_xts,
                  above_p66 = df_p66_xts))
      
    } else {
      #pct4
      df_sp_p25 <-  cate_dext_df %>% 
        filter(dext_group == "below_pct25") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p25_xts <- df_sp_p25 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p25$year)
      
      df_sp_p25_to_50 <-  cate_dext_df %>% 
        filter(dext_group == "pct25_to_50") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p25_to_50_xts <- df_sp_p25_to_50 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p25_to_50$year)

      df_sp_p50_to_75 <-  cate_dext_df %>% 
        filter(dext_group == "pct50_to_75") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p50_to_75_xts <- df_sp_p50_to_75 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p50_to_75$year)
      
      df_sp_p75 <-  cate_dext_df %>% 
        filter(dext_group == "above_pct75") %>% 
        select(iso3c, year, deuda_ext_porc_pib) %>% 
        spread(key = iso3c, value = deuda_ext_porc_pib)
      
      df_p75_xts <- df_sp_p75 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p75$year)
    
      return(list(below_p25 = df_p25_xts, p25_to_50 = df_p25_to_50_xts,
                  p50_to_75 = df_p50_to_75_xts, above_p75 = df_p75_xts))
      
    }
  }
  
  
}

cate_gen <- function(df, value_col_name, time_breaks = NULL, level_breaks = NULL,
                      is_med = TRUE, is_pct3 = FALSE, is_pct4 = FALSE,
                     dating_col_name = "year") {
  wrapr::let(alias = list(value_col = value_col_name, date_col = dating_col_name),
             expr = {
  if (is.null(time_breaks)) {
    if (is_med == TRUE) {
      new_df <- df %>% arrange(iso3c, year) %>% 
        mutate(median_of_avg = median(value_col, na.rm = TRUE)) %>% 
        group_by(iso3c) %>% 
        mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
        mutate(gen_group = if_else(avg_value > median_of_avg, "above_median",
                                    "below_median"))
      return(new_df)
    } else {
      if (is_pct3) {
        new_df <- df  %>% arrange(iso3c, year) %>% 
          mutate(pct33_of_avg = quantile(value_col, probs = 0.33,
                                         na.rm = TRUE),
                 pct66_of_avg = quantile(value_col, probs = 0.66,
                                         na.rm = TRUE)
          ) %>% 
          group_by(iso3c) %>% 
          mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
          mutate(gen_group = if_else(avg_value > pct66_of_avg, "above_pct66",
                                      if_else(avg_value > pct33_of_avg,
                                              "pct33_to_66",
                                              "below_pct33")
          )
          
          )
        return(new_df)
      } else {
        new_df <- df %>% 
          mutate(pct25_of_avg = quantile(value_col, probs = 0.25,
                                         na.rm = TRUE),
                 pct50_of_avg = quantile(value_col, probs = 0.50,
                                         na.rm = TRUE),
                 pct75_of_avg = quantile(value_col, probs = 0.75,
                                         na.rm = TRUE)
          ) %>% 
          group_by(iso3c) %>% 
          mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
          mutate(gen_group = if_else(avg_value > pct75_of_avg, "above_pct75",
                                      if_else(avg_value > pct50_of_avg, "pct50_to_75",
                                              if_else(avg_value > pct25_of_avg,
                                                      "pct25_to_50",
                                                      "below_pct25"))
          )
          
          )
        return(new_df)
      }
    }
    
  } else {
    # some time break used: e.g. 2001. Creates 1990-2001 and 2002, 2015
    
    if (is_med) {
      new_df <- df %>%  arrange(iso3c, date_col) %>% 
        mutate(period = if_else(date_col <= time_breaks, "period_1", "period_2")) %>% 
        arrange(period) %>% 
        group_by(period) %>% 
        mutate(median_of_avg = median(value_col, na.rm = TRUE)) %>% 
        group_by(period, iso3c) %>% 
        mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(iso3c) %>% 
        mutate(gen_group = if_else(avg_value > median_of_avg, "above_median",
                                    "below_median"))
      
      new_df_period_1 <- new_df %>% 
        filter(period == "period_1")
      
      new_df_period_2 <- new_df %>% 
        filter(period == "period_2")
      
      return( list(df_period_1 = new_df_period_1,
                   df_period_2 = new_df_period_2, df_full =  new_df))
    } else {
      if (is_pct3) {
        new_df <- df %>%  arrange(iso3c, date_col) %>% 
          mutate(period = if_else(date_col <= time_breaks, "period_1", "period_2")) %>% 
          arrange(period) %>% 
          group_by(period) %>% 
          mutate(pct33_of_avg = quantile(value_col, probs = 0.33,
                                         na.rm = TRUE),
                 pct66_of_avg = quantile(value_col, probs = 0.66,
                                         na.rm = TRUE)
          ) %>% 
          group_by(period, iso3c) %>% 
          mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
          ungroup() %>% 
          group_by(iso3c) %>% 
          mutate(gen_group = if_else(avg_value > pct66_of_avg, "above_pct66",
                                      if_else(avg_value > pct33_of_avg,
                                              "pct33_to_66",
                                              "below_pct33")
          )
          
          )
        new_df_period_1 <- new_df %>% 
          filter(period == "period_1")
        
        new_df_period_2 <- new_df %>% 
          filter(period == "period_2")
        
        return( list(df_period_1 = new_df_period_1,
                     df_period_2 = new_df_period_2, df_full =  new_df))
      } else {
        new_df <- df %>%  arrange(iso3c, date_col) %>% 
          mutate(period = if_else(date_col <= time_breaks, "period_1", "period_2")) %>% 
          arrange(period) %>% 
          group_by(period) %>% 
          mutate(pct25_of_avg = quantile(value_col, probs = 0.25,
                                         na.rm = TRUE),
                 pct50_of_avg = quantile(value_col, probs = 0.50,
                                         na.rm = TRUE),
                 pct75_of_avg = quantile(value_col, probs = 0.75,
                                         na.rm = TRUE)
          ) %>% 
          group_by(period, iso3c) %>% 
          mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
          ungroup() %>% 
          group_by(iso3c) %>% 
          mutate(gen_group = if_else(avg_value > pct75_of_avg, "above_pct75",
                                      if_else(avg_value > pct50_of_avg, "pct50_to_75",
                                              if_else(avg_value > pct25_of_avg,
                                                      "pct25_to_50",
                                                      "below_pct25"))
          )
          
          )
        new_df_period_1 <- new_df %>% 
          filter(period == "period_1")
        
        new_df_period_2 <- new_df %>% 
          filter(period == "period_2")
        
        return( list(df_period_1 = new_df_period_1,
                     df_period_2 = new_df_period_2, df_full =  new_df))
        
      }
    }
  }
  })
}

cate_gen_to_xts_wide <- function(cate_gen_df, value_col_name, is_med = TRUE,
                                  is_pct3 = FALSE, is_pct4 = FALSE,
                                  is_full = FALSE, 
                                 dating_col_name = "year") {
  wrapr::let(alias = list(value_col = value_col_name, year = dating_col_name), expr = {
  if(is_full){
    df_sp <-  cate_gen_df %>% 
      select(iso3c, year, value_col) %>% 
      spread(key = iso3c, value = value_col)
    
    df_xts <- df_sp %>% 
      select(-year) %>% 
      xts(order.by = df_sp$year)
    
    return(df_xts)
  }
  
  
  if(is_med){
    df_sp_am <-  cate_gen_df %>% 
      filter(gen_group == "above_median") %>% 
      select(iso3c, year, value_col) %>% 
      spread(key = iso3c, value = value_col)
    
    df_am_xts <- df_sp_am %>% 
      select(-year) %>% 
      xts(order.by = df_sp_am$year)
    
    df_sp_bm <-  cate_gen_df %>% 
      filter(gen_group == "below_median") %>% 
      select(iso3c, year, value_col) %>% 
      spread(key = iso3c, value = value_col)
    
    df_bm_xts <- df_sp_bm %>% 
      select(-year) %>% 
      xts(order.by = df_sp_bm$year)
    
    return(list(below_median = df_bm_xts, above_median = df_am_xts))
  } else {
    if (is_pct3) {
      df_sp_p33 <-  cate_gen_df %>% 
        filter(gen_group == "below_pct33") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p33_xts <- df_sp_p33 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p33$year)
      
      df_sp_p33_to_66 <-  cate_gen_df %>% 
        filter(gen_group == "pct33_to_66") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p33_to_66_xts <- df_sp_p33_to_66 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p33_to_66$year)
      
      df_sp_p66 <-  cate_gen_df %>% 
        filter(gen_group == "above_pct66") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p66_xts <- df_sp_p66 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p66$year)
      
      
      return(list(below_p33 = df_p33_xts, p33_to_66 = df_p33_to_66_xts,
                  above_p66 = df_p66_xts))
      
    } else {
      #pct4
      df_sp_p25 <-  cate_gen_df %>% 
        filter(gen_group == "below_pct25") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p25_xts <- df_sp_p25 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p25$year)
      
      df_sp_p25_to_50 <-  cate_gen_df %>% 
        filter(gen_group == "pct25_to_50") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p25_to_50_xts <- df_sp_p25_to_50 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p25_to_50$year)
      
      df_sp_p50_to_75 <-  cate_gen_df %>% 
        filter(gen_group == "pct50_to_75") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p50_to_75_xts <- df_sp_p50_to_75 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p50_to_75$year)
      
      df_sp_p75 <-  cate_gen_df %>% 
        filter(gen_group == "above_pct75") %>% 
        select(iso3c, year, value_col) %>% 
        spread(key = iso3c, value = value_col)
      
      df_p75_xts <- df_sp_p75 %>% 
        select(-year) %>% 
        xts(order.by = df_sp_p75$year)
      
      return(list(below_p25 = df_p25_xts, p25_to_50 = df_p25_to_50_xts,
                  p50_to_75 = df_p50_to_75_xts, above_p75 = df_p75_xts))
      
    }
  }
  
  })
}

# load("./functions/prueba_cixts.rda")

# foo <- ci_18 %>% select(iso3c, date, total_to_gdp_seas)
# 
# moo <-  cate_gen(
#   df = foo,
#   value_col_name = "total_to_gdp_seas", is_med = FALSE,
#   is_pct4 = TRUE)
# 
# my_time_break = as.Date("2004-10-01", format = "%Y-%m-%d") # 4th quarter of 2004
# 
# goo <-  cate_gen(
#   df = foo, time_breaks = my_time_break,
#   value_col_name = "total_to_gdp_seas", is_med = FALSE,
#   is_pct4 = TRUE,
#   dating_col_name = "date")

#  time_breaks = 2001,
# load("functions/gobcen.rda")
# 
# foo <- cate_gen_to_xts_wide(cate_gen_df = gobcen_deuda_18, value_col_name = "total_p",
#                      is_med = FALSE, is_full = TRUE)

break_down_table <- function(bigtable, iso2c_list=NULL,
                             cuts_vector=NULL, ...) {
  
  if (!is.null(iso2c_list)) {
    dfs_list = vector("list", length = length(iso2c_list))
    for(i in 1:length(iso2c_list)){
      idx = bigtable$iso2c %in% iso2c_list[[i]]
      dfs_list[[i]] <- bigtable[idx, ]
    }
    
    return(dfs_list)
  }
  
  if (!is.null(cuts_vector)) {
    dfs_list = split(bigtable, cuts_vector)
    cond <- sapply(dfs_list, function(x) nrow(x) > 0)
    dfs_list <- dfs_list[cond]
    return(dfs_list)
  }
  
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
