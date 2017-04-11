library(dplyr)
library(tidyr)
library(xts)
library(replyr)
library(wrapr)
library(mFilter)

make_df_19_wbtype <- function(df) {
  df %>% 
    filter(iso2c %in% cepal_19_countries[["iso2c"]]) %>% 
    arrange(iso2c, date) %>% 
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c"),
           date = ymd(paste(date, "12", "31", sep = "-")) ) %>% 
    mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                          ordered = TRUE))
}


make_df_19_cstype <- function(df) {
  df %>% 
    filter(iso3c %in% cepal_19_countries[["iso3c"]]) %>% 
    arrange(iso3c, year) %>% 
    mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c"),
           date = ymd(paste(as.character(year), "12", "31", sep = "-")) ) %>% 
    mutate(iso3c = factor(iso3c, levels = cepal_19_countries[["iso3c"]],
                          ordered = TRUE)) %>% 
    rename(value = valor)
}


make_df_diff_hp <- function(df, type = "wb") {
  
  if (type == "wb") {
    new_df <- make_df_19_wbtype(df)
  } else {
    new_df <- make_df_19_cstype(df)
  }
  
  new_df <- add_diffrank(new_df)
  new_df <- add_ts_filters(new_df)
}


add_diffrank_old <- function(df, valuecol_name = "value", datecol_name = "date") {
  wrapr::let(alias = list(valuecol = valuecol_name, datecol = datecol_name), 
             expr = {
               df_with_ranking <-  df %>% 
                 group_by(datecol) %>% 
                 arrange(desc(valuecol)) %>%
                 mutate(ranking = dense_rank(valuecol),
                        quartile = ntile(valuecol, 4),
                        half = ntile(valuecol, 2)
                 ) %>%
                 ungroup() %>% 
                 arrange(datecol, iso3c)
               
               df_rdiff <- df_with_ranking %>%
                 group_by(iso3c) %>% 
                 arrange(datecol) %>% 
                 mutate(diff_lastval = dplyr::last(valuecol) - valuecol,
                        diff_avg3 = mean( c(
                          dplyr::last(valuecol),
                          lag(dplyr::last(valuecol)),
                          lag(dplyr::last(valuecol), 2)
                        ),na.rm = TRUE
                        ) - 
                          mean(c(valuecol, lag(valuecol), lag(valuecol, 2)),
                               na.rm = TRUE)  ) %>% 
                 ungroup() %>% 
                 arrange(iso3c, datecol)
             })
  
} 


add_diffrank_semi_new <- function(df, valuecol_name = "value", datecol_name = "date") {
  wrapr::let(alias = list(valuecol = valuecol_name, datecol = datecol_name), 
             expr = {
               df_with_ranking <-  df %>% 
                 group_by(datecol) %>% 
                 arrange(valuecol) %>%
                 mutate(ranking = dense_rank(valuecol),
                        quartile = ntile(valuecol, 4),
                        half = ntile(valuecol, 2)
                 ) %>%
                 ungroup() %>% 
                 arrange(datecol, iso3c)
               
               df_rdiff <- df_with_ranking %>%
                 group_by(iso3c) %>% 
                 arrange(datecol) %>% 
                 mutate(diff_lastval = dplyr::last(valuecol) - valuecol,
                        avg_last3 = mean( c(
                          dplyr::last(valuecol),
                          lag(dplyr::last(valuecol)),
                          lag(dplyr::last(valuecol), 2)
                        ),na.rm = TRUE),
                        avg_recent3 = mean(c(valuecol, lag(valuecol), lag(valuecol, 2)),
                                           na.rm = TRUE),
                        diff_avg3 = avg_last3 - avg_recent3,
                        ranking_recent3 = dense_rank(avg_recent3),
                        quartile_recent3 = ntile(avg_recent3, 4),
                        half_recent3 = ntile(avg_recent3, 2),
                        ranking_last3 = dense_rank(avg_last3),
                        quartile_last3 = ntile(avg_last3, 4),
                        half_last3 = ntile(avg_last3, 2)) %>% 
                 ungroup() %>% 
                 arrange(iso3c, datecol)
             })
  
} 


add_diffrank <- function(df, valuecol_name = "value", datecol_name = "date") {
  wrapr::let(alias = list(valuecol = valuecol_name, datecol = datecol_name), 
             expr = {
               
               df_rdiff <- df %>%
                 group_by(iso3c) %>% 
                 arrange(datecol) %>% 
                 mutate(diff_lastval = dplyr::last(valuecol) - valuecol,
                        avg_last3 = mean( c(
                          dplyr::last(valuecol),
                          lag(dplyr::last(valuecol)),
                          lag(dplyr::last(valuecol), 2)
                        ),na.rm = TRUE),
                        avg_recent3 = mean(c(valuecol, lag(valuecol), lag(valuecol, 2)),
                                           na.rm = TRUE),
                        diff_avg3 = avg_last3 - avg_recent3) %>% 
                 ungroup() %>% 
                 arrange(iso3c, datecol)
               
               df_with_ranking_rdiff <-  df_rdiff %>% 
                 group_by(datecol) %>% 
                 arrange(valuecol) %>%
                 mutate(ranking = dense_rank(valuecol),
                        quartile = ntile(valuecol, 4),
                        half = ntile(valuecol, 2),
                        ranking_recent3 = dense_rank(avg_recent3),
                        quartile_recent3 = ntile(avg_recent3, 4),
                        half_recent3 = ntile(avg_recent3, 2),
                        ranking_last3 = dense_rank(avg_last3),
                        quartile_last3 = ntile(avg_last3, 4),
                        half_last3 = ntile(avg_last3, 2)
                 ) %>%
                 ungroup() %>% 
                 arrange(datecol, iso3c)
               
             })
} 


prepare_tm <- function(df, suffix) {
  new_df <- df %>%
    select(iso3c, date, value, ranking, quartile, half, hp_cycle_pct, hp_trend,
           ranking_recent3, quartile_recent3, half_recent3, avg_recent3, 
           ranking_last3, quartile_last3, half_last3, avg_last3, 
           diff_lastval, diff_avg3)
  
  nc = ncol(new_df)
  
  names(new_df)[3:nc] <- paste(names(new_df), suffix, sep = "_")[3:nc]
  
  return(new_df)
}


make_country_lists_by_quant <- function(df) {
  q_all = df %>% select(iso3c, gen_group) %>% distinct(.keep_all = TRUE)
  
  q_4 = q_all %>% filter(gen_group == "above_pct75") %>% select(iso3c)
  q_3 = q_all %>% filter(gen_group == "pct50_to_75") %>% select(iso3c)
  q_2 = q_all %>% filter(gen_group == "pct25_to_50") %>% select(iso3c)
  q_1 = q_all %>% filter(gen_group == "below_pct25") %>% select(iso3c)
  
  return(list(q_1 = q_1, q_2 = q_2, q_3 = q_3, q_4 = q_4, q_all = q_all))
}


add_ts_filters <- function(df, date_colname = "date", value_colname = "value",
                           hp_type = "lambda", hp_freq = 1){
  
  df$hp_cycle <- NA
  df$hp_trend <- NA
  df$hp_cycle_pct <- NA
  
  for (co in unique(df$iso3c)) {
    co_data = df[df$iso3c == co, ]
    co_xts = xts(co_data[[value_colname]], order.by = co_data[[date_colname]])
    
    co_hp = hpfilter(co_xts,  type = "lambda", freq = 1)
    # co_bkfix = bkfilter(co_xts, pl=2, pu=40, type = "fixed") 
    # co_bkvar = bkfilter(co_xts, pl=2, pu=40, type = "variable") 
    
    df$hp_cycle[df$iso3c == co] <- co_hp$cycle
    df$hp_trend[df$iso3c == co] <- co_hp$trend
    df$hp_cycle_pct[df$iso3c == co] <- 100 * co_hp$cycle/co_hp$trend
    
  }
  
  return(df)
}


add_baselines <- function(df, value_colname = "value", date_colname = "date",
                          init_date = as.Date("2006", format = "%Y"),
                          final_date = as.Date("2016", format = "%Y"),
                          init_window = 3, final_window = 3) {
  
  wrapr::let(alias = list(value_col = value_colname, date_col = date_colname),
             expr = {
               
               in_win = year(init_date) - init_window + 1
               fi_win = year(final_date) - final_window + 1
              
               df_init <- df %>%
                 filter(year(date_col) >= in_win & year(date_col) <= year(init_date)) %>% 
                 group_by(iso3c) %>% 
                 summarise(init_avg = mean(value_col, na.rm = TRUE),
                           init_val = dplyr::last(value_col))
               
               
               df_final <- df %>%
                 filter(year(date_col) >= fi_win & year(date_col) <= year(final_date)) %>% 
                 group_by(iso3c) %>% 
                 summarise(final_avg = mean(value_col, na.rm = TRUE),
                           final_val = dplyr::last(value_col))
               
               df_infi <- left_join(df_init, df_final, by = "iso3c") %>% 
                 mutate(dif_values = final_val - init_val,
                        dif_avgs = final_avg - init_avg) %>% 
                 arrange(iso3c)
               
               
               joined_df <- full_join(df, df_infi, by = "iso3c")
             
               augmented_df <- joined_df %>% 
                 mutate(value_m_val = value_col - init_val,
                        value_m_avg = value_col - init_avg)
               
               return(augmented_df)
             })
  
}


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
      new_df <- df %>% arrange(iso3c, date_col) %>% 
        mutate(median_of_avg = median(value_col, na.rm = TRUE)) %>% 
        group_by(iso3c) %>% 
        mutate(avg_value = mean(value_col, na.rm = TRUE)) %>% 
        mutate(gen_group = if_else(avg_value > median_of_avg, "above_median",
                                    "below_median"))
      return(new_df)
    } else {
      if (is_pct3) {
        new_df <- df  %>% arrange(iso3c, date_col) %>% 
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
        new_df <- df %>% arrange(iso3c, date_col) %>% 
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
