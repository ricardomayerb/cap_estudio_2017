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
  
  new_df <- add_ts_filters(new_df)
  new_df <- add_diffrank(new_df)
}


add_diffrank <- function(df, valuecol_name = "value", datecol_name = "date",
                         hptrend_name = "hp_trend", hpcyclepct_name = "hp_cycle_pct") {
  wrapr::let(alias = list(valuecol = valuecol_name, datecol = datecol_name,
                          hptrend = hptrend_name, hpcyclepct = hpcyclepct_name), 
             expr = {
               
               df_rdiff <- df %>%
                 group_by(iso3c) %>% 
                 arrange(datecol) %>% 
                 mutate(last_pct_chg = 100 * (valuecol - lag(valuecol))/lag(valuecol),
                        diff_lastval = dplyr::last(valuecol) - valuecol,
                        avg_last3 = mean( c(
                          dplyr::last(valuecol),
                          lag(dplyr::last(valuecol)),
                          lag(dplyr::last(valuecol), 2)
                        ),na.rm = TRUE),
                        avg_recent3 = mean(c(valuecol, lag(valuecol), lag(valuecol, 2)),
                                           na.rm = TRUE),
                        diff_avg3 = avg_last3 - avg_recent3,
                        pct_diff_avg3 = 100 * diff_avg3/avg_recent3, 
                        diff_lastpcyple = dplyr::last(hpcyclepct) - hpcyclepct) %>% 
                 ungroup() %>% 
                 arrange(iso3c, datecol)
               
               df_with_ranking <-  df_rdiff %>% 
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
           diff_lastval, diff_avg3, sd_cycle_pct, sd_cycle_pct_2010plus,
           sd_cycle_pct_calm, pct_diff_avg3, pct_diff_avg3, last_pct_chg)
  
  nc = ncol(new_df)
  
  names(new_df)[3:nc] <- paste(names(new_df), suffix, sep = "_")[3:nc]
  
  return(new_df)
}



add_ts_filters <- function(df, date_colname = "date", value_colname = "value",
                           hp_type = "lambda", data_periodicity = "annual"){
  
  df$hp_cycle <- NA
  df$hp_trend <- NA
  df$hp_cycle_pct <- NA
  
  if(data_periodicity == "annual"){
    lambda_value = 6.25
  } else {
    if(data_periodicity == "quarterly"){
      lambda_value = 1600
    } else{
      if (data_periodicity == "monthly") {
        lambda_value = 129600
      }
    }
  }
  
  for (co in unique(df$iso3c)) {
    co_data = df[df$iso3c == co, ]
    co_xts = xts(co_data[[value_colname]], order.by = co_data[[date_colname]])
    
    co_hp = hpfilter(co_xts,  type = "lambda", freq = lambda_value)
    # co_bkfix = bkfilter(co_xts, pl=2, pu=40, type = "fixed") 
    # co_bkvar = bkfilter(co_xts, pl=2, pu=40, type = "variable") 
    
    df$hp_cycle[df$iso3c == co] <- co_hp$cycle
    df$hp_trend[df$iso3c == co] <- co_hp$trend
    df$hp_cycle_pct[df$iso3c == co] <- 100 * co_hp$cycle/co_hp$trend
    
  }
  
  sd_cycle_pct_full_sample <- sd(df$hp_cycle_pct, na.rm = TRUE)
  dates_after_2009 <- year(co_data[[date_colname]]) > 2009
  dates_good_times <- year(co_data[[date_colname]]) %in% c(2003:2007, 2010:2015)
  
  sd_cycle_pct_2010_16 <- sd(df$hp_cycle_pct[dates_after_2009], na.rm = TRUE)
  sd_cycle_pct_calmer <- sd(df$hp_cycle_pct[dates_good_times], na.rm = TRUE)
  
  df$sd_cycle_pct <- sd_cycle_pct_full_sample
  df$sd_cycle_pct_2010plus <- sd_cycle_pct_2010_16
  df$sd_cycle_pct_calm <- sd_cycle_pct_calmer
  
  return(df)
}


make_tab_rank <- function(df, year, suffix) {
  
  avg_recent3_suffix = paste0("avg_recent3_", suffix)
  avg_last3_suffix = paste0("avg_last3_", suffix)
  quartile_recent3_suffix = paste0("quartile_recent3_", suffix)
  quartile_last3_suffix = paste0("quartile_last3_", suffix)
  ranking_recent3_suffix = paste0("ranking_recent3_", suffix)
  ranking_last3_suffix = paste0("ranking_last3_", suffix)
  
  wrapr::let(alias = list(avg_recent3_suffix = avg_recent3_suffix,
                          avg_last3_suffix = avg_last3_suffix,
                          quartile_recent3_suffix = quartile_recent3_suffix,
                          quartile_last3_suffix = quartile_last3_suffix,
                          ranking_recent3_suffix = ranking_recent3_suffix,
                          ranking_last3_suffix = ranking_last3_suffix),
             expr = {
               fil_df <- df %>% filter(year(date) %in% c(year)) %>% 
                 arrange(desc(avg_recent3_suffix))
               
               new_df <- with(fil_df,
                              data.frame(country = iso3c, 
                                         avg_ini = avg_recent3_suffix,
                                         avg_fin = avg_last3_suffix,
                                         qth_ini = quartile_recent3_suffix,
                                         qth_fin = quartile_last3_suffix,
                                         ran_ini = ranking_recent3_suffix,
                                         ran_fin = ranking_last3_suffix))
             })
  return(new_df)
}


make_tab_rank_d <- function(df, year, suffix) {
  
  avg_recent3_suffix = paste0("avg_recent3_", suffix)
  avg_last3_suffix = paste0("avg_last3_", suffix)
  quartile_recent3_suffix = paste0("quartile_recent3_", suffix)
  quartile_last3_suffix = paste0("quartile_last3_", suffix)
  ranking_recent3_suffix = paste0("ranking_recent3_", suffix)
  ranking_last3_suffix = paste0("ranking_last3_", suffix)
  avg_pctcycle_last3_suffix = paste0("avg_pctcycle_last3_", suffix)
  avg_pctcycle_recent3_suffix = paste0("avg_pctcycle_recent3_", suffix)
  pctcycle_last_suffix = paste0("pctcycle_last_", suffix)
  pctcycle_recent_suffix = paste0("pctcycle_recent_", suffix)
  
  wrapr::let(alias = list(avg_recent3_suffix = avg_recent3_suffix,
                          avg_last3_suffix = avg_last3_suffix,
                          quartile_recent3_suffix = quartile_recent3_suffix,
                          quartile_last3_suffix = quartile_last3_suffix,
                          ranking_recent3_suffix = ranking_recent3_suffix,
                          ranking_last3_suffix = ranking_last3_suffix,
                          avg_pctcycle_last3_suffix = avg_pctcycle_last3_suffix,
                          avg_pctcycle_recent3_suffix = avg_pctcycle_recent3_suffix,
                          pctcycle_last_suffix = pctcycle_last_suffix,
                          pctcycle_recent_suffix = pctcycle_recent_suffix),
             expr = {
               fil_df <- df %>% filter(year(date) %in% c(year)) %>% 
                 arrange(desc(avg_recent3_suffix))
               
               new_df <- with(fil_df,
                              data.frame(country = iso3c, 
                                         avg_ini = avg_recent3_suffix,
                                         avg_fin = avg_last3_suffix,
                                         d_ave = avg_last3_suffix - avg_recent3_suffix,
                                         qth_ini = quartile_recent3_suffix,
                                         d_qth = quartile_last3_suffix - quartile_recent3_suffix,
                                         ran_ini = ranking_recent3_suffix,
                                         d_ran_fin = ranking_last3_suffix - ranking_recent3_suffix))
             })
  return(new_df)
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
