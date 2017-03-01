library(tidyverse)
library(stringr)
library(lubridate)

load("./produced_data/klems_ar_br_cl_co_mx")

arg_notes_df <- klems_dfs_arg_map$Notes
colnames(arg_notes_df) <- c("table_name", "description", "status")
arg_notes_df_ok <- arg_notes_df %>% filter(status == "OK")
arg_list_sheets_ok <- klems_dfs_arg_map[arg_notes_df_ok[["table_name"]]]


bra_notes_df <- klems_dfs_bra_map$Notes
colnames(bra_notes_df) <- c("table_name", "description", "status")
bra_notes_df_ok <- bra_notes_df %>% filter(status == "OK")
bra_list_sheets_ok <- klems_dfs_bra_map[bra_notes_df_ok[["table_name"]]]


chl_notes_df <- klems_dfs_chi_map$Notes
colnames(chl_notes_df) <- c("table_name", "description", "status")
chl_notes_df_ok <- chl_notes_df %>% filter(status == "OK")
chl_list_sheets_ok <- klems_dfs_chi_map[chl_notes_df_ok[["table_name"]]]


col_notes_df <- klems_dfs_col_map$Notes
colnames(col_notes_df) <- c("table_name", "description", "status")
col_notes_df_ok <- col_notes_df %>% filter(status == "OK")
col_list_sheets_ok <- klems_dfs_col_map[col_notes_df_ok[["table_name"]]]


mex_notes_df <- klems_dfs_mex_map$Notes
colnames(mex_notes_df) <- c("table_name", "description", "status")
mex_notes_df_ok <- mex_notes_df %>% filter(status == "OK")
mex_list_sheets_ok <- klems_dfs_mex_map[mex_notes_df_ok[["table_name"]]]


mex_finer_notes_df <- klems_dfs_mex_finer_map$Notes
colnames(mex_finer_notes_df) <- c("table_name", "description", "status")
mex_finer_notes_df_ok <- mex_finer_notes_df %>% filter(status == "OK")
mex_finer_list_sheets_ok <- klems_dfs_mex_finer_map[
                                mex_finer_notes_df_ok[["table_name"]]]



arg_dfs_long <- list_along(arg_list_sheets_ok)
for (i in 1:length(arg_list_sheets_ok)){
  this_df <- arg_list_sheets_ok[[i]]
  this_df <- filter(this_df, desc != is.na(desc))
  names(this_df) <- str_replace_all(names(this_df), "_", "")
  this_table <- names(arg_list_sheets_ok)[[i]]
  this_df$basic_table <- this_table
  df_long <- gather(this_df, date, value, -c(desc, code, basic_table))
  df_long$value[ df_long$value == "error" ] <- NA
  df_long$value <- as.numeric(df_long$value)
  arg_dfs_long[[i]] <- df_long
  
}
arg_long <- bind_rows(arg_dfs_long)


bra_dfs_long <- list_along(bra_list_sheets_ok)
for (i in 1:length(bra_list_sheets_ok)){
  this_df <- bra_list_sheets_ok[[i]]
  this_df <- filter(this_df, desc != is.na(desc))
  names(this_df) <- str_replace_all(names(this_df), "_", "")
  this_table <- names(bra_list_sheets_ok)[[i]]
  this_df$basic_table <- this_table
  df_long <- gather(this_df, date, value, -c(desc, code, basic_table))
  df_long$value[ df_long$value == "error" ] <- NA
  df_long$value <- as.numeric(df_long$value)
  bra_dfs_long[[i]] <- df_long
  
}
bra_long <- bind_rows(bra_dfs_long)


chl_dfs_long <- list_along(chl_list_sheets_ok)
for (i in 1:length(chl_list_sheets_ok)){
  this_df <- chl_list_sheets_ok[[i]]
  this_df <- filter(this_df, desc != is.na(desc))
  names(this_df) <- str_replace_all(names(this_df), "_", "")
  this_table <- names(chl_list_sheets_ok)[[i]]
  this_df$basic_table <- this_table
  df_long <- gather(this_df, date, value, -c(desc, code, basic_table))
  df_long$value[ df_long$value == "error" ] <- NA
  df_long$value <- as.numeric(df_long$value)
  chl_dfs_long[[i]] <- df_long
  
}
chl_long <- bind_rows(chl_dfs_long)


col_dfs_long <- list_along(col_list_sheets_ok)
for (i in 1:length(col_list_sheets_ok)){
  this_df <- col_list_sheets_ok[[i]]
  this_df <- filter(this_df, desc != is.na(desc))
  names(this_df) <- str_replace_all(names(this_df), "_", "")
  this_table <- names(col_list_sheets_ok)[[i]]
  this_df$basic_table <- this_table
  df_long <- gather(this_df, date, value, -c(desc, code, basic_table))
  df_long$value[ df_long$value == "error" ] <- NA
  df_long$value <- as.numeric(df_long$value)
  col_dfs_long[[i]] <- df_long
  
}
col_long <- bind_rows(col_dfs_long)


mex_dfs_long <- list_along(mex_list_sheets_ok)
for (i in 1:length(mex_list_sheets_ok)){
  this_df <- mex_list_sheets_ok[[i]]
  this_df <- filter(this_df, desc != is.na(desc))
  names(this_df) <- str_replace_all(names(this_df), "_", "")
  this_table <- names(mex_list_sheets_ok)[[i]]
  this_df$basic_table <- this_table
  df_long <- gather(this_df, date, value, -c(desc, code, basic_table))
  df_long$value[ df_long$value == "error" ] <- NA
  df_long$value <- as.numeric(df_long$value)
  mex_dfs_long[[i]] <- df_long
  print(dim(df_long))
}
mex_long <- bind_rows(mex_dfs_long)


mex_finer_dfs_long <- list_along(mex_finer_list_sheets_ok)
for (i in 1:length(mex_finer_list_sheets_ok)){
  this_df <- mex_finer_list_sheets_ok[[i]]
  this_df <- filter(this_df, desc != is.na(desc))
  names(this_df) <- str_replace_all(names(this_df), "_", "")
  this_table <- names(mex_finer_list_sheets_ok)[[i]]
  this_df$basic_table <- this_table
  df_long <- gather(this_df, date, value, -c(desc, code, basic_table))
  df_long$value[ df_long$value == "error" ] <- NA
  df_long$value <- as.numeric(df_long$value)
  mex_finer_dfs_long[[i]] <- df_long
}
mex_finer_long <- bind_rows(mex_finer_dfs_long)


klems_9_tidy = bind_rows(arg_long, bra_long, chl_long, col_long, mex_long)

save(mex_finer_long, klems_9_tidy, file = "./produced_data/klems_tidy")
