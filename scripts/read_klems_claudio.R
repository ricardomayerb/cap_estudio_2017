library(readxl)
library(tidyverse)

# workaround bc of the unwanted message DEFINEDNAME: blah blah ... blame the libxml package
quiet_read <- purrr::quietly(readxl::read_excel)
quiet_sheets <- purrr::quietly(readxl::excel_sheets)


## read chile
path_chi = "./raw_data/chi_output_09II.xls"
sheets_chi = quiet_sheets(path_chi)$result
# data_in_sheets_chi = lapply(sheets_chi, read_excel, path = path_chi)

# klems_dfs_chi_loop = list()
# 
# # loop version, it's actually pretty concise
# for(s in sheets_chi){
#   # print(s)
#   klems_dfs_chi_loop[[s]] = quiet_read(path = path_chi, sheet = s)$result
#   # print("done")
# }

klems_dfs_chi_map <- path_chi %>% 
                      map2(sheets_chi, function(x, y)  quiet_read(x, sheet = y) ) %>% 
                      map("result") 
names(klems_dfs_chi_map) <- sheets_chi


## read argentina
path_arg = "./raw_data/ar_output_09II.xls"
sheets_arg = quiet_sheets(path_arg)$result

klems_dfs_arg_map <- path_arg %>% 
  map2(sheets_arg, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_arg_map) <- sheets_arg


## read colombia
path_col = "./raw_data/co_output_09II.xls"
sheets_col = quiet_sheets(path_col)$result

klems_dfs_col_map <- path_col %>% 
  map2(sheets_col, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_col_map) <- sheets_col


## read brazil
path_bra = "./raw_data/br_output_09II.xls"
sheets_bra = quiet_sheets(path_bra)$result

klems_dfs_bra_map <- path_bra %>% 
  map2(sheets_bra, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_bra_map) <- sheets_bra


## read mexico
path_mex = "./raw_data/mex_output_09II_menor_agregacion.xls"
sheets_mex = quiet_sheets(path_mex)$result

klems_dfs_mex_map <- path_mex %>% 
  map2(sheets_mex, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_mex_map) <- sheets_mex

save(klems_dfs_arg_map, klems_dfs_bra_map, klems_dfs_chi_map, klems_dfs_col_map, 
     klems_dfs_mex_map, file="./produced_data/klems_ar_br_cl_co_mx")


