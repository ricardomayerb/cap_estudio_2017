library(readxl)
library(tidyverse)

# workaround bc of the unwanted message DEFINEDNAME: blah blah ... blame the libxml package
quiet_read <- purrr::quietly(readxl::read_excel)


## read chile
path_chi = "./raw_data/chi_output_09II.xls"
sheets_chi = excel_sheets(path_chi)
# data_in_sheets_chi = lapply(sheets_chi, read_excel, path = path_chi)

klems_dfs_chi_loop = list()

# loop version, it's actually pretty concise
for(s in sheets_chi){
  # print(s)
  klems_dfs_chi_loop[[s]] = quiet_read(path = path_chi, sheet = s)$result
  # print("done")
}

klems_dfs_chi_map <- path_chi %>% 
                      map2(sheets_chi, function(x, y)  quiet_read(x, sheet = y) ) %>% 
                      map("result") 
names(klems_dfs_chi_map) <- sheets_chi


## read argentina
path_arg = "./raw_data/ar_output_09II.xls"
sheets_arg = excel_sheets(path_arg)

klems_dfs_arg_map <- path_arg %>% 
  map2(sheets_arg, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_arg_map) <- sheets_arg


## read colombia
path_col = "./raw_data/co_output_09II.xls"
sheets_col = excel_sheets(path_col)

klems_dfs_col_map <- path_col %>% 
  map2(sheets_col, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_col_map) <- sheets_col


## read brazil
path_bra = "./raw_data/br_output_09II.xls"
sheets_bra = excel_sheets(path_bra)

klems_dfs_bra_map <- path_bra %>% 
  map2(sheets_bra, function(x, y)  quiet_read(x, sheet = y) ) %>% 
  map("result") 
names(klems_dfs_bra_map) <- sheets_bra




