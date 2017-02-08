library(tidyverse)
library(XLConnect)
# library(openxlsx)
library(stringr)





wb = loadWorkbook("./raw_data/Datos_monetarios_de_Alejandra.xlsx")

sheet_name = "Tasa_politica"
rstart <-  3
rend <-  373
cstart <-  1
cend <- 34

tpm <- readWorksheet(wb, sheet = sheet_name, startRow = rstart,
                     endRow = rend, startCol = cstart, endCol = cend)



sheet_name = "Meta de Inflación"
rstart <-  5
rend <-  9
cstart <-  1
cend <- 180
meta_inf <- readWorksheet(wb, sheet = sheet_name, startRow = rstart,
                          endRow = rend, startCol = cstart, endCol = cend, header = FALSE)


rstart <-  12
rend <-  16
meta_inf_limsup <- readWorksheet(wb, sheet = sheet_name, startRow = rstart,
                          endRow = rend, startCol = cstart, endCol = cend, header = FALSE)

rstart <-  19
rend <-  23
meta_inf_liminf <- readWorksheet(wb, sheet = sheet_name, startRow = rstart,
                                 endRow = rend, startCol = cstart, endCol = cend, header = FALSE)


sheet_name = "Cartera Vencida"
rstart <-  3
rend <-  280
cstart <-  1
cend <- 34
cartera_vencida <- readWorksheet(wb, sheet = sheet_name, startRow = rstart,
                                 endRow = rend, startCol = cstart, endCol = cend)


sheet_name = "Crédito interno"
country_names_mess <- readWorksheet(wb, sheet = sheet_name,
                               startRow = 1, endRow = 1,
                               startCol = 2, endCol = 199, header = FALSE) 
country_names_units <- country_names_mess[!is.na(country_names_mess)]
country_n_list <- str_split(country_names_units, "\\(")
country_names <- vector(mode = "character", 
                        length = length(country_names_units))

j = 1
for( i in country_n_list ) {
  country_names[[j]] = i[[1]]
  j <- j + 1
}


rstart <-  2
rend <-  323
cstarts <- seq(from = 3, by = 6, length.out = 33)
cends <- cstarts + 5

dfs_ci <- list_along(cstarts)


for (i in seq_along(cstarts) ) {
  
  dfs_ci[[i]] <- readWorksheet(wb, sheet = sheet_name,
                                 startRow = rstart, endRow = rend,
                                 startCol = cstarts[[i]], endCol = cends[[i]]) 
}



sheet_name = "Préstamos bancarios"
rstart <-  5
rend <-  349
cstarts <- seq(from = 2, by = 7, length.out = 33)
cends <- cstarts + 6

dfs_pb <- list_along(cstarts)

for (i in seq_along(cstarts) ) {
  dfs_pb[[i]] <- readWorksheet(wb, sheet = sheet_name,
                               startRow = rstart, endRow = rend,
                               startCol = cstarts[[i]], endCol = cends[[i]]) 
}
