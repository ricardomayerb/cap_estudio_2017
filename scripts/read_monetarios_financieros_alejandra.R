library(tidyverse)
library(XLConnect)
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
country_names_mess_ci <- readWorksheet(wb, sheet = sheet_name,
                               startRow = 1, endRow = 1,
                               startCol = 2, endCol = 199, header = FALSE) 

country_names_credito_interno <- country_names_mess_ci %>% 
                                 select_if(! is.na(country_names_mess_ci)) %>% 
                                 str_split( "\\(") %>% 
                                 map_chr( .f = c(1,1)) %>% 
                                 str_trim()

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
country_names_mess_pb <- readWorksheet(wb, sheet = sheet_name,
                                    startRow = 4, endRow = 4,
                                    startCol = 2, endCol = 240, header = FALSE) 
country_names_prestamos_bancarios <- country_names_mess_pb %>% 
                                      select_if(! is.na(country_names_mess_pb)) %>% 
                                      str_split( "\\(") %>% 
                                      map_chr( .f = c(1,1)) %>% 
                                      str_trim()


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
