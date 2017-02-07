library(tidyverse)
library(XLConnect)


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