# Reading  Cecilia's excel workbook (Balance of Payments data)

library(tidyverse)
library(stringr)
library(XLConnect)
library(lubridate)

bdpwb = loadWorkbook("./raw_data/BdpAnualSICValores10enero2017.xlsx")

## import tables from sheet = BP-America Latina
cstart <- 1
cend <-  43
cstarts <- rep(cstart, 43)
cends <- rep(cend, 43)

rstart <-  6
rend <-  61
row_block <-  rend - rstart
rstart_next <-  65
rows_to_next <-  rstart_next - rstart

number_of_tables <- 21

rstarts <-  seq(from = 6, by = rows_to_next, length.out = number_of_tables)
rends <- rstarts + row_block

dfs_bpal <- list_along(rstarts)

sheet_name <- "BP-América Latina"

for (i in seq_along(rstarts) ) {
  
  dfs_bpal[[i]] <- readWorksheet(bdpwb, sheet = sheet_name,
                              startRow = rstarts[[i]], endRow = rends[[i]],
                              startCol = cstarts[[i]], endCol = cends[[i]]) 
}



## import tables from sheet = BP-Caribe
cstart <- 1
cend <-  43
cstarts <- rep(cstart, 43)
cends <- rep(cend, 43)

rstart <-  6
rend <-  61
row_block <-  rend - rstart
rstart_next <-  65
rows_to_next <-  rstart_next - rstart

number_of_tables <- 14

rstarts <-  seq(from = 6, by = rows_to_next, length.out = number_of_tables)
rends <- rstarts + row_block

dfs_bpcar <- list_along(rstarts)

sheet_name <- "BP-Caribe"

for (i in seq_along(rstarts) ) {
  
  dfs_bpcar[[i]] <- readWorksheet(bdpwb, sheet = sheet_name,
                                 startRow = rstarts[[i]], endRow = rends[[i]],
                                 startCol = cstarts[[i]], endCol = cends[[i]]) 
}


save(dfs_bpal, dfs_bpcar, file = "./produced_data/from_BdP_excel")






