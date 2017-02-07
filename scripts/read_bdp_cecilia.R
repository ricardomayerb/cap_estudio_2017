library(tidyverse)
library(XLConnect)

bdpwb = loadWorkbook("./raw_data/BdpAnualSICValores10enero2017.xlsx")

cstart <- 1
cend <-  43
cstarts <- rep(cstart, 43)
cends <- rep(cend, 43)

rstartARG = 6
rendARG = 61
rdiffARG = rendARG - rstartARG

rstartBOL = 65
rendBOL = 120
rdiffBOL = rendBOL - rstartBOL

diffBOLARG = rstartBOL - rstartARG

rstarts = seq(from = 6, by = diffBOLARG, length.out = 21)
rstarts

rends <- rstarts + rdiffARG
rends

dfs <- list_along(rstarts)

data = readWorksheet(bdpwb, sheet = 1, startRow = rstartARG, endRow = rendARG,
                     startCol = cstart, endCol = cend)

data2 = readWorksheet(bdpwb, sheet = "BP-América Latina", startRow = rstartARG, endRow = rendARG,
                     startCol = cstart, endCol = cend)


for (i in seq_along(rstarts) ) {
  print(rstarts[[i]])
  print(rends[[i]])
  print(cstarts[[i]])
  print(cends[[i]])
  dfs[[i]] <- readWorksheet(bdpwb, sheet = "BP-América Latina",
                              startRow = rstarts[[i]], endRow = rends[[i]],
                              startCol = cstarts[[i]], endCol = cends[[i]]) 
}

# nameSheet =  "BP-Caribe"
# nameRegion = "Argentina"
# createName(bdpwb, name = nameRegion, formula = paste(nameSheet, "$A$6", sep = "!") )


# wb = loadWorkbook("XLConnectExample1.xlsx", create = TRUE)
# createSheet(wb,  name = "chickSheet")
# writeWorksheet(wb, ChickWeight, sheet = "chickSheet", startRow = 3, startCol = 4)
# saveWorkbook(wb)
#         
# writeWorksheetToFile("XLConnectExample2.xlsx", data = ChickWeight, sheet = "chickSheet", startRow = 3, startCol = 4)
# 
# 
# wb = loadWorkbook("XLConnectExample3.xlsx", create = TRUE)
# createSheet(wb,  name = "womenData")
# createName(wb, name = "womenName", formula = "womenData!$C$5", overwrite = TRUE)
# writeNamedRegion(wb, women, name = "womenName")
# saveWorkbook(wb)
# 
# wb = loadWorkbook("XLConnectExample1.xlsx", create = TRUE)
# data = readWorksheet(wb, sheet = "chickSheet", startRow = 0, endRow = 10, startCol = 0, endCol = 10)
# data
# 
# wb = loadWorkbook("XLConnectExample3.xlsx", create = TRUE)
# data = readNamedRegion(wb, name = "womenName")
# data
# 
# library(zoo)
# library(ggplot2)
# curr <-  XLConnect::swissfranc
# curr <- curr[order(curr$Date), ]
# # View(curr)
# wbFilename = "swiss_franc.xlsx"
# wb = loadWorkbook(wbFilename, create = TRUE)
# sheet = "Swiss_Franc"
# createSheet(wb, name = sheet)
# dataName = "currency"
# nameLocation = paste(sheet, "$A$1", sep = "!")
# createName(wb, name = dataName, formula = nameLocation)
# setStyleAction(wb, XLC$"STYLE_ACTION.DATA_FORMAT_ONLY")
# setDataFormatForType(wb, type = XLC$DATA_TYPE.NUMERIC, format = "0.0000")
# writeNamedRegion(wb, data = curr, name = dataName, header = TRUE)
# saveWorkbook(wb)
