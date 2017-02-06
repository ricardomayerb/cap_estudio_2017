library(tidyverse)
library(XLConnect)

bdpwb = loadWorkbook("./raw_data/BdpAnualSICValores10enero2017.xlsx")

nameSheet =  "BP-Caribe"
nameRegion = "Argentina"
createName(bdpwb, name = nameRegion, formula = paste(nameSheet, "$A$6", sep = "!") )


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
