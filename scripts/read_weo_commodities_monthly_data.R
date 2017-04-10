library(openxlsx)
library(tibble)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(xts)

commodity_description <- read.xlsx( "./raw_data/commodity_description.xlsx")

commodity_data <- read.xlsx("./raw_data/commodity_data.xlsx",
                             cols = c(1:64) )

commodity_data_sel <- commodity_data %>% 
  select(Frequency, PALLFNF, PNFUEL, PFOOD, PMETA, PNRG, POILAPSP, PCOFFOTM, PCOPP,
         PNGASUS, PSMEA, PSUGAISA) %>% 
  mutate(date = paste0(str_replace(Frequency, "M", "-"),"-15") ,
         date = ymd(date)) %>% 
  select(-Frequency)


commodity_data_sel_long <- commodity_data_sel %>% 
  gather(key = commodity, value = value, PALLFNF:PSUGAISA)


commodity_data_xts = as.xts(commodity_data_sel, 
                            order.by = commodity_data_sel$date)
commodity_data_xts$date = NULL

commodity_description_sel <- commodity_description %>% 
  select(Commodity, PALLFNF, PNFUEL, PFOOD, PMETA, PNRG, POILAPSP, PCOFFOTM, PCOPP,
         PNGASUS, PSMEA, PSUGAISA)
  
save(commodity_data_sel, commodity_description_sel, commodity_data_xts,
     file = "./produced_data/data_with_basic_wrangling/commodities")


