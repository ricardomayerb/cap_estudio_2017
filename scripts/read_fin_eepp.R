library(tidyverse)
library(readxl)

fin_eepp <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/raw_data/base_datos_financiera_EP.xlsx")

save(fin_eepp, file = "./produced_data/fin_eepp")


