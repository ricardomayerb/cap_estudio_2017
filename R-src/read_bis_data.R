library(tidyverse)
library(readxl)

c_gaps1612_QS <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/c_gaps1612.xlsx", 
                            sheet = "Quarterly Series", skip = 3)

c_gaps1612_documentation <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/c_gaps1612.xlsx", 
                                          sheet = "Documentation") 


c_gaps1612_content <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/c_gaps1612.xlsx", 
                             sheet = "Content")


totcredit_content <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/totcredit.xlsx", 
                                    sheet = "Content")

totcredit_documentation <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/totcredit.xlsx",
                    sheet = "Documentation")

totcredit_qs <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/totcredit.xlsx", 
                           +     sheet = "Quarterly Series", skip = 2)


dsr_content <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/dsr.xlsx", 
                      sheet = "Content")

dsr_documentation <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/dsr.xlsx", 
                                    sheet = "Documentation")

dsr_qs <- read_excel("V:/USR/RMAYER/cw/cap_estudio_2017/data/dsr.xlsx", 
                       sheet = "Quarterly Series")
