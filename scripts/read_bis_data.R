library(tidyverse)
library(readxl)

c_gaps1612_qs <- read_excel("./raw_data/c_gaps1612.xlsx", 
                            sheet = "Quarterly Series", skip = 3)

c_gaps1612_documentation <- read_excel("./raw_data/c_gaps1612.xlsx", 
                                          sheet = "Documentation") 

c_gaps1612_content <- read_excel("./raw_data/c_gaps1612.xlsx", 
                             sheet = "Content")



totcredit_content <- read_excel("./raw_data/totcredit.xlsx", 
                                    sheet = "Content")

totcredit_documentation <- read_excel("./raw_data/totcredit.xlsx",
                    sheet = "Documentation")

totcredit_qs <- read_excel("./raw_data/totcredit.xlsx", 
                               sheet = "Quarterly Series", skip = 2)


dsr_content <- read_excel("./raw_data/dsr.xlsx", 
                      sheet = "Content")

dsr_documentation <- read_excel("./raw_data/dsr.xlsx", 
                                    sheet = "Documentation")

dsr_qs <- read_excel("./raw_data/dsr.xlsx", 
                       sheet = "Quarterly Series")


# after inspecting the data frames, get country names
names(c_gaps1612_documentation)
# backticks ` are necessary bc the variable name includes spaces 
c_gaps1612_countries <- unique(c_gaps1612_documentation$`Borrowers' country`)
# result: ARG, BRA, CHL and MEX

names(totcredit_documentation)
totcredit_countries <- unique(totcredit_documentation$`Borrowers' country`)
# result: ARG, BRA, CHL and MEX

names(dsr_documentation)
dsr_countries <- unique(dsr_documentation$`Borrowers' country`)
# result: BRA and MEX


