library(wbstats)
library(tidyverse)
load("./produced_data/cepal_33_countries")
source("./functions/add_iso.R")
library(WDI)

wb_cachelist = wbcache()

# wb_credit_list = wbsearch(pattern = "credit")

wb_government_list = wbsearch(pattern = "government")

# avg_maturity_new_debt_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                            indicator = c("DT.MAT.PRVT", "DT.MAT.OFFT"))


# save(     gross_gengov_ext_to_gdp,
#      file = "./produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs")


# net_incof_liab_dom_gdp <- wb(country = cepal_33_countries[["iso3c"]],
#                                 indicator = c("GC.FIN.DOMS.GD.ZS"))


net_incof_liab_ext_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                                indicator = c("GC.FIN.FRGN.GD.ZS"), cache = wb_cachelist)  

net_incof_liab_ext_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                             indicator = c("GC.LBL.TOT.GD.ZS"))                                

                                
gov_net_lending <- wb(country = cepal_33_countries[["iso3c"]],
                      indicator = c("GC.NLD.TOTL.GD.ZS"))


wdi_results_liabilities <- WDIsearch(string = "liabil", field = "name")

net_incof_liab_dom_gdp <- WDI(country = cepal_33_countries[["iso2c"]], indicator = "GC.FIN.DOMS.GD.ZS")
