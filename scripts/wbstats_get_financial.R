library(wbstats)
library(tidyverse)
load("./produced_data/cepal_33_countries")
source("./functions/add_iso.R")

wb_cachelist = wbcache()

wb_credit_list = wbsearch(pattern = "credit")

wb_reserves_list = wbsearch(pattern = "reserve")

avg_maturity_new_debt_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                            indicator = c("DT.MAT.PRVT", "DT.MAT.OFFT"))

credit_to_cengov_and_soe_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                               indicator = "GFDD.EI.08")

dom_credit_to_priv_sec_to_gdp.GD <- wb(country = cepal_33_countries[["iso3c"]],
                            indicator = "GFDD.DI.14")

dom_credit_to_priv_sec_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                               indicator = "FS.AST.PRVT.GD.ZS")

dom_credit_to_priv_sec_by_banks_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                                      indicator = "FD.AST.PRVT.GD.ZS")

dom_cred_providd_by_finsec_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                                 indicator = "FS.AST.DOMS.GD.ZS")

gen_gov_pubsec_debt_extern_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                                 indicator = "DP.DOD.DECX.CR.GG.Z1")

gross_nfpcorp_ext_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
      indicator = "DP.DOD.DECD.CR.NF.Z1")

gross_gengov_ext_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                        indicator = "DP.DOD.DECD.CR.GG.Z1")

gross_gengov_ext_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                       indicator = "DP.DOD.DECD.CR.CG.Z1")

nplns_to_total <- wb(country = cepal_33_countries[["iso3c"]],
                     indicator = "FB.AST.NPER.ZS")



save(avg_maturity_new_debt_to_gdp,
     credit_to_cengov_and_soe_to_gdp,
     dom_credit_to_priv_sec_to_gdp.GD,
     dom_credit_to_priv_sec_to_gdp,
     dom_credit_to_priv_sec_by_banks_to_gdp,
     dom_cred_providd_by_finsec_to_gdp,
     gen_gov_pubsec_debt_extern_to_gdp,
     gross_nfpcorp_ext_to_gdp,
     gross_gengov_ext_to_gdp,
     gross_gengov_ext_to_gdp,
     file = "./produced_data/data_with_basic_wrangling/wb_credit_to_gdp_dfs")



tot_reserves_w_gold_to_gdp <- wb(country = cepal_33_countries[["iso3c"]],
                                 indicator = "FI.RES.TOTL.CD.ZS")

tot_reserves_as_perc_ext_debt <- wb(country = cepal_33_countries[["iso3c"]],
                                    indicator = "FI.RES.TOTL.DT.ZS")

tot_reserves_in_mo_imp_gds_and_ser <- wb(country = cepal_33_countries[["iso3c"]],
                                         indicator = "FI.RES.TOTL.MO.WB")

tot_reserves_in_mo_imp_gds <- wb(country = cepal_33_countries[["iso3c"]],
                                 indicator = "FI.RES.TOTL.MO")

short_term_debt_to_reserves <- wb(country = cepal_33_countries[["iso3c"]],
                                  indicator = "DT.DOD.DSTC.IR.ZS")
  
bank_liq_res_to_bank_ass <- wb(country = cepal_33_countries[["iso3c"]],
                               indicator = "FD.RES.LIQU.AS.ZS")

bank_cap_to_bank_ass <- wb(country = cepal_33_countries[["iso3c"]],
                           indicator = "FB.BNK.CAPA.ZS")


save(tot_reserves_w_gold_to_gdp,
     tot_reserves_as_perc_ext_debt,
     tot_reserves_in_mo_imp_gds_and_ser,
     tot_reserves_in_mo_imp_gds,
     short_term_debt_to_reserves,
     bank_liq_res_to_bank_ass,
     bank_cap_to_bank_ass,
     nplns_to_total,
     file = "./produced_data/data_with_basic_wrangling/wb_reservish_dfs")
     

wb_interest_list = wbsearch(pattern = "interest")


interest_rate_spread <- wb(country = cepal_33_countries[["iso3c"]],
                           indicator = "FR.INR.LNDP")


bank_lend_dep_spread <- wb(country = cepal_33_countries[["iso3c"]],
      indicator = "GFDD.EI.02")


risk_premium_on_leding <- wb(country = cepal_33_countries[["iso3c"]],
      indicator = "FR.INR.RISK")


real_interest_rate <- wb(country = cepal_33_countries[["iso3c"]],
      indicator = "FR.INR.RINR")


lending_interest_rate <- wb(country = cepal_33_countries[["iso3c"]],
      indicator = "FR.INR.LEND")


save(interest_rate_spread,
     bank_lend_dep_spread ,
     risk_premium_on_leding,
     real_interest_rate ,
     lending_interest_rate,
     file = "./produced_data/data_with_basic_wrangling/wb_interests_dfs")

# trade_to_gdp =  wb(country = cepal_33_countries[["iso3c"]],
#                    indicator = "NE.TRD.GNFS.ZS")
# 
# terms_of_trade = wb(country = cepal_33_countries[["iso3c"]],
#                     indicator = "NE.TRM.TRAD.XU")
# 
# terms_of_trade_idx = wb(country = cepal_33_countries[["iso3c"]],
#                     indicator = "NE.TRM.TRAD.XN")

wb_commodities_list = wbsearch(pattern = "commod")

prod_exp_diver_idx <- wb(country = cepal_33_countries[["iso3c"]],
                                         indicator = "TX.DVR.PROD.XQ")

prod_exp_conc_idx <- wb(country = cepal_33_countries[["iso3c"]],
                         indicator = "TX.CONC.IND.XQ")


library(readr)
IDS_Data <- read_csv("./raw_data/IDS_Data.csv", col_types = 
                     cols(default = col_double(),
                          `Country Name` = col_character(),
                          `Country Code` = col_character(),
                          `Indicator Name` = col_character(),
                          `Indicator Code` = col_character()
                          )
                    )

IDS_Data$X59 <- NULL 

IDS_Data_33 <-  IDS_Data %>% 
  filter(`Country Code` %in% cepal_33_countries[["iso3c"]])

IDS_Data_33_tidy <- IDS_Data_33

names(IDS_Data_33_tidy)[[1]] <- "country_name"
names(IDS_Data_33_tidy)[[2]] <- "iso3c"
names(IDS_Data_33_tidy)[[3]] <- "indicator_name"
names(IDS_Data_33_tidy)[[4]] <- "indicator_code"

IDS_Data_33_tidy$iso2c <- countrycode::countrycode(IDS_Data_33_tidy$iso3c,
                                              "iso3c", "iso2c")

save(IDS_Data_33_tidy, 
     file = "./produced_data/data_with_basic_wrangling/IDS_Data")



