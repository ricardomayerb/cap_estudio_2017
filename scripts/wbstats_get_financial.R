library(wbstats)
library(tidyverse)
load("./produced_data/cepal_33_countries")

# wb_cachelist = wbcache()

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

bank_liq_res_to_bank_ass <- wb(country = cepal_33_countries[["iso3c"]],
                           indicator = "FB.BNK.CAPA.ZS")


save(tot_reserves_w_gold_to_gdp,
     tot_reserves_as_perc_ext_debt,
     tot_reserves_in_mo_imp_gds_and_ser,
     tot_reserves_in_mo_imp_gds,
     short_term_debt_to_reserves,
     bank_liq_res_to_bank_ass,
     bank_liq_res_to_bank_ass,
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

