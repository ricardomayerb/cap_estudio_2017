library(tidyverse)
library(wbstats)

load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_20_countries")


# sources = updated_cache[[2]]$source
# JEDH_indicators = filter(updated_cache[[2]], source=="(JEDH) Joint External Debt Hub")
# JEDH_indicators_codes = JEDH_indicators$indicatorID
# 
# # shorter, using the fact we know now to look for JEDH as parte of the name of the source
# # long description is not included

JEDH_vars = wbsearch(pattern = "JEDH", cache=wb_cachelist, fields = "source")

debt_data_cepal_33 <- wb(country = cepal_33_countries[["iso3c"]],
                          indicator = JEDH_vars[["indicatorID"]],
                          mrv = 80)

debt_data_cepal_20 = debt_data_cepal_33 %>% 
                      filter(iso2c %in% cepal_20_countries[["iso2c"]])

save(debt_data_cepal_33, file = "./produced_data/debt_data_JEDH_cepal_33")
save(debt_data_cepal_20, file = "./produced_data/debt_data_JEDH_cepal_20")


