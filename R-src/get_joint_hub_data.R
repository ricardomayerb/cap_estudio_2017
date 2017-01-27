library(tidyverse)
library(wbstats)
library(xts)

# run this line only once, it's time consuming and no longer necessary once you hace updated the cache
# wb_cachelist = wbcache()



# sources = updated_cache[[2]]$source
# JEDH_indicators = filter(updated_cache[[2]], source=="(JEDH) Joint External Debt Hub")
# JEDH_indicators_codes = JEDH_indicators$indicatorID
# 
# # shorter, using the fact we know now to look for JEDH as parte of the name of the source
# # long description is not included

JEDH_vars = wbsearch(pattern = "JEDH", cache=wb_cachelist, fields = "source")

load("./output/cepal_33_countries")

debt_data <- wb(country = cepal_33_countries[["iso3c"]], indicator = JEDH_vars[["indicatorID"]],
                   mrv = 80)



# in case you don't want to download this everytime, save it and load it as need it
save(debt_data, file = "./output/debt_data_JEDH")
# load("./output/debt_data_JEDH")

