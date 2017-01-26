library(tidyverse)
library(wbstats)

updated_cache = wbcache()

# sources = updated_cache[[2]]$source
# JEDH_indicators = filter(updated_cache[[2]], source=="(JEDH) Joint External Debt Hub")
# JEDH_indicators_codes = JEDH_indicators$indicatorID
# 
# # shorter, using the fact we know now to look for JEDH as parte of the name of the source
# # long description is not included

JEDH_vars = wbsearch(pattern = "JEDH", cache=updated_cache, fields = "source")

load("cepal_33_countries")

debt_data <- wb(country = cepal_33_countries[["iso3c"]], indicator = JEDH_vars[["indicatorID"]],
                   mrv = 10)