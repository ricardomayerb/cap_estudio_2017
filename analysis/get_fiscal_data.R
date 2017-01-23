library(tidyverse)
library("XLConnect")
suppressPackageStartupMessages(library("dplyr"))


### 
# new_cache = wbcache()
lac_iso3c_wb = new_cache$countries %>% filter(regionID=="LCN") %>% select(iso3c, country)

# in WB's LC but not in Cepal's LACs coutries:
# c("ABW", "CUW", "CYM", "MAF", "SXM", "TCA", "VGB", "VIR")

not_cepal_member = c("ABW","BHS", "CUW", "CYM", "MAF", "SXM", "TCA", "VGB", "VIR")
Cepal_33_countries = lac_iso3c_wb %>% filter(! iso3c %in% not_cepal_member)
Cepal_33_iso3c = Cepal_33_countries %>% select(iso3c)

