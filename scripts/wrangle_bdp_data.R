library(tidyverse)
library(stringr)
library(lubridate)

load("./produced_data/from_BdP_excel")
load("./produced_data/cepal_33_countries")
load("./produced_data/cepal_20_countries")

dfs_joined <- bind_rows(dfs_bpal)
new_names <- dfs_joined %>% names()  %>% str_replace("X", "") 
names(dfs_joined) <- new_names

dfs_long_form_BPAL <- dfs_joined %>% 
  gather(Year, Value , `1980`:`2016`) %>% 
  arrange(areaID, Grupo, Year) %>% 
  mutate(Year = as.numeric(Year) ) %>% 
  rename(iso3c = areaID) %>% 
  tbl_df()
 



dfs_joined <- bind_rows(dfs_bpcar)
new_names <- dfs_joined %>% names()  %>% str_replace("X", "") 
names(dfs_joined) <- new_names

dfs_long_form_BPC <- dfs_joined %>% 
  gather(Year, Value , `1980`:`2016`) %>% 
  arrange(areaID, Grupo, Year) %>% 
  mutate(Year = as.numeric(Year) ) %>% 
  rename(iso3c = areaID) %>% 
  tbl_df()

rm(dfs_joined)

bop_tidy_BPALC <-  bind_rows(dfs_long_form_BPAL, dfs_long_form_BPC) %>% 
                      arrange(iso3c, Grupo, Year)

bop_tidy_BPALC$iso2c <- countrycode(bop_tidy_BPALC$iso3c, "iso3c", "iso2c")

names(bop_tidy_BPALC) <- str_to_lower(names(bop_tidy_BPALC))

bop_tidy_20 <- filter(bop_tidy_BPALC, iso3c %in% cepal_20_countries[["iso3c"]])

save(bop_tidy_BPALC, bop_tidy_20, 
     file = "./produced_data/bop_edd_tidy")

