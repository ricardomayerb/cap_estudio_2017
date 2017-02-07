library(tidyverse)
library(stringr)
library(lubridate)

load("./produced_data/from_BdP_excel")


dfs_joined <- bind_rows(dfs_bpal)
new_names <- dfs_joined %>% names()  %>% str_replace("X", "") 
names(dfs_joined) <- new_names

dfs_long_form_BPAL <- dfs_joined %>% 
  gather(Year, Value , `1980`:`2016`) %>% 
  arrange(areaID, Grupo, Year) %>% 
  mutate(Year = as.numeric(Year) ) %>% 
  rename(ISO3c = areaID) %>% 
  tbl_df()
 



dfs_joined <- bind_rows(dfs_bpcar)
new_names <- dfs_joined %>% names()  %>% str_replace("X", "") 
names(dfs_joined) <- new_names

dfs_long_form_BPC <- dfs_joined %>% 
  gather(Year, Value , `1980`:`2016`) %>% 
  arrange(areaID, Grupo, Year) %>% 
  mutate(Year = as.numeric(Year) ) %>% 
  rename(ISO3c = areaID) %>% 
  tbl_df()

rm(dfs_joined)

dfs_long_form_BPALC <-  bind_rows(dfs_long_form_BPAL, dfs_long_form_BPC) %>% 
                      arrange(ISO3c, Grupo, Year)

save(dfs_long_form_BPALC, file = "./produced_data/Balance_of_Payments_Cecilia_tidyed_up")

