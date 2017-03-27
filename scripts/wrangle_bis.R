library(tidyverse)
library(stringr)
library(countrycode)

load("./produced_data/bis_gap_tc_dsr")

names(c_gaps1612_qs) <- str_trim(names(c_gaps1612_qs))
names(dsr_qs) <- str_trim(names(dsr_qs))
names(totcredit_qs) <- str_trim(names(totcredit_qs))

c_g_qs_long <- gather(c_gaps1612_qs, key = code, value, -Period) %>% 
  rename(date = Period) %>% 
  mutate(country_name = 
           countrycode(
             str_extract(code, "(?<=:).*?(?=:)" ),
             "iso2c",
             "country.name.en")
         )



dsr_qs_long <- gather(dsr_qs, key = code, value, -Period) %>% 
  rename(date = Period) %>% 
  mutate(country_name = 
           countrycode(
             str_extract(code, "(?<=:).*?(?=:)" ),
             "iso2c",
             "country.name.en")
  )


tc_qs_long <- gather(totcredit_qs, key = code, value, -Period) %>% 
  rename(date = Period) %>% 
  mutate(country_name = 
           countrycode(
             str_extract(code, "(?<=:).*?(?=:)" ),
             "iso2c",
             "country.name.en")
  )


save(c_gaps1612_content, c_gaps1612_documentation, 
     dsr_content, dsr_documentation, totcredit_content, totcredit_documentation,
     c_g_qs_long, dsr_qs_long, tc_qs_long, file = "./produced_data/data_with_basic_wrangling/bis_tidy")

# foo <- dsr_documentation$Code
# str_extract(foo, "(?<=:).*?(?=:)" )
# str_extract(foo[1], "(?<=:).*?(?=:)" )
# str_extract(dsr_qs_long$code, "(?<=:).*?(?=:)" )
# countrycode(str_extract(dsr_qs_long$code, "(?<=:).*?(?=:)" ), "iso2c", "country.name.en")
