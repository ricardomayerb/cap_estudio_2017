library(countrycode)
library(dplyr)

add_iso_codes <- function(df, names_col, dict, lang="es") {
  if (lang == "es") {
    ori = "country.name.es"
  } else {
    ori = "country.name.en"
  }
  iso3_country_codes <-  countrycode(df[[names_col]],
                             custom_dict = dict,
                             origin = ori,
                             destination = "iso3c")

  iso2_country_codes <-  countrycode(df[[names_col]],
                                     custom_dict = dict,
                                     origin = ori,
                                     destination = "iso2c")
  
  df <- df %>% 
    mutate(iso3c = countrycode(df[[names_col]],
                               custom_dict = dict,
                               origin = ori,
                               destination = "iso3c"),
           iso2c = countrycode(df[[names_col]],
                               custom_dict = dict,
                               origin = ori,
                               destination = "iso2c")
           ) %>% 
    filter(!is.na(iso3c))
  
}