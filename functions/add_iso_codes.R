library(countrycode)
library(dplyr)

add_iso_codes <- function(df, names_col, dict, lang="es") {
  if (lang == "es") {
    ori <-  "country.name.es"
    new_names_col <- "nombre_pais"
    
  } else {
    ori = "country.name.en"
    new_names_col <- "country_name"
  }

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
    filter(!is.na(iso3c)) %>% 
    rename(new_names_col = names_col)
  
}