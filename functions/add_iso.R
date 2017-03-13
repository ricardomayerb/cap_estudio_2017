library(countrycode)
library(dplyr)
library(replyr)
# library(purrr)

add_iso <- function(df, names_col, dict, lang="es", rm_nf = FALSE) {
  if (lang == "es") {
    ori <-  "country.name.es"
    new_names_col <- "nombre_pais"
    
  } else {
    ori = "country.name.en"
    new_names_col <- "country_name"
  }
  
  # replyr::let(
  wrapr::let(
      alias = list(names_col = names_col, new_names_col = new_names_col),
    expr = {
    df <- df %>% 
    mutate(iso3c = countrycode(df$names_col,
                               custom_dict = dict,
                               origin = ori,
                               destination = "iso3c"),
           iso2c = countrycode(df$names_col,
                               custom_dict = dict,
                               origin = ori,
                               destination = "iso2c")
           ) %>% 
    filter(!is.na(iso3c)) %>% 
    rename(new_names_col = names_col)
    })
  
  if (rm_nf) {
    df <- df %>% select(-c(fuente, notas))
  }
  
  return(df)
}