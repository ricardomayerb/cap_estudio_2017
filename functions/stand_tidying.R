library(stringi)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(countrycode)

load("./produced_data/cepal_33_countries")

stand_string <- function(mystring) {
  
  new_string <-  mystring %>% 
    str_to_lower() %>% 
    str_replace_all(" ", "_") %>% 
    str_replace_all("/", "_") %>% 
    stri_trans_general("Latin-ASCII") 
    
  return(new_string)
} 


stand_tidying <- function(df, country_col = "nombre_pais", lang = "es" ) {
  
  old_names <- df %>% select(-c(notas, fuente)) %>% 
    names()
  
  df <- df %>% select(-c(notas, fuente))
  
  new_df <- df
  
  new_names <- old_names
  
  new_names <- str_replace(new_names, "Años", "year") %>% 
    stand_string() %>% 
    str_replace("pais", "nombre_pais") 
  
  
  print(new_names)
  
  names(new_df) <- new_names
  
  # class(new_df[[country_col]])
  # print(unique(new_df[[country_col]]))

  # sfoo <- countrycode(new_df[[country_col]],
  #                      custom_dict = cepal_33_countries,
  #                      origin = "country.name.es",
  #                      destination = "iso3c")
  # 
  #  sfoo2 <- countrycode(new_df[[country_col]],
  #                      custom_dict = cepal_33_countries,
  #                      origin = "country.name.es",
  #                      destination = "iso2c")
  # 
  #  efoo <- countrycode(new_df[[country_col]],
  #                      custom_dict = cepal_33_countries,
  #                      origin = "country.name.en",
  #                      destination = "iso3c")
  #  
  #  efoo2 <- countrycode(new_df[[country_col]],
  #                       custom_dict = cepal_33_countries,
  #                       origin = "country.name.en",
  #                       destination = "iso2c")
  # 
  #  print(unique(sfoo))
  #  print(unique(sfoo2))
  #  print(unique(efoo))
  #  print(unique(efoo2))
  #  
   print("antes del loop")
  
  if (lang == "es") {
    print("loop spanish")
    iso3_names <-  countrycode(new_df[[country_col]],
                                  custom_dict = cepal_33_countries,
                                  origin = "country.name.es",
                                  destination = "iso3c")
    
    iso2_names <- countrycode(new_df[[country_col]],
                         custom_dict = cepal_33_countries,
                         origin = "country.name.es",
                         destination = "iso2c")
    
  } else {
    print("loop english")
    iso3_names = countrycode(new_df$nombre_pais, 
                             custom_dict = cepal_33_countries, 
                             origin = "country.name.en", 
                             destination = "iso3c")
    
    iso2_names = countrycode(new_df$nombre_pais, 
                             custom_dict = cepal_33_countries, 
                             origin = "country.name.en", 
                             destination = "iso2c")
  }

   print("after loop")
   
  print(unique(iso3_names))
  print(unique(iso2_names))

  new_df$iso3c <- iso3_names
  new_df$iso2c <- iso2_names
  
  new_df <- new_df   %>%
    filter(!is.na(iso3c) & !is.na(iso2c)) %>% 
    mutate_if(is.character, stand_string)
  
  

  list_of_outputs = list(wdf = new_df)
  print("finishing")
  return(list_of_outputs)
}


load("./produced_data/x_m_por_producto")
exp_by_prod <- bind_rows(dfs_x_p)

load("./produced_data/from_BdP_excel")


dfs_joined <- bind_rows(dfs_bpal)
new_names <- dfs_joined %>% names()  %>% str_replace("X", "") 
names(dfs_joined) <- new_names

load("./produced_data/cepalstat_sector_publico")

res1 <- stand_tidying(cepalstat_sector_publico)

cs_sp_stdty <- res1[["wdf"]]



# print(cs_sp_stdty)

# %>% 
#   mutate_if(is.character(.), funs(. = stand_string(.)) )

