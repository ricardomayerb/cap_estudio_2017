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


stand_tidying <- function(df) {
  
  old_names <- df %>% select(-c(notas, fuente)) %>% 
    names()
  
  df <- df %>% select(-c(notas, fuente))
  
  new_names <- old_names

  # new_names <- str_replace(new_names, "Años", "year") %>% 
  #   str_to_lower() %>% 
  #   str_replace_all(" ", "_") %>% 
  #   stri_trans_general("Latin-ASCII") %>% 
  #   str_replace("pais", "nombre_pais")

  new_names <- str_replace(new_names, "Años", "year") %>% 
    stand_string() %>% 
    str_replace("pais", "nombre_pais") 
  
  print(new_names)
  new_df <- df
  names(new_df) <- new_names
  
  new_df <- new_df   %>%
    mutate_if(is.character, stand_string)
  
  
  

  print(new_df)
  
  
  return(new_names)
}


load("./produced_data/x_m_por_producto")
exp_by_prod <- bind_rows(dfs_x_p)

load("./produced_data/from_BdP_excel")


dfs_joined <- bind_rows(dfs_bpal)
new_names <- dfs_joined %>% names()  %>% str_replace("X", "") 
names(dfs_joined) <- new_names

load("./produced_data/cepalstat_sector_publico")

foo <- stand_tidying(cepalstat_sector_publico)

print(foo)

# %>% 
#   mutate_if(is.character(.), funs(. = stand_string(.)) )

