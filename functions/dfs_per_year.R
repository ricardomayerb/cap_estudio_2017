library(dplyr)
library(replyr)
library(wrapr)

dfs_per_year <- function(bigdf, vec_of_years) {
  
  dfs_list = vector("list", length = length(vec_of_years))
  
  for(i in 1:length(vec_of_years)){
    this_year = vec_of_years[i]
    this_df = bigdf %>% 
    filter(year == this_year)
    dfs_list[[i]] = this_df
  }
  return(dfs_list)

}


# 
# load("./produced_data/data_with_basic_wrangling/cs_deuda_externa")
# years = c(1990, 1999, 2009, 2015)
# 
# new_cs <- cs_deuda_externa %>% 
#   rename(year = Años)
# 
# mylist = dfs_per_year(new_cs, years)
