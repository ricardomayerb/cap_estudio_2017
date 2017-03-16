library(dplyr)
library(replyr)
library(wrapr)
# 
# fiso <- list(c("AR", "SV"), c("CH", "CO", "MX"), c("BR", "PE"))
# 
# load("./produced_data/data_with_basic_wrangling/cs_deuda_externa")
# load("./produced_data/cepal_33_countries")

break_down_table <- function(bigtable, iso2c_list=NULL,
                             cuts_vector=NULL, ...) {

  if (!is.null(iso2c_list)) {
    dfs_list = vector("list", length = length(iso2c_list))
    for(i in 1:length(iso2c_list)){
      idx = bigtable$iso2c %in% iso2c_list[[i]]
      dfs_list[[i]] <- bigtable[idx, ]
    }
    
    return(dfs_list)
  }

  if (!is.null(cuts_vector)) {
    dfs_list = split(bigtable, cuts_vector)
    cond <- sapply(dfs_list, function(x) nrow(x) > 0)
    dfs_list <- dfs_list[cond]
    return(dfs_list)
  }
  
}

# mydf = cs_deuda_externa
# 
# 
# deuda_chica <- cs_deuda_externa %>% 
#   filter(str_detect(indicador, "porcentaje"))  %>% 
#   sample_n(size = 300)
# cuts = c(0, 25, 50, 100, 500, 600, 1500, 2000)
# cut_factors = cut(deuda_chica$valor, cuts, include.lowest = TRUE)
# 
# moo <- break_down_table(mydf, iso2c_list = fiso)
# str(moo)
# # View(moo[[1]])
# 
# goo <- break_down_table(deuda_chica, cuts_vector = cut_factors)
# str(goo)
# 
# cuts_quantile <- cut(deuda_chica$valor, quantile(deuda_chica$valor), include.lowest = TRUE)
# soo <- break_down_table(deuda_chica, cuts_vector = cuts_quantile)
# str(soo)


