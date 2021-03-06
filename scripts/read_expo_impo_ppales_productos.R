library(tidyverse)
library(stringr)
library(XLConnect)
library(lubridate)

x_m_prod = loadWorkbook("./raw_data/expo_e_impo_por_grupo_de_productos.xlsx")


num_of_countries = 16

all_dates = readWorksheet(x_m_prod, sheet = 1, header = FALSE,
                          startRow = 3, endRow = 3, startCol = 2,
                          colTypes = "DATETIME")

all_dates_ch <- as.character.Date(all_dates, format = "%Y-%m-%d")

# all_dates <- cbind("dates", all_dates)


## read country names
rstart_names <- seq(from=4, by=12, length.out = num_of_countries)
all_names <- list_along(rstart_names)

for (i in seq_along(rstart_names) ) {
  
  this_name <- readWorksheet(x_m_prod, sheet = 1, header = FALSE,
                             startRow = rstart_names[[i]],
                             endRow = rstart_names[[i]],
                             startCol = 1, endCol = 1) 
  
  all_names[[i]] <- this_name[[1]]
}


## read exports (x) data
rstart_x <- seq(from=5, by=12, length.out = num_of_countries)
n_x <- 3
rend_x <- rstart_x + n_x

dfs_x_p <- list_along(rstart_x)

for (i in seq_along(rstart_x) ) {
  
  this_df <- readWorksheet(x_m_prod, sheet = 1, header = FALSE,
                          startRow = rstart_x[[i]], endRow = rend_x[[i]],
                          startCol = 1)
  
  # print(ncol(this_df))
  
  this_df[1,1] <- "total"
  
  colnames(this_df) <- append(c("producto"), all_dates_ch[1:ncol(this_df)-1])
  
  this_df$nombre_pais <- rep(all_names[[i]], n_x+1)
  
  this_df <- gather(this_df, date, value, -c(producto, nombre_pais))
  
  dfs_x_p[[i]] <- this_df
}



## read imports (m) data
rstart_m <- seq(from=10, by=12, length.out = num_of_countries)
n_m <- 4
rend_m <- rstart_m + n_m

dfs_m_p <- list_along(rstart_m)

for (i in seq_along(rstart_m) ) {
  
  this_df <- readWorksheet(x_m_prod, sheet = 1, header = FALSE,
                           startRow = rstart_m[[i]], endRow = rend_m[[i]],
                           startCol = 1)
  
  this_df[1,1] <- "total"
  
  colnames(this_df) <- append(c("producto"), all_dates_ch[1:ncol(this_df)-1])
  
  this_df$nombre_pais <- rep(all_names[[i]], n_m+1)
  
  this_df <- gather(this_df, date, value, -c(producto, nombre_pais))

  dfs_m_p[[i]] <- this_df
}

save(dfs_m_p, dfs_x_p, file = "./produced_data/x_m_por_producto")

