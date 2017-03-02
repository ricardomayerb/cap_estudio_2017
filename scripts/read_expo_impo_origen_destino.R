library(tidyverse)
library(stringr)
library(XLConnect)
library(lubridate)

x_m_oridest = loadWorkbook("./raw_data/expo_e_impo_por_destino_y_origen.xlsx")

num_of_countries = 16

all_dates = readWorksheet(x_m_oridest, sheet = 1, header = FALSE,
                          startRow = 2, endRow = 2, startCol = 2,
                          colTypes = "DATETIME")

all_dates_ch <- as.character.Date(all_dates, format = "%Y-%m-%d")

# all_dates <- cbind("dates", all_dates)


## read country names
rstart_names <- seq(from = 3, by = 21, length.out = num_of_countries)
all_names <- list_along(rstart_names)

for (i in seq_along(rstart_names) ) {
  
  this_name <- readWorksheet(x_m_oridest, sheet = 1, header = FALSE,
                             startRow = rstart_names[[i]],
                             endRow = rstart_names[[i]],
                             startCol = 1, endCol = 1) 
  
  all_names[[i]] <- this_name[[1]]
}


## read exports (x) data
rstart_x <- seq(from=5, by=21, length.out = num_of_countries)
n_x <- 7
rend_x <- rstart_x + n_x

dfs_x <- list_along(rstart_x)

for (i in seq_along(rstart_x) ) {
  
  this_df <- readWorksheet(x_m_oridest, sheet = 1, header = FALSE,
                          startRow = rstart_x[[i]], endRow = rend_x[[i]],
                          startCol = 1)
  
  this_df[5,1] <- "China"
  this_df[6,1] <- "Resto_de_Asia"
  
  colnames(this_df) <- append(c("region"), all_dates_ch)
  
  this_df$nombre_pais <- rep(all_names[[i]], n_x+1)
  
  this_df <- gather(this_df, date, value, -c(region, nombre_pais))
  
  dfs_x[[i]] <- this_df
}



## read imports (m) data
rstart_m <- seq(from=14, by=21, length.out = num_of_countries)
n_m <- 7
rend_m <- rstart_m + n_m

dfs_m <- list_along(rstart_m)

for (i in seq_along(rstart_m) ) {
  
  this_df <- readWorksheet(x_m_oridest, sheet = 1, header = FALSE,
                           startRow = rstart_m[[i]], endRow = rend_m[[i]],
                           startCol = 1)
  
  this_df[1,1] <- "Mundo"
  this_df[5,1] <- "China"
  this_df[6,1] <- "Resto_de_Asia"
  
  colnames(this_df) <- append(c("region"), all_dates_ch)
  
  this_df$nombre_pais <- rep(all_names[[i]], n_m+1)
  
  this_df <- gather(this_df, date, value, -c(region, nombre_pais))

  dfs_m[[i]] <- this_df
}

save(dfs_m, dfs_x, file = "./produced_data/x_m_por_origen_destino")

