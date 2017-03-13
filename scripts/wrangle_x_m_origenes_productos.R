library(tidyverse)
library(countrycode)
library(stringr)

load("./produced_data/x_m_por_origen_destino")
load("./produced_data/x_m_por_producto")

load("./produced_data/cepal_20_countries")
load("./produced_data/cepal_33_countries")

imp_by_ori <- bind_rows(dfs_m)
names(imp_by_ori)[1] <- "origin_region"
imp_by_ori$nombre_pais <- str_to_title(imp_by_ori$nombre_pais)
imp_by_ori$nombre_pais <- recode(imp_by_ori$nombre_pais, 
                            "Bolivia" = "Bolivia (Estado Plurinacional de)",
                            "Venezuela" = "Venezuela (República Bolivariana de)"
                                 )
imp_by_ori$iso2c <- countrycode(imp_by_ori$nombre_pais, "country.name.es",
                                "iso2c", custom_dict = cepal_33_countries)

imp_by_ori$iso3c <- countrycode(imp_by_ori$nombre_pais, "country.name.es",
                                "iso3c", custom_dict = cepal_33_countries)


exp_by_dest <- bind_rows(dfs_x)
names(exp_by_dest)[1] <- "dest_region"
exp_by_dest$nombre_pais <- str_to_title(exp_by_dest$nombre_pais)
exp_by_dest$nombre_pais <- recode(exp_by_dest$nombre_pais, 
                                 "Bolivia" = "Bolivia (Estado Plurinacional de)",
                                 "Venezuela" = "Venezuela (República Bolivariana de)"
)
exp_by_dest$iso2c <- countrycode(exp_by_dest$nombre_pais, "country.name.es",
                                "iso2c", custom_dict = cepal_33_countries)

exp_by_dest$iso3c <- countrycode(exp_by_dest$nombre_pais, "country.name.es",
                                "iso3c", custom_dict = cepal_33_countries)



imp_by_prod <- bind_rows(dfs_m_p)
# names(imp_by_prod)[1] <- "origin_region"
imp_by_prod$nombre_pais <- str_to_title(imp_by_prod$nombre_pais)
imp_by_prod$nombre_pais <- recode(imp_by_prod$nombre_pais, 
                                 "Bolivia" = "Bolivia (Estado Plurinacional de)",
                                 "Venezuela" = "Venezuela (República Bolivariana de)",
                                 "Mexico" = "México",
                                 "Rep. Dominicana" = "República Dominicana"
                                 )

imp_by_prod$iso2c <- countrycode(imp_by_prod$nombre_pais, "country.name.es",
                                "iso2c", custom_dict = cepal_33_countries)

imp_by_prod$iso3c <- countrycode(imp_by_prod$nombre_pais, "country.name.es",
                                 "iso3c", custom_dict = cepal_33_countries)


exp_by_prod <- bind_rows(dfs_x_p)
# names(exp_by_prod)[1] <- "origin_region"
exp_by_prod$nombre_pais <- str_to_title(exp_by_prod$nombre_pais)
exp_by_prod$nombre_pais <- recode(exp_by_prod$nombre_pais, 
                                  "Bolivia" = "Bolivia (Estado Plurinacional de)",
                                  "Venezuela" = "Venezuela (República Bolivariana de)",
                                  "Mexico" = "México",
                                  "Rep. Dominicana" = "República Dominicana"
)

exp_by_prod$iso2c <- countrycode(exp_by_prod$nombre_pais, "country.name.es",
                                 "iso2c", custom_dict = cepal_33_countries)

exp_by_prod$iso3c <- countrycode(exp_by_prod$nombre_pais, "country.name.es",
                                 "iso3c", custom_dict = cepal_33_countries)


exp_by_dest_tidy <- exp_by_dest
exp_by_prod_tidy <- exp_by_prod
imp_by_ori_tidy <- imp_by_ori
imp_by_prod_tidy <- imp_by_prod

# save(exp_by_dest_tidy, exp_by_prod_tidy, imp_by_ori_tidy, imp_by_prod_tidy,
#      file = "./produced_data/x_m_locations_products_tidy")
save(exp_by_dest_tidy, exp_by_prod_tidy, imp_by_ori_tidy, imp_by_prod_tidy,
     file = "./produced_data/data_with_basic_wrangling/x_m_locations_products_tidy")
