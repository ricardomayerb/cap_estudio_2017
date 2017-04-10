library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(stringi)

load("./produced_data/data_with_basic_wrangling/cs_sector_publico")

names_cs_sp <- names(cs_sector_publico)
names_cs_sp_std <- stri_trans_general(names_cs_sp, "Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower()
names(cs_sector_publico) <- names_cs_sp_std

uin <- unique(cs_sector_publico$indicador)
uin_std <- uin %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")

new_indicador <-  recode(cs_sector_publico$indicador,
                         `Ingresos tributarios por tipo de impuestos en porcentajes del PIB (América latina)` = uin_std[[1]],
                         `Ingresos tributarios por tipo de impuestos, en moneda nacional a precios corrientes` = uin_std[[2]],
                         `Alícuotas del impuesto a la renta` = uin_std[[3]],
                         `Alícuotas del Impuesto al Valor Agregado` = uin_std[[4]],
                         `Operaciones del gobierno (clasificación económica), en porcentajes del PIB` = uin_std[[5]],
                         `Operaciones de gobierno (clasificación económica), en moneda nacional a precios corrientes` = uin_std[[6]],
                         `Saldo de la deuda pública en porcentajes del PIB` = uin_std[[7]],
                         `Saldo de la deuda pública en millones de dólares` = uin_std[[8]])
cs_sector_publico$indicador <- new_indicador

uci <- unique(cs_sector_publico$clasificacion_impuestos)
uci_std <- uci %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")


new_clasi_imp <-  recode(cs_sector_publico$clasificacion_impuestos,
                         `Comercio y transacciones internacionales` = uci_std[[1]],
                         `Contribuciones sociales` = uci_std[[2]],
                         `Corporaciones y empresas` = uci_std[[3]],
                         `Impuestos específicos sobre bienes y servicios` = uci_std[[4]],
                         `Impuestos generales sobre bienes y servicios` = uci_std[[5]],
                         `Impuestos sobre ingreso, utilidades y ganancias de capital` = uci_std[[6]],
                         `Impuestos sobre la propiedad` = uci_std[[7]],
                         `Ingresos tributarios directos` = uci_std[[8]],
                         `Ingresos tributarios indirectos` = uci_std[[9]],
                         `No clasificables` = uci_std[[10]],
                         `Otros impuestos` = uci_std[[11]],
                         `Otros indirectos` = uci_std[[12]],
                         `Personas físicas` = uci_std[[13]],
                         `Total ingresos tributarios` = uci_std[[14]],
                         `Total ingresos tributarios (incluyendo contribuciones sociales)` = uci_std[[15]],
                         `Otros impuestos directos` = uci_std[[16]])
cs_sector_publico$clasificacion_impuestos <- new_clasi_imp


ucob <- unique(cs_sector_publico$cobertura_institucional)
ucob_std <- ucob %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")


new_cob_instit <-  recode(cs_sector_publico$cobertura_institucional,
                         `Gobierno general` = ucob_std[[1]],
                         `Gobierno_central` = ucob_std[[2]])
cs_sector_publico$cobertura_institucional <- new_cob_instit



ucob1 <- unique(cs_sector_publico$cobertura_institucional_1)
ucob1_std <- ucob1 %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")
new_cob_instit1 <-  recode(cs_sector_publico$cobertura_institucional_1,
                          `Gobierno central` = ucob1_std[[2]],
                          `Gobierno general` = ucob1_std[[3]],
                          `Sector público no financiero` = ucob1_std[[4]])
cs_sector_publico$cobertura_institucional_1 <- new_cob_instit1


ucob2 <- unique(cs_sector_publico$cobertura_institucional_2)
ucob2_std <- ucob2 %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")
new_cob_instit2 <-  recode(cs_sector_publico$cobertura_institucional_2,
                           `Gobierno central` = ucob2_std[[2]],
                           `Gobiernos subnacionales` = ucob2_std[[4]],
                           `Sector público no financiero` = ucob2_std[[3]],
                           `Sector público` = ucob2_std[[5]])
cs_sector_publico$cobertura_institucional_2 <- new_cob_instit2



uper <- unique(cs_sector_publico$periodo)
uper_std <- uper %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")
new_per <-  recode(cs_sector_publico$periodo,
                           `1999 ó 2000` = uper_std[[5]],
                           `Junio 2001` = uper_std[[6]],
                           `Diciembre 2002` = uper_std[[7]],
                           `Julio 2003` = uper_std[[8]],
                           `Julio 2004` = uper_std[[9]],
                           `Julio 2005` = uper_std[[10]],
                           `Febrero 2006` = uper_std[[11]],
                           `Mayo 2007` = uper_std[[12]])
cs_sector_publico$periodo <- new_per


uper1 <- unique(cs_sector_publico$periodo_1)
uper1_std <- uper1 %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")
new_per1 <-  recode(cs_sector_publico$periodo_1,
                   `Tasa inicial` = uper1_std[[2]],
                   `Julio 1992` = uper1_std[[3]],
                   `Octubre 1993` = uper1_std[[4]],
                   `Marzo 1994` = uper1_std[[5]],
                   `Septiembre 1995` = uper1_std[[6]],
                   `Julio 1996` = uper1_std[[7]],
                   `Junio 1997` = uper1_std[[8]],
                   `Marzo 1999` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]],
                   `Mayo 2007` = uper1_std[[9]])
cs_sector_publico$periodo_1 <- new_per1



uceo <- unique(cs_sector_publico$clasificacion_economica_operaciones_del_gobierno)
uceo_std <- uceo %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")
 
new_cleog <-  recode(cs_sector_publico$clasificacion_economica_operaciones_del_gobierno,
                          `Ingreso total y donaciones` = uceo_std[[2]],
                          `Ingresos corrientes` = uceo_std[[3]],
                          `Ingresos tributarios` = uceo_std[[4]],
                          `Ingresos no tributarios` = uceo_std[[5]],
                          `Ingresos de capital` = uceo_std[[6]],
                          `Donaciones externas` = uceo_std[[7]],
                          `Gasto total y préstamo neto` = uceo_std[[8]],
                          `Gastos corrientes` = uceo_std[[9]],
                          `Sueldos y salarios` = uceo_std[[10]],
                          `Compras de bienes y servicios` = uceo_std[[11]],
                          `Pagos de intereses` = uceo_std[[12]],
                          `Subsidios y transferencias corrientes` = uceo_std[[13]],
                          `Otros gastos corrientes` = uceo_std[[14]],
                          `Gastos de capital` = uceo_std[[15]],
                          `Adquisición de activos de capital fijo` = uceo_std[[16]],
                          `Transferencias de capital` = uceo_std[[17]],
                          `Otros gastos de capital` = uceo_std[[18]],
                          `Concesión de préstamos menos recuperaciones` = uceo_std[[19]],
                          `Resultado primario` = uceo_std[[20]],
                          `Resultado global` = uceo_std[[21]],
                          `Financiamiento total` = uceo_std[[22]],
                          `Financiamiento interno` = uceo_std[[23]],
                          `Financiamiento externo` = uceo_std[[24]],
                          `Financiamiento otro` = uceo_std[[25]])
cs_sector_publico$clasificacion_economica_operaciones_del_gobierno <- new_cleog


ucld <- unique(cs_sector_publico$clasificacion_deuda)
ucld_std <- ucld %>% stri_trans_general("Latin-ASCII") %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>% 
  str_replace_all(",", "") %>% 
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "")


new_clas_deu <-  recode(cs_sector_publico$clasificacion_deuda,
                          `Total deuda pública (clasificación por residencia)` = ucld_std[[2]],
                          `Deuda interna` = ucld_std[[3]],
                          `Deuda externa` = ucld_std[[4]]  )
cs_sector_publico$clasificacion_deuda <- new_clas_deu


save(cs_sector_publico, file = "./produced_data/data_with_basic_wrangling/cs_sector_publico_clean_names")
