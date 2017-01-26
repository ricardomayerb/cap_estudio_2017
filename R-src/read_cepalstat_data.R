library(tidyverse)
 


cepalstat_sector_real_dolares_completo <- read_delim("./data/cepalstat_sector_real_dolares_anual_completo.csv", 
                                            ";", escape_double = FALSE, 
                                            trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_sector_real_dolares_completo) - 1
cepalstat_sector_real_dolares_completo = select(cepalstat_sector_real_dolares_completo, 1:new_cols)
save(cepalstat_sector_real_dolares_completo, file="sector_real_dolares_anual_cepalstat")


cepalstat_desempleo <- read_delim("./data/cepalstat_desempleo.csv", 
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE,
                              locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_desempleo) - 1
cepalstat_desempleo = select(cepalstat_desempleo, 1:new_cols)
save(cepalstat_desempleo , file = "cepalstat_desempleo")


cepalstat_empleo <- read_delim("./data/cepalstat_empleo.csv", 
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE,
                               locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_empleo) - 1
cepalstat_empleo = select(cepalstat_empleo, 1:new_cols)
save(cepalstat_empleo , file = "cepalstat_empleo")



cepalstat_remuneraciones <- read_delim("./data/cepalstat_remuneraciones.csv", 
                               ";", escape_double = FALSE, 
                               trim_ws = TRUE,
                               locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_remuneraciones) - 1
cepalstat_remuneraciones = select(cepalstat_remuneraciones, 1:new_cols)
save(cepalstat_remuneraciones , file = "cepalstat_remuneraciones")



cepalstat_sector_financiero_monetario <- read_delim("./data/cepalstat_sector_financiero_monetario.csv", 
                                       ";", escape_double = FALSE, 
                                       trim_ws = TRUE, locale = locale("es", encoding="windows-1252",asciify=FALSE ))
new_cols = ncol(cepalstat_sector_financiero_monetario) - 1
cepalstat_sector_financiero_monetario = select(cepalstat_sector_financiero_monetario, 1:new_cols)
save(cepalstat_sector_financiero_monetario , file = "cepalstat_sector_financiero_monetario")
