library(tidyverse)
 
# 
# 
# cepalstat_sector_real_dolares_completo <- read_delim("./data/cepalstat_sector_real_dolares_anual_completo.csv", 
#                                             ";", escape_double = FALSE, 
#                                             trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))
# new_cols = ncol(cepalstat_sector_real_dolares_completo) - 1
# cepalstat_sector_real_dolares_completo = select(cepalstat_sector_real_dolares_completo, 1:new_cols)
# save(cepalstat_sector_real_dolares_completo, file="./output/cepalstat_sector_real_dolares_anual")
# 

cepalstat_desempleo <- read_delim("./data/cepalstat_desempleo.csv", 
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE,
                              locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_desempleo) - 1
cepalstat_desempleo = select(cepalstat_desempleo, 1:new_cols)
save(cepalstat_desempleo , file = "./output/cepalstat_desempleo")


cepalstat_empleo <- read_delim("./data/cepalstat_empleo.csv", 
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE,
                               locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_empleo) - 1
cepalstat_empleo = select(cepalstat_empleo, 1:new_cols)
save(cepalstat_empleo , file = "./output/cepalstat_empleo")



cepalstat_remuneraciones <- read_delim("./data/cepalstat_remuneraciones.csv", 
                               ";", escape_double = FALSE, 
                               trim_ws = TRUE,
                               locale = locale("es", encoding="windows-1252"))
new_cols = ncol(cepalstat_remuneraciones) - 1
cepalstat_remuneraciones = select(cepalstat_remuneraciones, 1:new_cols)
save(cepalstat_remuneraciones , file = "./output/cepalstat_remuneraciones")



cepalstat_sector_financiero_monetario <- read_delim("./data/cepalstat_sector_financiero_monetario.csv", 
                                       ";", escape_double = FALSE, 
                                       trim_ws = TRUE, locale = locale("es", encoding="windows-1252",asciify=FALSE ))
new_cols = ncol(cepalstat_sector_financiero_monetario) - 1
cepalstat_sector_financiero_monetario = select(cepalstat_sector_financiero_monetario, 1:new_cols)
save(cepalstat_sector_financiero_monetario , file = "./output/cepalstat_sector_financiero_monetario")



cepalstat_BP_anual <- read_delim("./data/cepalstat_BP_anual.csv", 
                                                    ";", escape_double = FALSE, 
                                                    trim_ws = TRUE, locale = locale("es", encoding="windows-1252",asciify=FALSE ))
new_cols = ncol(cepalstat_BP_anual) - 1
cepalstat_BP_anual = select(cepalstat_BP_anual, 1:new_cols)
save(cepalstat_BP_anual , file = "./output/cepalstat_BP_anual")



cepalstat_BP_trimestral <- read_delim("./data/cepalstat_BP_trimestral.csv", 
                                 ";", escape_double = FALSE, 
                                 trim_ws = TRUE, locale = locale("es", encoding="windows-1252",asciify=FALSE ))
new_cols = ncol(cepalstat_BP_trimestral) - 1
cepalstat_BP_trimestral = select(cepalstat_BP_trimestral, 1:new_cols)
save(cepalstat_BP_trimestral, file = "./output/cepalstat_BP_trimestral")



cepalstat_indicadores_derivados_de_la_BP <- read_delim("./data/cepalstat_indicadores_derivados_de_la_BP.csv", 
                                      ";", escape_double = FALSE, 
                                      trim_ws = TRUE, locale = locale("es", encoding="windows-1252",asciify=FALSE ))
new_cols = ncol(cepalstat_indicadores_derivados_de_la_BP) - 1
cepalstat_indicadores_derivados_de_la_BP = select(cepalstat_indicadores_derivados_de_la_BP, 1:new_cols)
save(cepalstat_indicadores_derivados_de_la_BP, file = "./output/cepalstat_indicadores_derivados_de_la_BP")



cepalstat_deuda_externa <- read_delim("./data/cepalstat_deuda_externa.csv", 
                                                       ";", escape_double = FALSE, 
                                                       trim_ws = TRUE, locale = locale("es", encoding="windows-1252",asciify=FALSE ))
new_cols = ncol(cepalstat_deuda_externa) - 1
cepalstat_deuda_externa = select(cepalstat_deuda_externa, 1:new_cols)
save(cepalstat_deuda_externa, file = "./output/cepalstat_deuda_externa")




