library(tidyverse)
 



cepalstat_sector_real_dolares_completo <- read_delim("./data/cepalstat_sector_real_dolares_anual_completo.csv",
                                              ";", escape_double = FALSE,
                                              trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_desempleo <- read_delim("./data/cepalstat_desempleo.csv", 
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE,
                                  locale = locale("es", encoding="windows-1252"))

cepalstat_empleo <- read_delim("./data/cepalstat_empleo.csv", 
                               ";", escape_double = FALSE, 
                               trim_ws = TRUE,
                               locale = locale("es", encoding="windows-1252"))

cepalstat_remuneraciones <- read_delim("./data/cepalstat_remuneraciones.csv", 
                                       ";", escape_double = FALSE, 
                                       trim_ws = TRUE,
                                       locale = locale("es", encoding="windows-1252"))

cepalstat_sector_financiero_monetario <- read_delim("./data/cepalstat_sector_financiero_monetario.csv", 
                                                    ";", escape_double = FALSE, 
                                                    trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_sector_publico2 <- read_delim("./data/cepalstat_sector_publico2.csv", 
                                        ";", escape_double = FALSE, 
                                        trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_BP_anual <- read_delim("./data/cepalstat_BP_anual.csv", 
                                 ";", escape_double = FALSE, 
                                 trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_BP_trimestral <- read_delim("./data/cepalstat_BP_trimestral.csv", 
                                      ";", escape_double = FALSE, 
                                      trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_indicadores_derivados_de_la_BP <- read_delim("./data/cepalstat_indicadores_derivados_de_la_BP.csv", 
                                                       ";", escape_double = FALSE, 
                                                       trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_exp_imp_servicios <- read_delim("./data/cepalstat_exp_imp_servicios.csv", 
                                          ";", escape_double = FALSE, 
                                          trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_deuda_externa <- read_delim("./data/cepalstat_deuda_externa.csv", 
                                      ";", escape_double = FALSE, 
                                      trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_comercio_intrarregional <- read_delim("./data/cepalstat_comercio_intrarregional.csv", 
                                                ";", escape_double = FALSE, 
                                                trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_exp_imp_grandes_cat <- read_delim("./data/cepalstat_exp_imp_grandes_cat.csv", 
                                            ";", escape_double = FALSE, 
                                            trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_exp_imp_pro_ppal_part_1_of_2 <- read_delim("./data/cepalstat_exp_imp_pro_ppal_part_1_of_2.csv", 
                                                     ";", escape_double = FALSE, 
                                                     trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_exp_imp_pro_ppal_part_2_of_2 <- read_delim("./data/cepalstat_exp_imp_pro_ppal_part_2_of_2.csv", 
                                                     ";", escape_double = FALSE, 
                                                     trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_exp_imp_totales_mensuales <- read_delim("./data/cepalstat_exp_imp_totales_mensuales.csv", 
                                                  ";", escape_double = FALSE, 
                                                  trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_exp_prim_manuf <- read_delim("./data/cepalstat_exp_prim_manuf.csv", 
                                       ";", escape_double = FALSE, 
                                       trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_indic_vol_precios_imp_exp <- read_delim("./data/cepalstat_indic_vol_precios_imp_exp.csv", 
                                                  ";", escape_double = FALSE, 
                                                  trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))

cepalstat_tipo_de_cambio <- read_delim("./data/cepalstat_tipo_de_cambio.csv", 
                                       ";", escape_double = FALSE, 
                                       trim_ws = TRUE, locale = locale("es", encoding="windows-1252"))




new_cols = ncol(cepalstat_sector_real_dolares_completo) - 1
cepalstat_sector_real_dolares_completo = select(cepalstat_sector_real_dolares_completo, 1:new_cols)

new_cols = ncol(cepalstat_desempleo) - 1
cepalstat_desempleo = select(cepalstat_desempleo, 1:new_cols)

new_cols = ncol(cepalstat_empleo) - 1
cepalstat_empleo = select(cepalstat_empleo, 1:new_cols)

new_cols = ncol(cepalstat_remuneraciones) - 1
cepalstat_remuneraciones = select(cepalstat_remuneraciones, 1:new_cols)

new_cols = ncol(cepalstat_sector_financiero_monetario) - 1
cepalstat_sector_financiero_monetario = select(cepalstat_sector_financiero_monetario, 1:new_cols)

new_cols = ncol(cepalstat_sector_publico2) - 1
cepalstat_sector_publico2 = select(cepalstat_sector_publico2, 1:new_cols)

new_cols = ncol(cepalstat_BP_anual) - 1
cepalstat_BP_anual = select(cepalstat_BP_anual, 1:new_cols)

new_cols = ncol(cepalstat_BP_trimestral) - 1
cepalstat_BP_trimestral = select(cepalstat_BP_trimestral, 1:new_cols)

new_cols = ncol(cepalstat_indicadores_derivados_de_la_BP) - 1
cepalstat_indicadores_derivados_de_la_BP = select(cepalstat_indicadores_derivados_de_la_BP, 1:new_cols)

new_cols = ncol(cepalstat_deuda_externa) - 1
cepalstat_deuda_externa = select(cepalstat_deuda_externa, 1:new_cols)

new_cols = ncol(cepalstat_comercio_intrarregional) - 1
cepalstat_comercio_intrarregional = select(cepalstat_comercio_intrarregional, 1:new_cols)

new_cols = ncol(cepalstat_exp_imp_grandes_cat) - 1
cepalstat_exp_imp_grandes_cat = select(cepalstat_exp_imp_grandes_cat, 1:new_cols)

new_cols = ncol(cepalstat_exp_imp_pro_ppal_part_1_of_2) - 1
cepalstat_exp_imp_pro_ppal_part_1_of_2 = select(cepalstat_exp_imp_pro_ppal_part_1_of_2, 1:new_cols)

new_cols = ncol(cepalstat_exp_imp_pro_ppal_part_2_of_2) - 1
cepalstat_exp_imp_pro_ppal_part_2_of_2 = select(cepalstat_exp_imp_pro_ppal_part_2_of_2, 1:new_cols)

new_cols = ncol(cepalstat_exp_imp_servicios) - 1
cepalstat_exp_imp_servicios = select(cepalstat_exp_imp_servicios, 1:new_cols)

new_cols = ncol(cepalstat_exp_imp_totales_mensuales) - 1
cepalstat_exp_imp_totales_mensuales = select(cepalstat_exp_imp_totales_mensuales, 1:new_cols)

new_cols = ncol(cepalstat_exp_prim_manuf) - 1
cepalstat_exp_prim_manuf = select(cepalstat_exp_prim_manuf, 1:new_cols)

new_cols = ncol(cepalstat_indic_vol_precios_imp_exp) - 1
cepalstat_indic_vol_precios_imp_exp = select(cepalstat_indic_vol_precios_imp_exp, 1:new_cols)

new_cols = ncol(cepalstat_tipo_de_cambio) - 1
cepalstat_tipo_de_cambio = select(cepalstat_tipo_de_cambio, 1:new_cols)



save(cepalstat_sector_real_dolares_completo, file="./output/cepalstat_sector_real_dolares_anual")

save(cepalstat_desempleo , file = "./output/cepalstat_desempleo")

save(cepalstat_empleo , file = "./output/cepalstat_empleo")

save(cepalstat_remuneraciones , file = "./output/cepalstat_remuneraciones")

save(cepalstat_sector_financiero_monetario , file = "./output/cepalstat_sector_financiero_monetario")

save(cepalstat_sector_publico2 , file = "./output/cepalstat_sector_publico2")

save(cepalstat_BP_anual , file = "./output/cepalstat_BP_anual")

save(cepalstat_BP_trimestral, file = "./output/cepalstat_BP_trimestral")

save(cepalstat_indicadores_derivados_de_la_BP, file = "./output/cepalstat_indicadores_derivados_de_la_BP")

save(cepalstat_deuda_externa, file = "./output/cepalstat_deuda_externa")

save(cepalstat_comercio_intrarregional, file = "./output/cepalstat_comercio_intrarregional")

save(cepalstat_exp_imp_grandes_cat, file = "./output/cepalstat_exp_imp_grandes_cat")

save(cepalstat_exp_imp_pro_ppal_part_1_of_2, file = "./output/cepalstat_exp_imp_pro_ppal_part_1_of_2")

save(cepalstat_exp_imp_pro_ppal_part_2_of_2, file = "./output/cepalstat_exp_imp_pro_ppal_part_2_of_2")

save(cepalstat_exp_imp_servicios, file = "./output/cepalstat_exp_imp_servicios")

save(cepalstat_exp_imp_totales_mensuales, file = "./output/cepalstat_exp_imp_totales_mensuales")

save(cepalstat_exp_prim_manuf, file = "./output/cepalstat_exp_prim_manuf")

save(cepalstat_indic_vol_precios_imp_exp, file = "./output/cepalstat_indic_vol_precios_imp_exp")

save(cepalstat_tipo_de_cambio, file = "./output/cepalstat_tipo_de_cambio")

