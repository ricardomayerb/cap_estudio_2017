library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))


gs_gap() %>% 
  gs_copy(to = "Gapminder")
## or, if you don't use pipes
gs_copy(gs_gap(), to = "Gapminder")
