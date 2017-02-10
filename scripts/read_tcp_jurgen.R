library(tidyverse)
library(readxl)


TCP_2015 <- read_excel("./raw_data/TCP_2015.xlsx", skip = 1)
save(TCP_2015, file="./produced_data/trab_cuenta_propia_jurgen")



