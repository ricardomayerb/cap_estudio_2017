library(xts)
library(tidyverse)
library(countrycode)
library(broom)

load("./produced_data/cepal_20_countries")
load("./produced_data/debt_data_JEDH_cepal_33")
load("./produced_data/debt_data_JEDH_cepal_20")

# convert current character debt 1990 Q1, to standard 2016 Q2, 2016 Q1 and 1990-01-01
# convert to 2016 Q2, 2016 Q1, 2015 Q4, 2015 Q3, 2015 Q2 etc.

# debt = debt_dates_cepal_33
debt_data <- tbl_df(debt_data_cepal_33)
debt_dates = as.yearqtr(debt_data$date)
debt_data$dateYQ = debt_dates


# Categories and indicators:
# Category A: 1 and 2, Category B: 3, 4 and 5  Category C: 6, 7 and 8
# Category D: 9 ad 10, Category E: 11, Category F: 12
# Category G: 13,  Category H: 14, 15
# Category I: 16, 17, 18 and 19, Category J: 20 and 21, Category K: 22 and 23
# Category L: 24 and 25, Category M: 26, Category, N: 27 and 28
ind_to_keep = c(1, 2, 11, 12, 16, 17, 18, 19, 26)

debt_data <- debt_data %>% 
                select(- indicatorID) %>% 
                mutate(ind_short = strtrim(indicator, 2)) 
                
n_ind_short = as.numeric(debt_data$ind_short)

ind_groups <- case_when(
          n_ind_short %in% c(1, 2)           ~ "A",
          n_ind_short %in% c(3, 4, 5)        ~ "B",
          n_ind_short %in% c(6, 7, 8)        ~ "C",
          n_ind_short %in% c(9, 10)          ~ "D",
          n_ind_short %in% c(11)             ~ "E",
          n_ind_short %in% c(12)             ~ "F",
          n_ind_short %in% c(13)             ~ "G",
          n_ind_short %in% c(14, 15)         ~ "H",
          n_ind_short %in% c(16, 17, 18, 19) ~ "I",
          n_ind_short %in% c(20, 21)         ~ "J",
          n_ind_short %in% c(22, 23)         ~ "K",
          n_ind_short %in% c(24, 25)         ~ "L",
          n_ind_short %in% c(26)             ~ "M",
          n_ind_short %in% c(27, 28)         ~ "N")  

debt_data$ind_groups <- ind_groups

debt_data <- debt_data %>% 
  unite(ind_num_group, ind_groups, ind_short, remove = FALSE ) 

debt_data_kept <- debt_data %>% filter(as.numeric(ind_short) %in% ind_to_keep )


debt_by_cat <- debt_data %>% group_by(ind_groups)

debt_by_cat_n <- debt_data %>% group_by(ind_groups) %>% 
                      nest(.key = subdata)

debt_by_cat_n <- debt_by_cat_n %>% 
  mutate(isum = map(subdata, summary))

print(debt_by_cat_n$isum[[1]] )


debt_by_cat_ind <- debt_data %>% group_by(ind_groups, ind_num_group)

debt_by_cat_ind_n <- debt_data %>% group_by(ind_groups, ind_num_group) %>% 
                      nest(.key = subdata)

debt_by_cat_ind_n <- debt_by_cat_ind_n %>% 
                      mutate(isum = map(subdata, summary)) %>% 
                      mutate(tidysum = isum)
  

print(debt_by_cat_ind_n$isum[[1]] )


# # alternative, droping variables and using recode
# 
# debt_data <- debt_data %>% 
#   select(- indicatorID) %>% 
#   mutate(ind_short = strtrim(indicator, 2)) %>% 
#   filter(as.numeric(ind_short) %in% ind_to_keep ) %>% 
#   mutate(group_ind_num = recode(ind_short, 
#           "01" = "A_01",
#           "02" = "A_02",
#           "11" = "E_11",
#           "12" = "F_12",
#           "16" = "I_16",
#           "17" = "I_17",
#           "18" = "I_18",
#           "19" = "I_19",
#           "26" = "M_26")
#          )


