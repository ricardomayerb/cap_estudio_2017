library(xts)
library(tidyverse)
library(countrycode)
load("./produced_data/cepal_20_countries")
load("./produced_data/debt_data_JEDH_cepal_33")
load("./produced_data/debt_data_JEDH_cepal_20")

# convert current character debt 1990 Q1, to standard 2016 Q2, 2016 Q1 and 1990-01-01
# convert to 2016 Q2, 2016 Q1, 2015 Q4, 2015 Q3, 2015 Q2 etc.

# debt = debt_dates_cepal_33
debt_data <- debt_data_cepal_33
debt_dates = as.yearqtr(debt_data$date)
debt_data$dateYQ = debt_dates

debt_data <- debt_data %>% mutate(ind_short = indicator) %>% 
  mutate(new = strtrim(indicator, 2)) %>% 
             mutate(ind_short = 
                    recode(ind_short,
                    "01_Cross-border loans from BIS reporting banks" = "01",
                    "02_Cross-border loans from BIS banks to nonbanks" = "02"))

# convert to standard year-month-day
debt_dates = as.Date.yearqtr(debt_dates)
debt_data$date = as.Date(debt_dates, "%Y-%m-%d")

## make tables of all the individual variables that we are going to use with the
## number of observations and end and start date.
indicator_first = debt_data %>%
  group_by(indicator, country) %>%
  summarise(nr_of_obs = n(),
            start = min(dateYQ),
            end = max(dateYQ)) %>%
  arrange(indicator, nr_of_obs)


Variable1_catA <- indicator_first %>% 
                  filter(indicator == 
                           "01_Cross-border loans from BIS reporting banks")

Variable2_catA <- indicator_first %>% 
                  filter(indicator == 
                           "02_Cross-border loans from BIS banks to nonbanks")

Variable11_catE <- indicator_first %>% 
                  filter(indicator == 
                           "11_SDR allocation")

Variable12_catF <- indicator_first %>% 
                  filter(indicator == 
                           "12_Liabilities to BIS banks (cons.), short term")

Variable16_catH <- indicator_first %>% 
                  filter(indicator == 
                           "16_International debt securities, all maturities")

Variable17_catH <- indicator_first %>% 
                  filter(indicator == 
                           "17_International debt securities, nonbanks")

Variable18_catH <- indicator_first %>% 
                  filter(indicator == 
                           "18_International debt securities, short term")

Variable19_catH <- indicator_first %>% 
                  filter(indicator == 
                           "19_Intnl debt securities, nonbanks, short term")

Variable26_catL <- indicator_first %>% 
                  filter(indicator == 
                           "26_Portfolio investment assets")

## now we know the country sample and range lets wrangle the data such that we
## can use it.

debt_data1 <-
  debt_data %>% select(indicator, country, dateYQ, value)

# I would like to rename the indicator names so they are automatically in order,
# ordered by categroy and less long.
debt_data1$indicator[debt_data$indicator == "01_Cross-border loans from BIS reporting banks"] <-
  "A1"

debt_data1$indicator[debt_data$indicator == "02_Cross-border loans from BIS banks to nonbanks"] <-
  "A2"

debt_data1$indicator[debt_data$indicator == "11_SDR allocation"] <-
  "E11"

debt_data1$indicator[debt_data$indicator == "12_Liabilities to BIS banks (cons.), short term"] <-
  "F12"

debt_data1$indicator[debt_data$indicator == "16_International debt securities, all maturities"] <-
  "H16"

debt_data1$indicator[debt_data$indicator == "17_International debt securities, nonbanks"] <-
  "H17"

debt_data1$indicator[debt_data$indicator == "18_International debt securities, short term"] <-
  "H18"

debt_data1$indicator[debt_data$indicator == "19_Intnl debt securities, nonbanks, short term"] <-
  "H19"

debt_data1$indicator[debt_data$indicator == "26_Portfolio investment assets"] <-
  "L26"

distinct(debt_data1, indicator)

#remove the variables that we do not need
remove_var <-
  c(
    "04_Official bilateral loans, aid loans",
    "10_Insured export credit exposures, short term (BU)",
    "09_Insured export credit exposures, Berne Union",
    "20_Paris Club claims (ODA)",
    "21_Paris Club claims (non ODA)",
    "14_Debt securities held by nonresidents",
    "28_Cross-border dep. with BIS banks,  nonbanks",
    "22_Liabilities to BIS banks, locational, total",
    "27_Cross-border deposits with BIS rep. banks",
    "23_Liabilities to BIS banks, consolidated, total",
    "03_Official bilateral loans, total",
    "05_Official bilateral loans, other",
    "06_Multilateral loans, total",
    "08_Multilateral loans, other institutions",
    "25_SDR holdings",
    "15_Debt securities held by nonresidents, total, short term",
    "13_Multilateral loans, IMF, short term",
    "07_Multilateral loans, IMF",
    "24_International reserves (excluding gold)"
  )

debt_data1_cleaned <-
  debt_data1 %>% filter(!indicator %in% remove_var)

distinct(debt_data1_cleaned, indicator)

debt_data1_cleaned1 <-
  debt_data1_cleaned %>% arrange(indicator, country, dateYQ)

# now divide into the categories (there must be an easier way to do this than i did, no?)
Category_A <- slice(debt_data1_cleaned1, 1:3200)
Category_E <- slice(debt_data1_cleaned1, 3201:4720)
Category_F <- slice(debt_data1_cleaned1, 4721:6320)
Category_H <- slice(debt_data1_cleaned1, 6321:12236)
Category_L <- slice(debt_data1_cleaned1, 12237:12731)
# inspected the data in each category and constructed a word report with tables and short analysis
#L26 in category L turned out to be an annual variable. There is only data for each Q4. Hence, I reduce this category (it is the only variable in this category) to only the Q4 values
Category_L <- Category_L %>% filter(!value %in% 0)
# Now make the summary statistics. How do I make R round the numbers to a specified number of decimals?
## For now I expressed all values in billions because I have not scaled them to GDP yet.
## 1) An aggregate summary for each variable, i.e. all countries together ##
CategoryA_Aggregate_Summary = Category_A %>%
  group_by(indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  )
CategoryE_Aggregate_Summary = Category_E %>%
  group_by(indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  )
CategoryF_Aggregate_Summary = Category_F %>%
  group_by(indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  )
CategoryH_Aggregate_Summary = Category_H %>%
  group_by(indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  )
CategoryL_Aggregate_Summary = Category_L %>%
  group_by(indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  )
## 2) A summary for each variable for each individual country ##
CategoryA_Summary = Category_A %>%
  group_by(country, indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  ) %>%
  arrange(indicator) #if choose to order it by indicator but i can adjust it to your preference
CategoryE_Summary = Category_E %>%
  group_by(country, indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  ) %>%
  arrange(indicator)
CategoryF_Summary = Category_F %>%
  group_by(country, indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  ) %>%
  arrange(indicator)
CategoryH_Summary = Category_H %>%
  group_by(country, indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  ) %>%
  arrange(indicator)
CategoryL_Summary = Category_L %>%
  group_by(country, indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE) / 1000000000,
    min_value = min(value) / 1000000000,
    max_value = max(value) / 1000000000,
    sd = sd(value) / 1000000000,
    count = n(),
    start = min(dateYQ),
    end = max(dateYQ)
  ) %>%
  arrange(indicator)
# you asked me to scale them according to current GDP. How do I add a dataset with current GDP to this dataset and how do i merge them?

