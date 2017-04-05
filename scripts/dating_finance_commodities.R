library(xts)
library(lubridate)

# Bear Stearns informs investors that it is suspending redemptions from its
# High-Grade Structured Credit Strategies Enhanced Leverage Fund.
fincri_fine  <- as.Date("2007-06-07", format = "%Y-%m-%d")
fincri_fine_lub <- ymd("2007-06-07")


pre_fincri_a <- as.Date("2006", format = "%Y")
start_fincri_a <- as.Date("2007", format = "%Y")
end_fincri_a <- as.Date("2009", format = "%Y")

pre_commo_date_a_1 <- as.Date("2007", format = "%Y")
start_commo_date_a_1 <- as.Date("2008", format = "%Y")
end_commo_date_a_1 <- as.Date("2009", format = "%Y")
end_commo_date_a_2 <- as.Date("2014", format = "%Y")


fincri_q <- as.yearqtr(2007 + 2/4) # "2007 Q3"
pre_fincri_q <- as.yearqtr(2007 + 1/4) # "2007 Q2"

fincri_m <- as.yearmon(2007 + 4/12) # "May 2007"
pre_fincri_m <- as.yearmon(2007 + 5/12)# "June 2007"

commo_date_1 <- as.Date("2009", format = "%Y")
pre_commo_date_1 <- as.Date("2009", format = "%Y")

commo_date_2 <- as.Date("2014", format = "%Y")
pre_commo_date_1 <- as.Date("20013", format = "%Y")

dates_cri <- c(pre_fincri_a, start_fincri_a, end_fincri_a,
               pre_commo_date_a_1, start_commo_date_a_1, end_commo_date_a_1,
               end_commo_date_a_2)

