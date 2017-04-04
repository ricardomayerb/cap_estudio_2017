library(xts)
library(lubridate)

# Bear Stearns informs investors that it is suspending redemptions from its
# High-Grade Structured Credit Strategies Enhanced Leverage Fund.
fincri_fine  <- as.Date("2007-06-07", format = "%Y-%m-%d")

fincri_a <- as.Date("2007", format = "%Y")
pre_fincri_a <- as.Date("2006", format = "%Y")

fincri_q <- as.yearqtr(2007 + 2/4) # "2007 Q3"
pre_fincri_q <- as.yearqtr(2007 + 1/4) # "2007 Q2"

fincri_m <- as.yearmon(2007 + 4/12) # "May 2007"
pre_fincri_m <- as.yearmon(2007 + 5/12)# "June 2007"




