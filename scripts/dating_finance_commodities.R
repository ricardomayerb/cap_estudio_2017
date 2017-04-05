library(xts)
library(lubridate)

# Bear Stearns informs investors that it is suspending redemptions from its
# High-Grade Structured Credit Strategies Enhanced Leverage Fund.
fincri_fine  <- as.Date("2007-06-07", format = "%Y-%m-%d")
fincri_fine_lub <- ymd("2007-06-07")


nber_peaks <-  c("1990-07-01", "2001-03-01", "2007-12-01") 
nber_peaks <-  ymd(nber_peaks)
nber_troughs <-  c("1991-03-01", "2001-11-01", "2009-06-01")
nber_troughs <-  ymd(nber_troughs)
nber_dates_df <- data.frame(nb_peak = nber_peaks, nb_trough = nber_troughs)

fin_peaks <-  c("2007-06-07")
fin_peaks <-  ymd(fin_peaks)
fin_troughs <-  c("2009-07-07")
fin_troughs <- ymd(fin_troughs)

fin_dates_df <-data.frame(fn_peak = fin_peaks, fn_trough = fin_troughs)

save(nber_dates_df, fin_dates_df, file = "./produced_data/peak_trough_dates" )



# trough_comm_m_1 <-  as.yearmon(1996 + 11/12)
# peak_comm_m_1 <-  as.yearmon(2008 + 5/12)
# trough_comm_m_2 <-  as.yearmon(2009 + 1/12)
# peak_comm_m_2 <-  as.yearmon(2014 + 4/12)
# 
# trough_comm_q_1 <-  as.yearqtr(trough_comm_m_1)
# peak_comm_q_1 <-  as.yearqtr(peak_comm_m_1)
# trough_comm_q_2 <-  as.yearqtr(trough_comm_m_2)
# peak_comm_q_2 <-  as.yearqtr(peak_comm_m_2)
# 
# peak_metals_m_2 <-  as.yearmon(2011 + 3/12)
# peak_agri_m_2 <-  as.yearmon(2011 + 3/12)
# 
# commo_date_2 <- as.Date("2014", format = "%Y")
# 
# 
# 
# fincri_m <- as.yearmon(2007 + 4/12) # "May 2007"
# pre_fincri_m <- as.yearmon(2007 + 5/12)# "June 2007"
# 
# start_fincri_q <- as.yearqtr(2007 + 2/4) # "2007 Q3"
# end_fincri_q <- as.yearqtr(2009 + 1/4) # "2009 Q2"
