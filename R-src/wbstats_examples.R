library(wbstats)
library(tidyverse)

str(wb_cachelist, max.level = 1)

new_cache = wbcache()


unemploy_vars <- wbsearch(pattern = "unemployment")

debt_vars <- wbsearch(pattern = "external debt", cache=new_cache)

crude_vars <- wbsearch(pattern = "crude")

blmbrg_vars <- wbsearch(pattern = "Bloomberg", fields = "sourceOrg")
head(blmbrg_vars)

pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2000, enddate = 2002)
head(pop_data)


# regionID: LCN, region: Latin America & Caribbean
pop_data_alc <- wb(indicator = "SP.POP.TOTL", country="LCN", startdate = 2000, enddate = 2002)
head(pop_data_alc)

# Population, total
# country values: iso3c, iso2c, regionID, adminID, incomeID

lac_wb = new_cache$countries %>% filter(regionID=="LCN") %>% select(iso2c, iso3c, country)

# in WB's LC but not in Cepal's LACs coutries:
# c("ABW", "CUW", "CYM", "MAF", "PRI", "SXM", "TCA", "VGB", "VIR")

not_cepal_member = c("ABW", "CUW", "CYM", "MAF", "PRI", "SXM", "TCA", "VGB", "VIR")
cepal_33_countries = lac_wb %>% filter(! iso3c %in% not_cepal_member)

save(cepal_33_countries, file = "cepal_33_countries")

pop_gdp_data <- wb(country = cepal_33_countries[["iso3c"]], indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"),
                   startdate = 1971, enddate = 1973)

pop_gdp_data <- arrange(pop_gdp_data, country, date)
head(pop_gdp_data)

# Using mrv (most recent values) instead or start and end
pop_gdp_data_latest = wb(country = cepal_33_countries[["iso3c"]], indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"),
                         mrv=4)
head(pop_gdp_data_latest)

pop_gdp_data <- wb(country = c("US", "NO"), indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"),
                   startdate = 1971, enddate = 1973, POSIXct = TRUE)
head(pop_gdp_data)

oil_data <- wb(indicator = "CRUDE_BRENT", mrv = 10, freq = "M", POSIXct = TRUE)

head(oil_data)

oil_data <- wb(indicator = c("CRUDE_DUBAI", "CRUDE_BRENT", "CRUDE_WTI", "CRUDE_PETRO"),
               startdate = "2012M01", enddate = "2014M12", freq = "M", POSIXct = TRUE)

ggplot(oil_data, aes(x = date_ct, y = value, colour = indicator)) + geom_line(size = 1) +
  labs(title = "Crude Oil Price Comparisons", x = "Date", y = "US Dollars")



