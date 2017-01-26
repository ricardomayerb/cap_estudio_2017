library(rsdmx)

providers <- getSDMXServiceProviders();

providers_df = as.data.frame(providers)

myUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MIG/TOT../OECD?startTime=2000&endTime=2011"
dataset <- readSDMX(myUrl)
stats <- as.data.frame(dataset)

sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "MIG",
                 key = list("TOT", NULL, NULL), start = 2010, end = 2011)
df <- as.data.frame(sdmx)
head(df)

