library(IMFData)
library(tidyverse)

load("cepal_33_countries")

availableDB <- DataflowMethod()

IFS.available.codes <- DataStructureMethod("IFS")

maybeDebt = CodeSearch(IFS.available.codes, "CL_INDICATOR_IFS", "Debt")
maybedebt = CodeSearch(IFS.available.codes, "CL_INDICATOR_IFS", "debt")
