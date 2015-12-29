###########################################################################
## RWBclimate R package
## 21-11-2013
###########################################################################

require(devtools)

install_github("rWBclimate", "ropensci")
require(rWBclimate)
help(package = "rWBclimate")

uncodes = na.omit(unique(FAOcountryProfile[, "UN_CODE"]))

tmp = get_model_temp(locator = uncodes, type = "annualavg", 
                     start = 1990, end = 2050)