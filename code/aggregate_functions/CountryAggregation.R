###########################################################################
## Title: This script uses the Aggregation function of the FAOSTAT 
## package in order to collapse the list of countries in the world country
## list chosen by the user.
## Updated: 16/07/2014
## Notes:
###########################################################################

country.df <- 
  Aggregation(data = preAgg.df, aggVar = con.df[,"STS_ID"], 
              thresholdProp = con.df[,"THRESHOLD_PROP"], 
              weightVar = con.df[,"STS_ID_WEIGHT"], 
              relationDF = na.omit(FAOcountryProfile[, c("FAOST_CODE", "M49_FAOST_CODE")]), 
              aggMethod = con.df[,"AGGREGATION"], 
              applyRules = FALSE,
              keepUnspecified = FALSE)
colnames(country.df)[grep("M49_FAOST_CODE", colnames(country.df))] = "FAOST_CODE"
country.df[, "Area"] <- "Territory"