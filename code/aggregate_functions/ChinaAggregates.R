###########################################################################
## Title: This script computes the China 357 and China 351 aggregates
## Updated: 16/07/2014
## Notes: China (China mainland + Taiwan) (faostat country code = 357) and 
## China (China mainland + Taiwan + Macao + Hong Kong) (faostat country 
## code = 351) have to be treated as special aggregates. In theory, data
## in a dataset whould be mutually exclusive. This means for example that 
## ex USSR and countries that were born after USSR should not overlap in 
## terms of years and values. China is a special case because different 
## organizations use different aggregation levels to indicate China. For
## some organizations China corresponds to China mainland, for some others
## China corresponds to China mainland + Taiwan, or even China + Taiwan +
## Macao + Hong Kong. In the faostat country coding system, the first China
## has faostat code = 41, the second one = to 357, the third one = 351. The 
## purpose of this script is to compute the China 357 and China 351 aggregates
## avoiding double counting and keeping all the low levels entities 
## (China mainland, Taiwan, Macao, Hong Kong).
###########################################################################


# Remove duplicates for China 357 -----------------------------------------

## We do the computations in a subset of the main dataset because we do not
## want to loose any information. As a matter of fact, the "overlap" 
## function would set to NA the overlapping countries, but we want to keep
## China 41 and Taiwan in the main dataset.
china357.df <- country.df[country.df[, "FAOST_CODE"] %in% c(357,41,214),]
## This is a trick. We use the overlap function to spot duplicates for China.
## In case of duplicates, if we select take = "takeOld" we prioritize 
## the already aggregated China 357, while if we select take = "takeNew" we
## prioritize the aggregate China 357 that will be computed in the next
## step. The option take = "complete" prioritize the aggregate China 357
## computed by us but only if both China 41 and Taiwan are available.
china357.df <- 
  overlap(old = 357, new = c(41, 214), data = china357.df, 
          var = colnames(china357.df)[-grep("FAOST_CODE|Year|Area|FAO_TABLE_NAME", colnames(china357.df))],
          take = "complete")

# Aggregate china 357 -----------------------------------------------------

## Relation data frame
china357 <- 
  data.frame(FAOST_CODE = c(41,214,357), M49_FAOST_CODE = rep(357, times = 3))
## Aggregation
china357.df <- 
  Aggregation(data = china357.df,
              relationDF = china357,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              applyRules = FALSE,
              keepUnspecified = FALSE)
colnames(china357.df)[grep("M49_FAOST_CODE", colnames(china357.df))] <- "FAOST_CODE"
china357.df[, "Area"] <- "Territory"
# countryName <- 
#   subset(na.omit(unique(FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")])))
# china357.df <- merge(china357.df, countryName, all.x = TRUE, by = "FAOST_CODE")
## Replace the values in the database
country.df <- country.df[country.df[,"FAOST_CODE"] != 357,]
country.df <- rbind(country.df, china357.df)
rm(list = c("china357.df", "china357"))

# Remove duplicates for China 351 -----------------------------------------

## Given the previous steps, we do not need to work with China 41 and 
## Taiwan anymore. We just need China 357.
china351.df <- 
  country.df[country.df[, "FAOST_CODE"] %in% c(351,357,96,128),]
china351.df <- 
  overlap(old = 351, new = c(357,96,128), data = china351.df, 
          var = colnames(china351.df)[-grep("FAOST_CODE|Year|Area|FAO_TABLE_NAME", colnames(china351.df))],
          take = "complete")

# Aggregate china 351 -----------------------------------------------------

## Relation data frame
china351 <- 
  data.frame(FAOST_CODE = c(351, 357,96,128), M49_FAOST_CODE = rep(351, times = 4))
## Aggregation
china351.df <- 
  Aggregation(data = china351.df,
              relationDF = china351,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              applyRules = FALSE,
              keepUnspecified = FALSE)
colnames(china351.df)[grep("M49_FAOST_CODE", colnames(china351.df))] <- "FAOST_CODE"
china351.df[, "Area"] <- "Territory"
# countryName <- 
#   subset(na.omit(unique(FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")])))
# china351.df <- merge(china351.df, countryName, all.x = TRUE, by = "FAOST_CODE")
## Replace the values in the database
country.df <- country.df[country.df[,"FAOST_CODE"] != 351,]
country.df <- rbind(country.df, china351.df)
# rm(list = c("china351.df", "china351"))

