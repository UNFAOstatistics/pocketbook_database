###########################################################################
## Title: This script compute the FAO regional offices aggregates
## Created: 01-09-2013
## Modified: 24-09-2015 by Markus
###########################################################################

# To allow parallel processing 
session_path <- readLines(con = "./input_data/session_path.txt")
load(paste0(session_path,"/pre_agg_image.RData"))
lapply(pkg_list, library, character.only = TRUE)

## NOTE (FILIPPO): For the same geographical region we can have several
## aggregates that are not comparable for several reasons.
## Let's take for example Africa. The regional office for Africa does not
## include North Africa while M49 Africa does. Even then, M49 Africa includes
## territories like Reunion that are not in the Regional office for Africa
## (shold we compute a different country level aggregation for FAO regional
## offices? is Runion just discarded? big issue...). Moreover, M49 Africa
## aggregate is computed following the 65% aggregation rule at the world level,
## while the Regional Office for Africa is aggregated following the rule of
## 65% at African level.

# Africa ------------------------------------------------------------------

## Relation data frame
RAFsubReg = FAOcountryProfile[, c("FAOST_CODE", "FAO_RAF_SUB_REG")]
## Aggregation
RAFsubReg.df =
  Aggregation(data = country.df,
              relationDF = RAFsubReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RAFsubReg.df)[which(colnames(RAFsubReg.df) == "FAO_RAF_SUB_REG")] = "FAOST_CODE"
## Specify the area
RAFsubReg.df$Area = "RAFsubReg"
## Add country names
RAFsubRegName = data.frame(FAOST_CODE = c("RAFCentralAfrica",
                                          "RAFEastAfrica",
                                          "RAFNorthAfrica",
                                          "RAFSouthernAfrica",
                                          "RAFWestAfrica"),
                           FAO_TABLE_NAME = c("Central Africa",
                                              "Eastern Africa",
                                              "Northern Africa",
                                              "Southern Africa",
                                              "Western Africa"),
                           stringsAsFactors = FALSE)
RAFsubReg.df = merge(RAFsubReg.df, RAFsubRegName, all.x = TRUE, by = "FAOST_CODE")

## Relation data frame
RAFregion = FAOcountryProfile[, c("FAOST_CODE", "FAO_RAF_REG")]
## Aggregation
RAFregion.df =
  Aggregation(data = country.df,
              relationDF = RAFregion,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RAFregion.df)[which(colnames(RAFregion.df) == "FAO_RAF_REG")] = "FAOST_CODE"
## Specify the area
RAFregion.df$Area = "RAFregion"
## Add country names
RAFregion.df[, "FAO_TABLE_NAME"] = "Regional Office for Africa"

# Asia and Pacific --------------------------------------------------------

## Relation data frame
RAPdev = FAOcountryProfile[, c("FAOST_CODE", "FAO_RAP_DVDDVG_REG")]
## Aggregation
RAPdev.df =
  Aggregation(data = country.df,
              relationDF = RAPdev,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RAPdev.df)[which(colnames(RAPdev.df) == "FAO_RAP_DVDDVG_REG")] = "FAOST_CODE"
## Specify the area
RAPdev.df$Area = "RAPdev"
## Add country names
RAPdevName = data.frame(FAOST_CODE = c("RAPDeveloped", "RAPDeveloping"),
                        FAO_TABLE_NAME = c("Developed countries",
                                           "Developing countries"),
                        stringsAsFactors = FALSE)
RAPdev.df = merge(RAPdev.df, RAPdevName, all.x = TRUE, by = "FAOST_CODE")

## Relation data frame
RAPsubReg = FAOcountryProfile[, c("FAOST_CODE", "FAO_RAP_SUB_REG")]
## Aggregation
RAPsubReg.df =
  Aggregation(data = country.df,
              relationDF = RAPsubReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RAPsubReg.df)[which(colnames(RAPsubReg.df) == "FAO_RAP_SUB_REG")] = "FAOST_CODE"
## Specify the area
RAPsubReg.df$Area = "RAPsubReg"
## Add country names
# RAPsubRegName = data.frame(FAOST_CODE = c("RAPCentralAsia", "RAPDevelopedCountries",
#                                           "RAPEastAsia", "RAPPacificIslands",
#                                           "RAPSoutheastAsia",
#                                           "RAPSouthSouthwestAsia"),
#                            FAO_TABLE_NAME = c("Central Asia",
#                                     "Developed countries",
#                                     "East Asia",
#                                     "Pacific Islands",
#                                     "Southeast Asia",
#                                     "South and Southwest Asia"),
#                            stringsAsFactors = FALSE)
# RAPsubReg.df = merge(RAPsubReg.df, RAPsubRegName, all.x = TRUE, by = "FAOST_CODE")

RAPsubRegName = data.frame(FAOST_CODE = c(
                                          # "RAPAustraliaNewZealand",
                                          "RAPCentralAsia",
                                          "RAPEastAsia",
                                          # "RAPFrance",
                                          # "RAPMelanesia",
                                          # "RAPMicronesia",
                                          # "RAPPolynesia",
                                          # "RAPRussianFederation",
                                          "RAPSoutheastAsia",
                                          "RAPSouthernAsia",
                                          "RAPOceania"
                                          # "RAPUnitedStates",
                                          # "RAPWesternAsia"
                                          ),
                           FAO_TABLE_NAME = c(
                                              # "Australia and New Zealand",
                                               "Central Asia",
                                               "East Asia",
                                               # "France",
                                               # "Melanesia",
                                               # "Micronesia",
                                               # "Polynesia",
                                               # "Russian Federation",
                                               "Southeast Asia",
                                               "Southern Asia",
                                               "Oceania"
                                               # "United States",
                                               # "Western Asia"
                                               ),
                           stringsAsFactors = FALSE)
RAPsubReg.df = merge(RAPsubReg.df, RAPsubRegName, all.x = TRUE, by = "FAOST_CODE")





## Relation data frame
RAPregion = FAOcountryProfile[, c("FAOST_CODE", "FAO_RAP_REG")]
## Aggregation
RAPregion.df =
  Aggregation(data = country.df,
              relationDF = RAPregion,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RAPregion.df)[which(colnames(RAPregion.df) == "FAO_RAP_REG")] = "FAOST_CODE"
## Specify the area
RAPregion.df$Area = "RAPregion"
## Add country names
RAPregion.df[, "FAO_TABLE_NAME"] = "Regional Office for Asia and the Pacific"


# # New regiona for a ALL FAO RAP MEMBERS ie. WAP - no longer needed 20160502
# ## Relation data frame
# WAPregion = FAOcountryProfile[, c("FAOST_CODE", "FAO_WAP_REG")]
# ## Aggregation
# WAPregion.df =
#   Aggregation(data = country.df,
#               relationDF = WAPregion,
#               aggVar = con.df[, "STS_ID"],
#               aggMethod = con.df[, "AGGREGATION"],
#               weightVar = con.df[, "STS_ID_WEIGHT"],
#               thresholdProp = con.df[, "THRESHOLD_PROP"],
#               #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               #               applyRules = TRUE,
#               keepUnspecified = FALSE)
# colnames(WAPregion.df)[which(colnames(WAPregion.df) == "FAO_WAP_REG")] = "FAOST_CODE"
# ## Specify the area
# WAPregion.df$Area = "WAPregion"
# ## Add country names
# WAPregion.df[, "FAO_TABLE_NAME"] = "FAO RAP member countries"




# Europe and Central Asia -------------------------------------------------

## Relation data frame
REUsubReg = FAOcountryProfile[, c("FAOST_CODE", "FAO_REU_SUB_REG")]
## Aggregation
REUsubReg.df =
  Aggregation(data = country.df,
              relationDF = REUsubReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(REUsubReg.df)[which(colnames(REUsubReg.df) == "FAO_REU_SUB_REG")] = "FAOST_CODE"
## Specify the area
REUsubReg.df$Area = "REUsubReg"
## Add country names
REUsubRegName = data.frame(FAOST_CODE = c("REUCISeurope",
                                          "REUCaucAndTurkey",
                                          "REUCentralAsia",
                                          "REUEUCentralandEastern",
                                          # "REUIsrael",
                                          "REUAndorraIsraelMonacoandSanMarino",
                                          "REUEUOtherAndEFTA",
                                          "REUSouthEasternEurope"),
                           FAO_TABLE_NAME = c("CIS Europe",
                                              "Caucasus and Turkey",
                                              "Central Asia",
                                              "EU Central Eastern",
                                              # "Israel",
                                              "REU_Andorra, Israel, Monaco and San Marino",
                                              "EU Other and EFTA",
                                              "South Eastern Europe"),
                           stringsAsFactors = FALSE)
REUsubReg.df = merge(REUsubReg.df, REUsubRegName, all.x = TRUE, by = "FAOST_CODE")

## Relation data frame
REUregion = FAOcountryProfile[, c("FAOST_CODE", "FAO_REU_REG")]
## Aggregation
REUregion.df =
  Aggregation(data = country.df,
              relationDF = REUregion,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(REUregion.df)[which(colnames(REUregion.df) == "FAO_REU_REG")] = "FAOST_CODE"
## Specify the area
REUregion.df$Area = "REUregion"
## Add country names
REUregion.df[, "FAO_TABLE_NAME"] = "Regional Office for Europe and Central Asia"

# Commented by Markus on 20160208 to speed up the process
# # Latin America and the Caribbean -----------------------------------------
# 
# ## Relation data frame
# LACsubReg = FAOcountryProfile[, c("FAOST_CODE", "FAO_LAC_SUB_REG")]
# ## Aggregation
# LACsubReg.df =
#   Aggregation(data = country.df,
#               relationDF = LACsubReg,
#               aggVar = con.df[, "STS_ID"],
#               
#               aggMethod = con.df[, "AGGREGATION"],
#               weightVar = con.df[, "STS_ID_WEIGHT"],
#               thresholdProp = con.df[, "THRESHOLD_PROP"],
#               #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               #               applyRules = TRUE,
#               keepUnspecified = FALSE)
# colnames(LACsubReg.df)[which(colnames(LACsubReg.df) == "FAO_LAC_SUB_REG")] = "FAOST_CODE"
# ## Specify the area
# LACsubReg.df$Area = "LACsubReg"
# ## Add country names
# LACsubRegName = data.frame(FAOST_CODE = c("LACCaribbean", "LACCentralAmerica",
#                                           "LACNorthAmerica", "LACSouthAmerica"),
#                            FAO_TABLE_NAME = c("Caribbean", "Central America",
#                                               "North America", "South America"),
#                            stringsAsFactors = FALSE)
# LACsubReg.df = merge(LACsubReg.df, LACsubRegName, all.x = TRUE, by = "FAOST_CODE")
# 
# ## Relation data frame
# LACregion = FAOcountryProfile[, c("FAOST_CODE", "FAO_LAC_REG")]
# ## NOTE (FILIPPO): I'm removing LACNorthAmerica because it is not part of the
# ## Regional Office for Latin America and the Caribbean and (although they
# ## want to show it) I want to keep the two aggregates separate.
# NAcountries =
#   na.omit(FAOcountryProfile[FAOcountryProfile[, "FAO_LAC_SUB_REG"] == "LACNorthAmerica", "FAOST_CODE"])
# LACregion[LACregion[, "FAOST_CODE"] %in% NAcountries, "FAO_LAC_REG"] = NA
# ## Aggregation
# LACregion.df =
#   Aggregation(data = country.df,
#               relationDF = LACregion,
#               aggVar = con.df[, "STS_ID"],
#               aggMethod = con.df[, "AGGREGATION"],
#               weightVar = con.df[, "STS_ID_WEIGHT"],
#               thresholdProp = con.df[, "THRESHOLD_PROP"],
#               #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               #               applyRules = TRUE,
#               keepUnspecified = FALSE)
# colnames(LACregion.df)[which(colnames(LACregion.df) == "FAO_LAC_REG")] = "FAOST_CODE"
# ## Specify the area
# LACregion.df$Area = "LACregion"
# ## Add country names
# LACregion.df[, "FAO_TABLE_NAME"] = "Regional Office for Latin America and the Caribbean"

# Near East ---------------------------------------------------------------

## Relation data frame
RNEsubReg = FAOcountryProfile[, c("FAOST_CODE", "FAO_RNE_SUB_REG")]
## Aggregation
RNEsubReg.df =
  Aggregation(data = country.df,
              relationDF = RNEsubReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RNEsubReg.df)[which(colnames(RNEsubReg.df) == "FAO_RNE_SUB_REG")] = "FAOST_CODE"
## Specify the area
RNEsubReg.df$Area = "RNEsubReg"
## Add country names
RNEsubRegName = data.frame(FAOST_CODE = c("RNEgccsy", "RNEna",
                                          "RNEome"),
                           FAO_TABLE_NAME = c("Gulf Cooperation Council States and Yemen",
                                              "North Africa",
                                              "Other Near East countries"),
                           stringsAsFactors = FALSE)
RNEsubReg.df = merge(RNEsubReg.df, RNEsubRegName, all.x = TRUE, by = "FAOST_CODE")

## Relation data frame
RNEregion = FAOcountryProfile[, c("FAOST_CODE", "FAO_RNE_REG")]
## Aggregation
RNEregion.df =
  Aggregation(data = country.df,
              relationDF = RNEregion,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
              #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
              #               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(RNEregion.df)[which(colnames(RNEregion.df) == "FAO_RNE_REG")] = "FAOST_CODE"
## Specify the area
RNEregion.df$Area = "RNEregion"
## Add country names
RNEregion.df[, "FAO_TABLE_NAME"] = "Regional Office for the Near East"

# FAOregions --------------------------------------------------------------

FAOregions.df = rbind(RAFsubReg.df, RAFregion.df,
                      RAPsubReg.df, RAPregion.df, RAPdev.df,
                      # WAPregion.df,
                      REUsubReg.df, REUregion.df,
                      #LACsubReg.df, LACregion.df,
                      RNEsubReg.df, RNEregion.df)
# rm(list = c("RAFsubReg", "RAFsubReg.df", "RAFsubRegName", "RAFregion", "RAFregion.df",
#             "RAPsubReg", "RAPsubReg.df", "RAPsubRegName", "RAPregion", "RAPregion.df",
#             
#             "RAPdev", "RAPdev.df", "RAPdevName",
#             "REUsubReg", "REUsubReg.df", "REUsubRegName", "REUregion", "REUregion.df",
#             # "LACsubReg", "LACsubReg.df", "LACsubRegName", "LACregion", "LACregion.df",
#             "RNEsubReg", "RNEsubReg.df", "RNEsubRegName", "RNEregion", "RNEregion.df"))


saveRDS(FAOregions.df, file = paste0(session_path,"/FAOregions.df.RDS"))