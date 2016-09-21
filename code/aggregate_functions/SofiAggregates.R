###########################################################################
## Title: This script compute the SOFI aggregates
## Created: 01-09-2013
## Modified: 22-12-2013
###########################################################################

# To allow parallel processing 
session_path <- readLines(con = "./input_data/session_path.txt")
load(paste0(session_path,"/pre_agg_image.RData"))
lapply(pkg_list, library, character.only = TRUE)

# Sub-region  -------------------------------------------------------------

## Relation data frame
SOFIsubReg = FAOcountryProfile[, c("FAOST_CODE", "SOFI_SUB_REG_CODE")]
## Aggregation
SOFIsubReg.df =  
  Aggregation(data = country.df,
              relationDF = SOFIsubReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(SOFIsubReg.df)[grep("SOFI_SUB_REG_CODE", colnames(SOFIsubReg.df))] = "FAOST_CODE"
## Specify the area
SOFIsubReg.df$Area = "SOFISubRegion"
## Add country names
SOFIsubReg.df = 
  merge(SOFIsubReg.df, 
        na.omit(unique(FAOcountryProfile[, c("SOFI_SUB_REG_CODE", "SOFI_SUB_REG")])),
        by.x = "FAOST_CODE", by.y = "SOFI_SUB_REG_CODE", all.x = TRUE)
colnames(SOFIsubReg.df)[grep("SOFI_SUB_REG", colnames(SOFIsubReg.df))] = "FAO_TABLE_NAME"

# Other sub-regions -------------------------------------------------------

## Relation data frame
SOFIotherReg = FAOcountryProfile[, c("FAOST_CODE", "SOFI_OTHER_REG_CODE")]
## Aggregation
SOFIotherReg.df =  
  Aggregation(data = country.df,
              relationDF = SOFIotherReg,
              aggVar = con.df[!is.na(con.df["AGGREGATION"]), "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(SOFIotherReg.df)[grep("SOFI_OTHER_REG_CODE", colnames(SOFIotherReg.df))] = "FAOST_CODE"
## Specify the area
SOFIotherReg.df$Area = "SOFIOtherRegion"
## Add country names
SOFIotherReg.df = 
  merge(SOFIotherReg.df, 
        na.omit(unique(FAOcountryProfile[, c("SOFI_OTHER_REG_CODE", "SOFI_OTHER_REG")])),
        by.x = "FAOST_CODE", by.y = "SOFI_OTHER_REG_CODE", all.x = TRUE)
colnames(SOFIotherReg.df)[grep("SOFI_OTHER_REG", colnames(SOFIotherReg.df))] = "FAO_TABLE_NAME"

# Macro regions -----------------------------------------------------------

## Relation data frame
SOFImacroReg = FAOcountryProfile[, c("FAOST_CODE", "SOFI_MACRO_REG_CODE")]
## Aggregation
SOFImacroReg.df =  
  Aggregation(data = country.df,
              relationDF = SOFImacroReg,
              aggVar = con.df[!is.na(con.df["AGGREGATION"]), "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(SOFImacroReg.df)[grep("SOFI_MACRO_REG_CODE", colnames(SOFImacroReg.df))] = "FAOST_CODE"
## Specify the area
SOFImacroReg.df$Area = "SOFIMacroRegion"
## Add country names
SOFImacroReg.df = 
  merge(SOFImacroReg.df, 
        na.omit(unique(FAOcountryProfile[, c("SOFI_MACRO_REG_CODE", "SOFI_MACRO_REG")])),
        by.x = "FAOST_CODE", by.y = "SOFI_MACRO_REG_CODE", all.x = TRUE)
colnames(SOFImacroReg.df)[grep("SOFI_MACRO_REG", colnames(SOFImacroReg.df))] = "FAO_TABLE_NAME"

# Developed / developing --------------------------------------------------

## Relation data frame
SOFIdevReg = FAOcountryProfile[, c("FAOST_CODE", "SOFI_DVDDVG_REG_CODE")]
## Aggregation
SOFIdevReg.df =  
  Aggregation(data = country.df,
              relationDF = SOFIdevReg,
              aggVar = con.df[!is.na(con.df["AGGREGATION"]), "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(SOFIdevReg.df)[grep("SOFI_DVDDVG_REG_CODE", colnames(SOFIdevReg.df))] = "FAOST_CODE"
## Specify the area
SOFIdevReg.df$Area = "SOFIDevelopmentRegion"
## Add country names
SOFIdevReg.df = 
  merge(SOFIdevReg.df, 
        na.omit(unique(FAOcountryProfile[, c("SOFI_DVDDVG_REG_CODE", "SOFI_DVDDVG_REG")])),
        by.x = "FAOST_CODE", by.y = "SOFI_DVDDVG_REG_CODE", all.x = TRUE)
colnames(SOFIdevReg.df)[grep("SOFI_DVDDVG_REG", colnames(SOFIdevReg.df))] = "FAO_TABLE_NAME"

# World -------------------------------------------------------------------

## Relation data frame
SOFIworld = FAOcountryProfile[, c("FAOST_CODE", "SOFI_WORLD_REG_CODE")]
## Aggregation
SOFIworld.df =  
  Aggregation(data = country.df,
              relationDF = SOFIworld,
              aggVar = con.df[!is.na(con.df["AGGREGATION"]), "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(SOFIworld.df)[grep("SOFI_WORLD_REG_CODE", colnames(SOFIworld.df))] = "FAOST_CODE"
## Specify the area
SOFIworld.df$Area = "SOFIWorld"
## Add country names
SOFIworld.df = 
  merge(SOFIworld.df, 
        na.omit(unique(FAOcountryProfile[, c("SOFI_WORLD_REG_CODE", "SOFI_WORLD_REG")])),
        by.x = "FAOST_CODE", by.y = "SOFI_WORLD_REG_CODE", all.x = TRUE)
colnames(SOFIworld.df)[grep("SOFI_WORLD_REG", colnames(SOFIworld.df))] = "FAO_TABLE_NAME"

# R bind ------------------------------------------------------------------

sofiAggs.df = rbind(SOFIsubReg.df, SOFIotherReg.df, 
                    SOFImacroReg.df, SOFIdevReg.df, SOFIworld.df)
rm(list = c("SOFIdevReg", "SOFIdevReg.df", "SOFImacroReg", "SOFImacroReg.df",
             "SOFIotherReg", "SOFIotherReg.df", "SOFIsubReg", "SOFIsubReg.df",
             "SOFIworld", "SOFIworld.df"))


saveRDS(sofiAggs.df, file = paste0(session_path,"/sofiAggs.df.RDS"))
