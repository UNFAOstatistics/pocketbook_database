###########################################################################
## Title: This script compute the FAO/RAF economical aggregates
## Created: 03-02-2013
## Modified: 03-02-2013
###########################################################################


# To allow parallel processing 
session_path <- readLines(con = "./input_data/session_path.txt")
load(paste0(session_path,"/pre_agg_image.RData"))
lapply(pkg_list, library, character.only = TRUE)

# COMESA ------------------------------------------------------------------

## Relation data frame
comesa = FAOcountryProfile[, c("FAOST_CODE", "COMESA_REG")]
## Aggregation
comesa.df =  
  Aggregation(data = country.df,
              relationDF = comesa,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(comesa.df)[grep("COMESA_REG", colnames(comesa.df))] = "FAOST_CODE"
## Specify the area
comesa.df$Area = "COMESA"
## Add country names
comesa.df$FAO_TABLE_NAME = "COMESA"

# ECOWAS ------------------------------------------------------------------

## Relation data frame
ecowas = FAOcountryProfile[, c("FAOST_CODE", "ECOWAS_REG")]
## Aggregation
ecowas.df =  
  Aggregation(data = country.df,
              relationDF = ecowas,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(ecowas.df)[grep("ECOWAS_REG", colnames(ecowas.df))] = "FAOST_CODE"
## Specify the area
ecowas.df$Area = "ECOWAS"
## Add country names
ecowas.df$FAO_TABLE_NAME = "ECOWAS"

# ECCAS -------------------------------------------------------------------

## Relation data frame
eccas = FAOcountryProfile[, c("FAOST_CODE", "ECCAS_REG")]
## Aggregation
eccas.df =  
  Aggregation(data = country.df,
              relationDF = eccas,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(eccas.df)[grep("ECCAS_REG", colnames(eccas.df))] = "FAOST_CODE"
## Specify the area
eccas.df$Area = "ECCAS"
## Add country names
eccas.df$FAO_TABLE_NAME = "ECCAS"

# SADC --------------------------------------------------------------------

## Relation data frame
sadc = FAOcountryProfile[, c("FAOST_CODE", "SADC_REG")]
## Aggregation
sadc.df =  
  Aggregation(data = country.df,
              relationDF = sadc,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(sadc.df)[grep("SADC_REG", colnames(sadc.df))] = "FAOST_CODE"
## Specify the area
sadc.df$Area = "SADC"
## Add country names
sadc.df$FAO_TABLE_NAME = "SADC"

# IGAD --------------------------------------------------------------------

## Relation data frame
igad = FAOcountryProfile[, c("FAOST_CODE", "IGAD_REG")]
## Aggregation
igad.df =  
  Aggregation(data = country.df,
              relationDF = igad,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(igad.df)[grep("IGAD_REG", colnames(igad.df))] = "FAOST_CODE"
## Specify the area
igad.df$Area = "IGAD"
## Add country names
igad.df$FAO_TABLE_NAME = "IGAD"

# UMA ---------------------------------------------------------------------

## Relation data frame
uma = FAOcountryProfile[, c("FAOST_CODE", "UMA_REG")]
## Aggregation
uma.df =  
  Aggregation(data = country.df,
              relationDF = uma,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(uma.df)[grep("UMA_REG", colnames(uma.df))] = "FAOST_CODE"
## Specify the area
uma.df$Area = "UMA"
## Add country names
uma.df$FAO_TABLE_NAME = "UMA"

# CEMAC -------------------------------------------------------------------

## Relation data frame
cemac = FAOcountryProfile[, c("FAOST_CODE", "CEMAC_REG")]
## Aggregation
cemac.df =  
  Aggregation(data = country.df,
              relationDF = cemac,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(cemac.df)[grep("CEMAC_REG", colnames(cemac.df))] = "FAOST_CODE"
## Specify the area
cemac.df$Area = "CEMAC"
## Add country names
cemac.df$FAO_TABLE_NAME = "CEMAC"

# UEMOA -------------------------------------------------------------------

## Relation data frame
uemoa = FAOcountryProfile[, c("FAOST_CODE", "UEMOA_REG")]
## Aggregation
uemoa.df =  
  Aggregation(data = country.df,
              relationDF = uemoa,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(uemoa.df)[grep("UEMOA_REG", colnames(uemoa.df))] = "FAOST_CODE"
## Specify the area
uemoa.df$Area = "UEMOA"
## Add country names
uemoa.df$FAO_TABLE_NAME = "UEMOA"

# CENSAD ------------------------------------------------------------------

## Relation data frame
censad = FAOcountryProfile[, c("FAOST_CODE", "CENSAD_REG")]
## Aggregation
censad.df =  
  Aggregation(data = country.df,
              relationDF = censad,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(censad.df)[grep("CENSAD_REG", colnames(censad.df))] = "FAOST_CODE"
## Specify the area
censad.df$Area = "CENSAD"
## Add country names
censad.df$FAO_TABLE_NAME = "CENSAD"

# Least developed countries -----------------------------------------------

## It is also in the M49aggregates.R script
# ## Relation data frame
# ldc = FAOcountryProfile[, c("FAOST_CODE", "UNSD_LDC_REG")]
# ## Aggregation
# ldc.df =  
#   Aggregation(data = country.df,
#               relationDF = ldc,
#               aggVar = con.df[, "STS_ID"],
#               aggMethod = con.df[, "AGGREGATION"],
#               weightVar = con.df[, "STS_ID_WEIGHT"],
#               thresholdProp = con.df[, "THRESHOLD_PROP"],
# #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
# #               applyRules = TRUE,
#               keepUnspecified = FALSE)
# colnames(ldc.df)[grep("UNSD_LDC_REG", colnames(ldc.df))] = "FAOST_CODE"
# ## Specify the area
# ldc.df$Area = "LDC"
# ## Add country names
# ldc.df$FAO_TABLE_NAME = "Least developed countries"
# ## FAOST_CODE
# ldc.df[, "FAOST_CODE"] = 5801

# Landlocked developing countries -----------------------------------------

# # It is also in the M49aggregates.R script
# ## Relation data frame
# lldc = FAOcountryProfile[, c("FAOST_CODE", "UNSD_LLDC_REG")]
# ## Aggregation
# lldc.df =  
#   Aggregation(data = country.df,
#               relationDF = lldc,
#               aggVar = con.df[, "STS_ID"],
#               aggMethod = con.df[, "AGGREGATION"],
#               weightVar = con.df[, "STS_ID_WEIGHT"],
#               thresholdProp = con.df[, "THRESHOLD_PROP"],
# #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
# #               applyRules = TRUE,
#               keepUnspecified = FALSE)
# colnames(lldc.df)[grep("UNSD_LLDC_REG", colnames(lldc.df))] = "FAOST_CODE"
# ## Specify the area
# lldc.df$Area = "LLDC"
# ## Add country names
# lldc.df$FAO_TABLE_NAME = "Landlocked developing countries"
# ## FAOST_CODE
# lldc.df[, "FAOST_CODE"] = 5802

# Small island developing States ------------------------------------------

## It is also in the M49aggregates.R script
# ## Relation data frame
# sids = FAOcountryProfile[, c("FAOST_CODE", "UNSD_SIDS_REG")]
# ## Aggregation
# sids.df =  
#   Aggregation(data = country.df,
#               relationDF = sids,
#               aggVar = con.df[, "STS_ID"],
#               aggMethod = con.df[, "AGGREGATION"],
#               weightVar = con.df[, "STS_ID_WEIGHT"],
#               thresholdProp = con.df[, "THRESHOLD_PROP"],
# #               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
# #               applyRules = TRUE,
#               keepUnspecified = FALSE)
# colnames(sids.df)[grep("UNSD_SIDS_REG", colnames(sids.df))] = "FAOST_CODE"
# ## Specify the area
# sids.df$Area = "SIDS"
# ## Add country names
# sids.df$FAO_TABLE_NAME = "Small island developing States"
# ## FAOST_CODE
# sids.df[, "FAOST_CODE"] = 5803

# Low-income economies -----------------------------------------------------

## Relation data frame
lie = FAOcountryProfile[, c("FAOST_CODE", "WB_LIE_REG")]
## Aggregation
lie.df =  
  Aggregation(data = country.df,
              relationDF = lie,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(lie.df)[grep("WB_LIE_REG", colnames(lie.df))] = "FAOST_CODE"
## Specify the area
lie.df$Area = "LIE"
## Add country names
lie.df$FAO_TABLE_NAME = "Low-income economies"
## FAOST_CODE
lie.df[, "FAOST_CODE"] = 5858

# Lower-middle-income economies -------------------------------------------

## Relation data frame
lmie = FAOcountryProfile[, c("FAOST_CODE", "WB_LMIE_REG")]
## Aggregation
lmie.df =  
  Aggregation(data = country.df,
              relationDF = lmie,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(lmie.df)[grep("WB_LMIE_REG", colnames(lmie.df))] = "FAOST_CODE"
## Specify the area
lmie.df$Area = "LMIE"
## Add country names
lmie.df$FAO_TABLE_NAME = "Lower-middle-income economies"
## FAOST_CODE
lmie.df[, "FAOST_CODE"] = 5859

# Low-income food-deficit countries ---------------------------------------

## Relation data frame
lifdc = FAOcountryProfile[, c("FAOST_CODE", "FAO_LIFDC_REG")]
## Aggregation
lifdc.df =  
  Aggregation(data = country.df,
              relationDF = lifdc,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(lifdc.df)[grep("FAO_LIFDC_REG", colnames(lifdc.df))] = "FAOST_CODE"
## Specify the area
lifdc.df$Area = "LIFDC"
## Add country names
lifdc.df$FAO_TABLE_NAME = "Low-income food-deficit economies"
## FAOST_CODE
lifdc.df[, "FAOST_CODE"] = 5816

# Economic Aggregates -----------------------------------------------------

EconomicAggregates.df = 
  rbind(comesa.df, ecowas.df, eccas.df, sadc.df, igad.df, uma.df, 
        cemac.df, uemoa.df, censad.df, 
#         ldc.df, lldc.df, sids.df,
        lie.df, lmie.df, lifdc.df)
rm(list = c("comesa", "comesa.df", "ecowas", "ecowas.df", 
            "eccas", "eccas.df", "sadc", "sadc.df", 
            "igad", "igad.df", "uma", "uma.df", "cemac", "cemac.df",
            "uemoa", "uemoa.df", "censad", "censad.df", 
#             "ldc", "ldc.df", "lldc", "lldc.df", "sids", "sids.df", 
            "lie", "lie.df", "lmie", "lmie.df", "lifdc", "lifdc.df"))

saveRDS(EconomicAggregates.df, file = paste0(session_path,"/EconomicAggregates.df.RDS"))

