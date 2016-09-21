###########################################################################
## Title: This script computes the M49 groups.
## Created: 03-06-2014
## Modified: 03-06-2014
###########################################################################

# To allow parallel processing 
session_path <- readLines(con = "./input_data/session_path.txt")
load(paste0(session_path,"/pre_agg_image.RData"))
lapply(pkg_list, library, character.only = TRUE)


# M49 sub-regions ---------------------------------------------------------

## Relation data frame
M49subReg <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE")]))
## NOTE: we need to subset the dataset because some aggregations
## are not specified. This is the case for (e.g.) ex USSR (for which we
## cannot specify sub region, macro region etc. because it covers more
## than one area) and Antarctica (that has not a classification at all).
## aggregation.

## Aggregation
M49subReg.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49subReg[, "FAOST_CODE"],],
              relationDF = M49subReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49subReg.df[, "Area"] <- "M49subReg"
## Add country names
M49subRegName <- 
  subset(na.omit(unique(
    FAOcountryProfile[, c("UNSD_SUB_REG", "UNSD_SUB_REG_CODE")])))
M49subReg.df = merge(M49subReg.df, M49subRegName, all.x = TRUE, 
                     by = "UNSD_SUB_REG_CODE")
colnames(M49subReg.df)[which(colnames(M49subReg.df) == 
                               "UNSD_SUB_REG")] = "FAO_TABLE_NAME"
colnames(M49subReg.df)[which(colnames(M49subReg.df) == 
                               "UNSD_SUB_REG_CODE")] = "FAOST_CODE"

# Asia developing, Sub-Saharan Africa, Oceania developing -----------------

## Relation data frame
M49otherReg = FAOcountryProfile[, c("FAOST_CODE", "UNSD_OTHER_REG_CODE")]
## Aggregation
M49otherReg.df <-  
  Aggregation(data = country.df,
              relationDF = M49otherReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49otherReg.df[, "Area"] <- "M49otherReg"
## Add country names
M49otherRegName <- 
  subset(na.omit(unique(
    FAOcountryProfile[, c("UNSD_OTHER_REG", "UNSD_OTHER_REG_CODE")])))
M49otherReg.df = merge(M49otherReg.df, M49otherRegName, all.x = TRUE, 
                     by = "UNSD_OTHER_REG_CODE")
colnames(M49otherReg.df)[which(colnames(M49otherReg.df) == 
                               "UNSD_OTHER_REG")] = "FAO_TABLE_NAME"
colnames(M49otherReg.df)[which(colnames(M49otherReg.df) == 
                               "UNSD_OTHER_REG_CODE")] = "FAOST_CODE"
## Remove Channel Islands already taken into account in the first step agg
M49otherReg.df <- M49otherReg.df[!M49otherReg.df[, "FAOST_CODE"] %in% c(259), ]

# Latin America -----------------------------------------------------------
  
## Relation data frame
M49laReg <- subset(FAOcountryProfile, UNSD_SUB_REG_CODE %in% c(5204, 5207), select= c("FAOST_CODE"))
M49laReg[, "FAOST_CODE_LA"] <- 348
## Aggregation
M49laReg.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49laReg[, "FAOST_CODE"],],
              relationDF = M49laReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
colnames(M49laReg.df)[which(colnames(M49laReg.df) == "FAOST_CODE_LA")] <- "FAOST_CODE"
## Specify the area
M49laReg.df[, "Area"] <- "M49LatinAmerica"
## Add country names
M49laReg.df[, "FAO_TABLE_NAME"] <- "Latin America"

# Caribbean ---------------------------------------------------------------

## Relation data frame
M49lacReg <- subset(FAOcountryProfile, UNSD_SUB_REG_CODE %in% c(5204, 5206, 5207), "FAOST_CODE")
M49lacReg[, "FAOST_CODE_LAC"] <- 5205
## Aggregation
M49lacReg.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49lacReg[, "FAOST_CODE"],],
              relationDF = M49lacReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49lacReg.df[, "Area"] <- "M49LatinAmericaAndCaribbean"
## Add country names
M49lacReg.df[, "FAO_TABLE_NAME"] <- "Latin America and the Caribbean"
colnames(M49lacReg.df)[which(colnames(M49lacReg.df) == "FAOST_CODE_LAC")] <- "FAOST_CODE"

# macro-region  -----------------------------------------------------------

## Relation data frame
M49macroReg <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_MACRO_REG_CODE")]))
## Aggregation
M49macroReg.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49macroReg[, "FAOST_CODE"],],
              relationDF = M49macroReg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49macroReg.df[, "Area"] <- "M49macroReg"
## Add country names
M49macroRegName = 
  subset(na.omit(unique(FAOcountryProfile[, c("UNSD_MACRO_REG", "UNSD_MACRO_REG_CODE")])))
M49macroReg.df = merge(M49macroReg.df, M49macroRegName, all.x = TRUE, 
                       by = "UNSD_MACRO_REG_CODE")
colnames(M49macroReg.df)[which(colnames(M49macroReg.df) == 
                                 "UNSD_MACRO_REG")] = "FAO_TABLE_NAME"
colnames(M49macroReg.df)[which(colnames(M49macroReg.df) == 
                                 "UNSD_MACRO_REG_CODE")] = "FAOST_CODE"

# World  ------------------------------------------------------------------
  
## Relation data frame
M49world <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_WORLD_REG_CODE")]))
## Aggregation
M49world.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49world[, "FAOST_CODE"],],
              relationDF = M49world,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49world.df[, "Area"] <- "M49world"
## Add country names
M49world.df[, "FAO_TABLE_NAME"] <- "World"
colnames(M49world.df)[which(colnames(M49world.df) == 
                              "UNSD_WORLD_REG_CODE")] = "FAOST_CODE"
    
# Developed Developing  ---------------------------------------------------

## Relation data frame
M49dvddvg <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_DVDDVG_REG_CODE")]))
## Aggregation
M49dvddvg.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49dvddvg[, "FAOST_CODE"],],
              relationDF = M49dvddvg,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49dvddvg.df[, "Area"] <- "DevLev"
## Add country names
M49dvddvgName = 
  subset(na.omit(unique(FAOcountryProfile[, c("UNSD_DVDDVG_REG", "UNSD_DVDDVG_REG_CODE")])))
M49dvddvg.df = merge(M49dvddvg.df, M49dvddvgName, all.x = TRUE, 
                       by = "UNSD_DVDDVG_REG_CODE")
colnames(M49dvddvg.df)[which(colnames(M49dvddvg.df) == 
                                 "UNSD_DVDDVG_REG")] = "FAO_TABLE_NAME"
colnames(M49dvddvg.df)[which(colnames(M49dvddvg.df) == 
                                 "UNSD_DVDDVG_REG_CODE")] = "FAOST_CODE"

# Least developed countries -----------------------------------------------

## Relation data frame
M49ldc <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_LDC_REG")]))
## Aggregation
M49ldc.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49ldc[, "FAOST_CODE"],],
              relationDF = M49ldc,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49ldc.df[, "Area"] <- "LDC"
## Add country names
colnames(M49ldc.df)[which(colnames(M49ldc.df) == 
                            "UNSD_LDC_REG")] = "FAOST_CODE"
M49ldc.df[, "FAOST_CODE"] = 5801
M49ldc.df[, "FAO_TABLE_NAME"] <- "Least developed countries"
  
# Landlocked developing countries  ----------------------------------------

## Relation data frame
M49lldc <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_LLDC_REG")]))
## Aggregation
M49lldc.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49lldc[, "FAOST_CODE"],],
              relationDF = M49lldc,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49lldc.df[, "Area"] <- "LLDC"
## Add country names
colnames(M49lldc.df)[which(colnames(M49lldc.df) == 
                            "UNSD_LLDC_REG")] = "FAOST_CODE"
M49lldc.df[, "FAOST_CODE"] = 5802
M49lldc.df[, "FAO_TABLE_NAME"] <- "Landlocked developing countries"

# Small island developing States  -----------------------------------------

## Relation data frame
M49sids <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_SIDS_REG")]))
## Aggregation
M49sids.df <-  
  Aggregation(data = country.df[country.df[, "FAOST_CODE"] %in% M49sids[, "FAOST_CODE"],],
              relationDF = M49sids,
              aggVar = con.df[, "STS_ID"],
              aggMethod = con.df[, "AGGREGATION"],
              weightVar = con.df[, "STS_ID_WEIGHT"],
              thresholdProp = con.df[, "THRESHOLD_PROP"],
#               thresholdCountry = con.df[, "THRESHOLD_COUNTRIES"],
#               applyRules = TRUE,
              keepUnspecified = FALSE)
## Specify the area
M49sids.df[, "Area"] <- "LLDC"
## Add country names
colnames(M49sids.df)[which(colnames(M49sids.df) == 
                             "UNSD_SIDS_REG")] = "FAOST_CODE"
M49sids.df[, "FAOST_CODE"] = 5803
M49sids.df[, "FAO_TABLE_NAME"] <- "Small island developing States"

# M49.df  -----------------------------------------------------------------
  
M49.df = rbind(M49subReg.df, M49laReg.df, M49lacReg.df,
               M49otherReg.df, M49macroReg.df, M49world.df,
               M49dvddvg.df, M49ldc.df, M49lldc.df, M49sids.df)
rm(list = c("M49subReg.df", "M49subReg", "M49subRegName",
            "M49laReg.df", "M49laReg", "M49lacReg.df", "M49lacReg",
            "M49otherReg.df", "M49otherReg", "M49macroReg.df", "M49macroReg", 
            "M49macroRegName", "M49otherRegName",
            "M49world.df", "M49world", "M49dvddvg.df", "M49dvddvg", "M49dvddvgName",
            "M49ldc.df", "M49ldc", "M49lldc.df", "M49lldc",
            "M49sids.df", "M49sids"))

saveRDS(M49.df, file = paste0(session_path,"/M49.df.RDS"))
