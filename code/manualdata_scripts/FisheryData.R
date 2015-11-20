###########################################################################
## This script processes the FISH variables
###########################################################################

## NOTE (FILIPPO): Instruction for exporting fishery's data
##                 Use of FishstatJ:
##                 - Open a new dataset, and choose the dataset
##                 - Data, Aggregate in order to choose the aggregation
##                 - Data, Filter in order to choose the measure
##                 - Select the countries, right clic, choose attribute, ISO 3
##                 - Tools, Preferences, Data display, Format highlights
##                 - Export with UN_CODE (the only mismatch will be "Other",
##                   UN_CODE = 896) that indicates Antartica areas
## NOTE (FILIPPO): Usually FishstatJ data are not updated. For this reason,
## data are usually provided by Tsuji, Sachiko (FIPS) or 
## Vannuccini, Stefania (FIPS).

# FI.PRD.AQ.TN.NO ---------------------------------------------------------

aquaProdTot.df <- 
    read.csv(file = "./Data/Raw/AquacultureProduction.csv", 
             header = TRUE, na.strings = "", stringsAsFactors = FALSE)
colnames(aquaProdTot.df)[1] <- "UN_CODE"
aquaProdTot.df <- 
  translateCountryCode(aquaProdTot.df, from = "UN_CODE", to = "FAOST_CODE")
## NOTE (FILIPPO): What is in FishStatJ as 156 UN_CODE is not really 
## China + Taiwan but just China (because they show Taiwan separately).
## Thus, we need to change 357 to 41. Moreover we need to remove 896.
aquaProdTot.df <- subset(aquaProdTot.df, UN_CODE != 896)
aquaProdTot.df[aquaProdTot.df$FAOST_CODE == 357, "FAOST_CODE"] = 41
aquaProdTot.df <- 
  melt(aquaProdTot.df[, -grep("UN_CODE|CountryName", colnames(aquaProdTot.df))], 
       id = "FAOST_CODE")
colnames(aquaProdTot.df) <- c("FAOST_CODE", "Year", "FI.PRD.AQ.TN.NO")
aquaProdTot.df[, "Year"] <- 
  as.integer(as.character(gsub("X", "", aquaProdTot.df[,"Year"])))

# FI.PRD.CAPT.TN.NO -------------------------------------------------------

captProdTot.df <- 
  read.csv(file = "./Data/Raw/CaptureProduction.csv", 
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
colnames(captProdTot.df)[1] <- "UN_CODE"
captProdTot.df <- 
  translateCountryCode(captProdTot.df, from = "UN_CODE", to = "FAOST_CODE")
## NOTE (FILIPPO): What is in FishStatJ as 156 UN_CODE is not really 
## China + Taiwan but just China (because they show Taiwan separately).
## Thus, we need to change 357 to 41. Moreover we need to remove 896.
captProdTot.df <- subset(captProdTot.df, UN_CODE != 896)
captProdTot.df[captProdTot.df$FAOST_CODE == 357, "FAOST_CODE"] = 41
captProdTot.df <- 
  melt(captProdTot.df[, -grep("UN_CODE|CountryName", colnames(captProdTot.df))], 
       id = "FAOST_CODE")
colnames(captProdTot.df) <- c("FAOST_CODE", "Year", "FI.PRD.CAPT.TN.NO")
captProdTot.df[, "Year"] <- 
  as.integer(as.character(gsub("X", "", captProdTot.df[,"Year"])))

# FI.EXVAL.FISH.USD.NO ----------------------------------------------------

fishExports.df <- 
  read.csv(file = "./Data/Raw/FishExports.csv", 
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
colnames(fishExports.df)[1] <- "UN_CODE"
fishExports.df <- 
  translateCountryCode(fishExports.df, from = "UN_CODE", to = "FAOST_CODE")
## NOTE (FILIPPO): What is in FishStatJ as 156 UN_CODE is not really 
## China + Taiwan but just China (because they show Taiwan separately).
## Thus, we need to change 357 to 41. Moreover we need to remove 896.
fishExports.df <- subset(fishExports.df, UN_CODE != 896)
fishExports.df[fishExports.df$FAOST_CODE == 357, "FAOST_CODE"] = 41
fishExports.df <- 
  melt(fishExports.df[, -grep("UN_CODE|CountryName", colnames(fishExports.df))], 
       id = "FAOST_CODE")
colnames(fishExports.df) <- c("FAOST_CODE", "Year", "FI.EXVAL.FISH.USD.NO")
fishExports.df[, "Year"] <- 
  as.integer(as.character(gsub("X", "", fishExports.df[,"Year"])))

# FI.IMVAL.FISH.USD.NO ----------------------------------------------------

fishImports.df <- 
  read.csv(file = "./Data/Raw/FishImports.csv", 
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
colnames(fishImports.df)[1] <- "UN_CODE"
fishImports.df <- 
  translateCountryCode(fishImports.df, from = "UN_CODE", to = "FAOST_CODE")
## NOTE (FILIPPO): What is in FishStatJ as 156 UN_CODE is not really 
## China + Taiwan but just China (because they show Taiwan separately).
## Thus, we need to change 357 to 41. Moreover we need to remove 896.
fishImports.df <- subset(fishImports.df, UN_CODE != 896)
fishImports.df[fishImports.df$FAOST_CODE == 357, "FAOST_CODE"] = 41
fishImports.df <- 
  melt(fishImports.df[, -grep("UN_CODE|CountryName", colnames(fishImports.df))], 
       id = "FAOST_CODE")
colnames(fishImports.df) <- c("FAOST_CODE", "Year", "FI.IMVAL.FISH.USD.NO")
fishImports.df[, "Year"] <- 
  as.integer(as.character(gsub("X", "", fishImports.df[,"Year"])))

# merge -------------------------------------------------------------------

Fishery.df <- data.frame()
Fishery.df <- 
  Reduce(function(x, y) merge(x, y, all = TRUE),
         x = list(aquaProdTot.df, captProdTot.df, fishExports.df, fishImports.df))
rm(list = c("aquaProdTot.df", "captProdTot.df", "fishExports.df", "fishImports.df"))
save(x = Fishery.df, file = "./Data/Processed/Fishery.RData")
