###########################################################################
## This script generates the dataset for the Statistical Yearbooks
###########################################################################

root.dir <- "~/btsync/fao_sync/syb_database/"
setwd(root.dir)


###########################################################################
## Settings
###########################################################################


# Needed libraries --------------------------------------------------------

require(devtools)
if (!"FAOSTAT" %in% installed.packages()) {
  install_github(username = "mkao006", repo = "FAOSTATpackage",
                 ref = "master", subdir = "FAOSTAT")
}
require(FAOSTAT)
require(plyr)
require(dplyr)
require(reshape2)
require(data.table)



# Packrat -----------------------------------------------------------------

# packrat::status()
# packrat::snapshot()
# packrat::restore()

# Source functions --------------------------------------------------------
source("./Rcode/Final/misc_functions/CheckLogical.R")
source("./Rcode/Final/misc_functions/CheckValues.R")
source("./Rcode/Final/misc_functions/Sourcehttps.R")
source("./Rcode/Final/misc_functions/sum2.R")
source("./Rcode/Final/read_functions/ReadConstruction.R")
source("./Rcode/Final/read_functions/ReadMetadata.R")


## -- Sourcings FAOSYBpackage from SYBdatabase folder. These used to sourced from Filippos github repo



###########################################################################
## User inputs
###########################################################################

# Country profile ---------------------------------------------------------

load("./Data/Processed/FAOcountryProfile.RData")
#FAOcountryProfile <- read.csv("./FAOcountryProfile.csv", header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8")

# Construction and metadata files -----------------------------------------

con.df <- ReadConstruction(file = "Construction2015.csv", 
                           encoding = "UTF-8", nrows = 665)
save(x = con.df, file = "./Data/Processed/Construction.RData")
meta.lst <- ReadMetadata(file = "Metadata2015.csv", 
                         encoding = "UTF-8", nrows = 665)
save(x = meta.lst, file = "./Data/Processed/Metadata.RData")

# con.df <- ReadConstruction(file = "./Data/old/Construction04-2014.csv", 
#                            encoding = "UTF-8", nrows = 670)
# save(x = con.df, file = "./Data/Processed/Construction.RData")
# meta.lst <- ReadMetadata(file = "./Data//old/Metadata04-2014.csv", 
#                          encoding = "UTF-8", nrows = 670)
# save(x = meta.lst, file = "./Data/Processed/Metadata.RData")




###########################################################################
## Data collection
###########################################################################

# Download variables from FAOSTAT, parameters -----------------------------

faostatData.df <- meta.lst[["FAOSTAT"]]
dwnldOA <- FALSE # Population
dwnldRL <- FALSE # Resources, Resources - Land
dwnldRF <- FALSE # Resources - Fertilizers
dwnldRP <- FALSE # Resources - Pesticides
dwnldCS <- FALSE # Investments - Capital stock
dwnldRM <- FALSE # Investments - Machinery
dwnldIG <- FALSE # Government expenditures
dwnldA <- FALSE # ASTI
dwnldQC <- FALSE # Production - Crops
dwnldQA <- FALSE # Production - Live animals
dwnldQD <- FALSE # Production - Crops processed
dwnldQL <- FALSE # Production - Livestock primary
dwnldQP <- FALSE # Production - Livestock processed
dwnldQV <- FALSE # Production - Value of agricultural production
dwnldQI <- FALSE # Production indices
dwnldTP <- FALSE # Trade - Crops and livestock products
dwnldTI <- FALSE # Trade - Trade indices
dwnldFO <- FALSE # Forestry
dwnldGHG <- FALSE # Greenhouse gases
dwnldFB <- FALSE # Food balance sheets

# Download variables from WORLD BANK, parameters -----------------------------

downloadWB <- FALSE; CheckLogical(downloadWB)

# FAOSTAT, Population - Annual population ---------------------------------

if (dwnldOA) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "OA",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOoa.df <- FAO.lst$entity; rm(dwnldOA); rm(FAO.lst)
  ## ...update list
  save(x = FAOoa.df, file = paste0("./Data/Processed/FAOoa", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOoa2015-04-15.RData")
}

# FAOSTAT, Resources - Land -----------------------------------------------

if (dwnldRL) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RL",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrl.df <- FAO.lst$entity; rm(dwnldRL); rm(FAO.lst)
  ## ...update list
  save(x = FAOrl.df, file = paste0("./Data/Processed/FAOrl", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOrl2015-04-15.RData")
}

# FAOSTAT, Resources - Fertilizers ----------------------------------------

if (dwnldRF) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RF",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrf.df <- FAO.lst$entity; rm(dwnldRF); rm(FAO.lst)
  ## ...update list
  save(x = FAOrf.df, file = paste0("./Data/Processed/FAOrf", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOrf2015-04-15.RData")
}

# FAOSTAT, Resources - Pesticides -----------------------------------------

if (dwnldRP) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RP",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrp.df <- FAO.lst$entity; rm(dwnldRP); rm(FAO.lst)
  ## ...update list
  save(x = FAOrp.df, file = paste0("./Data/Processed/FAOrp", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOrp2015-04-15.RData")
}

# FAOSTAT, Investments - Capital stock ------------------------------------

if (dwnldCS) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "CS",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOcs.df <- FAO.lst$entity; rm(dwnldCS); rm(FAO.lst)
  ## ...update list
  save(x = FAOcs.df, file = paste0("./Data/Processed/FAOcs", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOcs2015-04-15.RData")
}

# FAOSTAT, Investments - Machinery ----------------------------------------

if (dwnldRM) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RM",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrm.df <- FAO.lst$entity; rm(dwnldRM); rm(FAO.lst)
  ## ...update list
  save(x = FAOrm.df, file = paste0("./Data/Processed/FAOrm", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOrm2015-04-15.RData")
}

# FAOSTAT, Government expenditures ----------------------------------------

if (dwnldIG) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "IG",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOig.df <- FAO.lst$entity; rm(dwnldIG); rm(FAO.lst)
  ## ...update list
  save(x = FAOig.df, file = paste0("./Data/Processed/FAOig", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOig2014-12-05.RData") # so the last time this was built succesfully
}

# FAOSTAT, ASTI -----------------------------------------------------------

if (dwnldA) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] %in% c("AA", "AE", "AR"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOa.df <- FAO.lst$entity; rm(dwnldA); rm(FAO.lst)
  ## ...update list
  save(x = FAOa.df, file = paste0("./Data/Processed/FAOa", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOa2015-04-15.RData")
}

# FAOSTAT, Production - Crops ---------------------------------------------

if (dwnldQC) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QC",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqc.df <- FAO.lst$entity; rm(dwnldQC); rm(FAO.lst)
  ## ...update list
  save(x = FAOqc.df, file = paste0("./Data/Processed/FAOqc", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOqc2015-04-15.RData")
}

# FAOSTAT, Production - Live animals --------------------------------------

if (dwnldQA) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QA",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqa.df <- FAO.lst$entity; rm(dwnldQA); rm(FAO.lst)
  ## ...update list
  save(x = FAOqa.df, file = paste0("./Data/Processed/FAOqa", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOqa2015-04-15.RData")
}

# FAOSTAT, Production - Crops processed -----------------------------------

if (dwnldQD) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QD",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqd.df <- FAO.lst$entity; rm(dwnldQD); rm(FAO.lst)
  ## ...update list
  save(x = FAOqd.df, file = paste0("./Data/Processed/FAOqd", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOqd2015-04-15.RData")
}

# FAOSTAT, Production - Livestock primary ---------------------------------

if (dwnldQL) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QL",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOql.df <- FAO.lst$entity; rm(dwnldQL); rm(FAO.lst)
  ## ...update list
  save(x = FAOql.df, file = paste0("./Data/Processed/FAOql", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOql2015-04-15.RData")
}

# FAOSTAT, Production - Livestock processed -------------------------------

if (dwnldQP) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QP",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqp.df <- FAO.lst$entity; rm(dwnldQP); rm(FAO.lst)
  ## ...update list
  save(x = FAOqp.df, file = paste0("./Data/Processed/FAOqp", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOqp2015-04-15.RData")
}

# FAOSTAT, Production - Value of agricultural production ------------------

if (dwnldQV) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QV",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqv.df <- FAO.lst$entity; rm(dwnldQV); rm(FAO.lst)
  ## ...update list
  save(x = FAOqv.df, file = paste0("./Data/Processed/FAOqv", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOqv2015-04-15.RData")
}

# FAOSTAT, Production - Production indices --------------------------------

if (dwnldQI) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QI",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqi.df <- FAO.lst$entity; rm(dwnldQI); rm(FAO.lst)
  ## ...update list
  save(x = FAOqi.df, file = paste0("./Data/Processed/FAOqi", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOqi2015-04-15.RData")
}

# FAOSTAT, Trade - Crops and livestock products ---------------------------

if (dwnldTP) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "TP",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOtp.df <- FAO.lst$entity; rm(dwnldTP); rm(FAO.lst)
  ## ...update list
  save(x = FAOtp.df, file = paste0("./Data/Processed/FAOtp", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOtp2015-05-27.RData")
}

# FAOSTAT, Trade - Trade indices ------------------------------------------

if (dwnldTI) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "TI",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOti.df <- FAO.lst$entity; rm(dwnldTI); rm(FAO.lst)
  ## ...update list
  save(x = FAOti.df, file = paste0("./Data/Processed/FAOti", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOti2015-05-27.RData")
}

# FAOSTAT, Forestry -------------------------------------------------------

if (dwnldFO) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "FO",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOfo.df <- FAO.lst$entity; rm(dwnldFO); rm(FAO.lst)
  ## ...update list
  save(x = FAOfo.df, file = paste0("./Data/Processed/FAOfo", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOfo2015-04-15.RData")
}

# FAOSTAT, Greenhouse gases -----------------------------------------------

if (dwnldGHG) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] %in% c("GT", "GF", "GL", "GN", "GI"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOghg.df <- FAO.lst$entity; rm(dwnldGHG); rm(FAO.lst)
  for (i in 3:NCOL(FAOghg.df)) {
    FAOghg.df[, i] <- as.numeric(FAOghg.df[, i])
  }
  ## ...update list
  save(x = FAOghg.df, file = paste0("./Data/Processed/FAOghg", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOghg2015-04-15.RData")
}

# FAOSTAT, Food balance sheets --------------------------------------------

if (dwnldFB) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] %in% c("CC"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              productionDB = FALSE,
                              useCHMT = FALSE))
  FAOfb.df <- FAO.lst$entity; rm(dwnldFB); rm(FAO.lst)
  for (i in 3:NCOL(FAOfb.df)) {
    FAOfb.df[, i] <- as.numeric(FAOfb.df[, i])
  }
  ## ...update list
  save(x = FAOfb.df, file = paste0("./Data/Processed/FAOfb", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./Data/Processed/FAOfb2015-04-15.RData")
}

# Download variables from WB ----------------------------------------------

if (downloadWB) {
  ## Download data from WDI
  WB.lst <- with(meta.lst[["WDI"]],
                 getWDItoSYB(indicator = WDINAME, name = STS_ID))
  ## ...update list
  save(x = WB.lst, file = paste0("./Data/Processed/WBdata", Sys.Date(), ".RData"))
} else {
  ## ...open list
  #   load(file = "./Data/Processed/WBdata2015-03-11.RData")
  # load(file = "./Data/Processed/WBdata2014-07-07.RData")
  load(file = "./Data/Processed/WBdata2015-04-17.RData")
}
WB.df <- WB.lst$entity
## This is a mistake in the WDI database
# WB.df[WB.df[, "Country"] == "Cabo Verde" & WB.df[, "Year"] %in% c(1960:2012), 
#       c("SP.POP.1564.FE.IN", "SP.POP.65UP.FE.IN", "SP.POP.1564.MA.IN", "SP.POP.65UP.MA.IN")] =
#   WB.df[WB.df[, "Country"] == "Cape Verde" & WB.df[, "Year"] %in% c(1960:2012), 
#         c("SP.POP.1564.FE.IN", "SP.POP.65UP.FE.IN", "SP.POP.1564.MA.IN", "SP.POP.65UP.MA.IN")]
# WB.df <- WB.df[WB.df[, "Country"] != "Cape Verde",]
WB.df <- WB.df[!duplicated(WB.df[c("ISO2_WB_CODE","Year")]),]
##
WB.df <- translateCountryCode(data = WB.df, 
                              from = "ISO2_WB_CODE", to = "FAOST_CODE")
WB.df <- WB.df[, -grep("ISO2_WB_CODE|Country", colnames(WB.df))]
#rm(downloadWB); rm(WB.lst)

#WB.df <- WB.df[!duplicated(WB.df[c("FAOST_CODE","Year")]),]

# Manual data -------------------------------------------------------------

load("./Data/Processed/wbManualData.RData")
load("./Data/Processed/AquastatManualData.RData")
load("./Data/Processed/GlobalForestResourceAssessment.RData")
load("./Data/Processed/BiofuelProduction.RData")
load("./Data/Processed/Fishery.RData")
load("./Data/Processed/UNPopManualData.RData")


# load("./database/Data/Processed/wbManualData.RData")
# load("./database/Data/Processed/AquastatManualData.RData")
# load("./database/Data/Processed/UNPopManualData.RData")
# load("./database/Data/Processed/Fishery.RData")
# load("./database/Data/Processed/BiofuelProduction.RData")
# load("./database/Data/Processed/GlobalForestResourceAssessment.RData")

# load("./Data/Processed/AquastatManualData_old.RData")
# o <- AquastatManualData.df
# load("./Data/Processed/AquastatManualData.RData")
# n <- AquastatManualData.df; rm(AquastatManualData.df)
# load("./Data/Processed/wbManualData.RData")
# w <- WBManualData.df
# load("./Data/Processed/UNPopManualData.RData")
# p <- UNPopManualData.df; rm(UNPopManualData.df)
# 
# dim(o);dim(n)
# head(o);head(n)
# str(o);str(n);str(p);str(w)







###########################################################################
## Processing
###########################################################################

# Merging -----------------------------------------------------------------

initial.df = Reduce(function(x, y) merge(x, y, all = TRUE),
                    x = list(FAOoa.df, # FAOSTAT, Population - Annual population
                             FAOrl.df, # FAOSTAT, Resources - Land
                             FAOrf.df, # FAOSTAT, Resources - Fertilizers
                             FAOrp.df, # FAOSTAT, Resources - Pesticides
                             FAOcs.df, # FAOSTAT, Investments - Capital stock
                             FAOrm.df, # FAOSTAT, Investments - Machinery
                             FAOig.df, # FAOSTAT, Government expenditures
                             FAOa.df, # FAOSTAT, ASTI
                             FAOqc.df, # FAOSTAT, Production - Crops
                             FAOqa.df, # FAOSTAT, Production - Live animals
                             FAOqd.df, # FAOSTAT, Production - Crops processed
                             FAOql.df, # FAOSTAT, Production - Livestock primary
                             FAOqp.df, # FAOSTAT, Production - Livestock processed
                             FAOqv.df, # FAOSTAT, Production - Value of agricultural production
                             FAOqi.df, # FAOSTAT, Production - Production indices
                             FAOtp.df, # FAOSTAT, Trade - Crops and livestock products
                             FAOti.df, # FAOSTAT, Trade - Trade indices
                             FAOfo.df, # FAOSTAT, Forestry
                             FAOghg.df,# FAOSTAT, Greenhouse gases
                             FAOfb.df, # FAOSTAT, Food balance sheets
                             #WB.df, # Download variables from WB
                             WBManualData.df, # not_update
                             AquastatManualData.df, # updated 2015
                             gfra.df, # not updated
                             BiofuelProduction.df, # not update
                             Fishery.df, # not updated  
                             UNPopManualData.df), # added by Markus 20150401
                    init = WB.df)
# rm(list = c("dwnldA", "dwnldCS", "dwnldFB", "dwnldFO", "dwnldGHG", "dwnldIG",
#             "dwnldOA", "dwnldQA", "dwnldQC", "dwnldQD", "dwnldQI", "dwnldQL",
#             "dwnldQP", "dwnldQV", "dwnldRF", "dwnldRL", "dwnldRM", "dwnldRP",
#             "dwnldTI", "dwnldTP"))
# rm(list = c("FAOoa.df", "FAOrl.df", "FAOrf.df", "FAOrp.df", "FAOcs.df", "FAOrm.df", 
#             "FAOig.df", "FAOa.df", "FAOqc.df", "FAOqa.df", "FAOqd.df", "FAOql.df", 
#             "FAOqp.df", "FAOqv.df", "FAOqi.df", "FAOtp.df", "FAOti.df", "FAOfo.df", 
#             "FAOghg.df", "WBManualData.df", "WB.df", "faostatData.df", "FAOfb.df",
#             "AquastatManualData.df", "gfra.df", "BiofuelProduction.df",
#             "Fishery.df"))
initial.df <- initial.df[initial.df[, "Year"] <= 2020,]

# Scale data to basic unit ------------------------------------------------
## -- Convert the characters formats as "thousand" into 1000

meta.lst[["UNIT_MULT"]][, "UNIT_MULT"] <- as.numeric(translateUnit(meta.lst[["UNIT_MULT"]]$UNIT_MULT))


preConstr.df <- scaleUnit(initial.df, meta.lst[["UNIT_MULT"]])
rm(initial.df)

#pre1 <- preConstr.df

# Imputation --------------------------------------------------------------

# Construction ------------------------------------------------------------

## Manual
source("./Rcode/Final/ManualConstruction.R")


## Automatic
postConstr.lst <- with(con.df[con.df[, "CONSTRUCTION_TYPE"] %in% c("share", "growth", "change", "index"),], # leave the manual construction outside
                       constructSYB(data = preConstr.df,
                                    origVar1 = STS_ID_CONSTR1,
                                    origVar2 = STS_ID_CONSTR2,
                                    newVarName = STS_ID,
                                    constructType = CONSTRUCTION_TYPE,
                                    grFreq = GROWTH_RATE_FREQ,
                                    grType = GROWTH_TYPE, 
                                    baseYear = 2000))
preAgg.df <- postConstr.lst$data 
rm(list = c("preConstr.df", "postConstr.lst"))

# Adjustment in scaling ---------------------------------------------------

# either of these - some issues prevail

#manScalVars <- subset(con.df, select = c("STS_ID", "SCALING"), subset = !is.na(SCALING))
manScalVars <- con.df[!(is.na(con.df[["SCALING"]])), c("STS_ID", "SCALING")] # Markus fix

# Removed var QA.STCK.PIG.HD.SHW  100 "Pigs, share of world" because it broked the manual scaling..
manScalVars <- manScalVars[-61,]

for (i in 1:NROW(manScalVars)) {
  preAgg.df[, manScalVars[i, "STS_ID"]] <- preAgg.df[, manScalVars[i, "STS_ID"]] * manScalVars[i, "SCALING"]
}
rm(list = c("manScalVars", "i"))

# Aggregations ------------------------------------------------------------
# source("./Rcode/Final/aggregate_functions/EconomicAggregates.R")
# source("./Rcode/Final/aggregate_functions/FAOAggregates.R")
# source("./Rcode/Final/aggregate_functions/SofiAggregates.R")

## Country aggregation
source("./Rcode/Final/aggregate_functions/CountryAggregation.R")

## China aggregation
source("./Rcode/Final/aggregate_functions/ChinaAggregates.R")

## Check overlapping in old countries
FAOchecked.df <- FAOcheck(var = colnames(country.df)[-grep("FAOST_CODE|Year|Area", colnames(country.df))],
                          data = country.df, type = "overlap",
                          take = "complete")
OldCountries <- 
  data.frame(FAOST_CODE = c(15,51,62,151,186,228,206,247,246,248),
             COUNTRY_NAME = c("Belgium-Luxembourg", "Czechoslovakia",
                              "Ethiopia PDR", "Netherlands Antilles",
                              "Serbia and Montenegro", "Soviet Union",
                              "Sudan (former)", "Yemen (former)", 
                              "Yemen (old)", "Yugoslav SFR"),
             stringsAsFactors = FALSE)
country.df[country.df[, "FAOST_CODE"] %in% OldCountries$FAOST_CODE, "Area"] =
  "Old territory"
rm(OldCountries)
## Add country names
country.df <- merge(country.df, FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")], 
                    by = "FAOST_CODE", all.x = FALSE)
## M49 aggregates
do49aggr <- TRUE
if (do49aggr) {
  source("./Rcode/Final/aggregate_functions/M49aggregates.R")
} else load()

## FAO aggregates
# Sourcehttps(source("./Rcode/Final/ComplementaryScripts/FAOAggregates.R")
## Economic aggregates
# Sourcehttps(source("./Rcode/Final/ComplementaryScripts/EconomicAggregates.R")
## rbind the datasets
postAgg.df <- rbind(country.df, M49.df)
# postAgg.df <- rbind(country.df, M49.df)
# rm(list = c("country.df", "M49.df")) # because M49.df takes a loooooong time to run

# Check for NaN, Inf, -Inf ------------------------------------------------

postAgg.df <- CheckValues(dataset = postAgg.df, columns = colnames(postAgg.df)[
  -grep("FAOST_CODE|Year|Area|POU_DISTR|FAO_TABLE_NAME", colnames(postAgg.df))])

SYB.df <- postAgg.df
save(x = SYB.df, file = "./Data/Processed/SYB_pre_fsi.RData")
load(file = "./Data/Processed/SYB_pre_fsi.RData")


# Merge the FSI dataset ---------------------------------------------------

## Merge the dataset
load(file = "./Data/Processed/fsi.RData")
# fsiVar <- c("FAOST_CODE", "Year",
#             "AV3YADESA.DISS", "QV.NPV.FOOD.ID.AV3YSHP.DISS", "FB.SDES.CRLSSR.KCD.AV3Y.DISS",
#             "FB.PSQ.GT.GCD.AV3Y.DISS", "FB.PSQ.AO.GCD.AV3Y.DISS", "FB.FSQ.GT.GCD.AV3Y.DISS",
#             "IS.ROD.PAVE.ZS.DISS", "IS.ROD.DNST.K2.DISS", "IS.RRS.DNST.K2.DISS",
#             "DFPLI.IN.NO.DISS", "SH.H2O.SAFE.ZS", "SH.STA.ACSN",
#             "FB.CIDR.CRLS.TN.AV3Y.DISS", "RL.AREA.EQIRR.HA.SHLAV3Y.DISS",
#             "TI.IV.FEFTMT.USD.AV3Y.DISS", "SFEP.NO", "WGI.PSAVT.IN.NO",  
#             "PCFPV.IN.NO.DISS", "PCFSV.IN.NO.DISS", "DFPLIV.IN.NO.DISS",
#             "SH.ANM.CHLD.ZS", "VITAMINA", "IODINE", "SH.PRG.ANEM",
#             "SH.STA.WAST.ZS", "SH.STA.STNT.ZS", "SH.STA.MALN.ZS", "SH.STA.AMALN.ZS",
#             "AV3YMDER_1.55.DISS", "AV3YADER_1.85.DISS", "AV3YMDER_1.75.DISS", "CV.DISS", "SK.DISS",
#             "LOSS.DISS", "AV3YDES.DISS", "AV3YPOU.DISS", "AV3YNOU.DISS", 
#             "AV3YPOU", "AV3YNOU", "AV3YDoFD.DISS", "AV3YPoFI.DISS", "AV3YPOP")
# fsi.df <- fsi.df[, fsiVar]
SYB.df <- merge(SYB.df, fsi.df, all = FALSE, by = c("FAOST_CODE", "Year"))
rm(fsi.df)


### FOR SOME REASON SOME VARS END UP WIITH .x
# names(SYB.df)[grep(".x", names(SYB.df))]

### HACK BEGINS #############

SYB.df$SH.DYN.MORT.y <- NULL
names(SYB.df)[names(SYB.df)=="SH.DYN.MORT.x"] <- "SH.DYN.MORT"

SYB.df$RL.AREA.ARBL.HA.NO.y <- NULL
names(SYB.df)[names(SYB.df)=="RL.AREA.ARBL.HA.NO.x"] <- "RL.AREA.ARBL.HA.NO"

SYB.df$QC.PRD.CRLS.TN.NO.y <- NULL
names(SYB.df)[names(SYB.df)=="QC.PRD.CRLS.TN.NO.x"] <- "QC.PRD.CRLS.TN.NO"

SYB.df$QV.NPV.FOOD.ID.NO.y <- NULL
names(SYB.df)[names(SYB.df)=="QV.NPV.FOOD.ID.NO.x"] <- "QV.NPV.FOOD.ID.NO"

SYB.df$Area.y <- NULL
names(SYB.df)[names(SYB.df)=="Area.x"] <- "Area"

SYB.df$FAO_TABLE_NAME.y <- NULL
names(SYB.df)[names(SYB.df)=="FAO_TABLE_NAME.x"] <- "FAO_TABLE_NAME"

### HACK ENDS ###########

save(x = SYB.df, file = "./Data/Processed/SYB.RData")

## Merge metadata and construction file
fsicon.df <- ReadConstruction(file = "FSIconstruction15.csv", 
                              encoding = "UTF-8", nrows = 287)
fsimeta.lst <- ReadMetadata(file = "FSImetadata15.csv", 
                            encoding = "UTF-8", nrows = 287)
fsimeta.df <- fsimeta.lst[["FULL"]]

fsicon.df <- fsicon.df[fsicon.df[, "STS_ID"] %in% fsiVar,]
fsimeta.df <- fsimeta.df[fsimeta.df[, "STS_ID"] %in% fsiVar,]

meta.lst[["FULL"]][["X"]] <- NULL
meta.lst[["FULL"]] <- rbind(meta.lst[["FULL"]], fsimeta.df)
con.df <- rbind(con.df, 
                fsicon.df[, -grep("MODULE_FSI|MODULE_POU|MODULE_DES", 
                                  colnames(fsicon.df))])

save(x = con.df, file = "./Data/Processed/Construction.RData")
save(x = meta.lst, file = "./Data/Processed/Metadata.RData")

###########################################################################
## End
###########################################################################
