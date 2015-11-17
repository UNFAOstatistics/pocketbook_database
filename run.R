###########################################################################
## This script generates the dataset for the Statistical Yearbooks
###########################################################################

root.dir <- "~/btsync/fao_sync/syb_database/"
setwd(root.dir)


###########################################################################
## Settings
###########################################################################


# Needed libraries --------------------------------------------------------

require(plyr)
require(dplyr)
require(reshape2)
require(data.table)
library(readr)


# Source functions --------------------------------------------------------
source("./code/misc_functions/CheckLogical.R")
source("./code/misc_functions/CheckValues.R")
source("./code/misc_functions/Sourcehttps.R")
source("./code/misc_functions/sum2.R")
source("./code/read_functions/ReadMetadata.R")


## -- Sourcings FAOSYBpackage from SYBdatabase folder. These used to sourced from Filippos github repo


###########################################################################
## User inputs
###########################################################################

# Country profile ---------------------------------------------------------

FAOcountryProfile <- read_csv("./data/FAOcountryProfile.csv")

# Construction and metadata files -----------------------------------------

# const <- read_csv("./data/Construction2015.csv")
# names(const)[names(const)=="COMMENTS"] <- "COMMENTS_construction"
# cat(paste(shQuote(names(const), type="cmd"), collapse=", "))
# 
# meta <- read_csv("./data/Metadata2015.csv")
# names(meta)[names(meta)=="COMMENTS"] <- "COMMENTS_metadata"
# f <- left_join(const,meta,by=)
# write.csv(f, file="data/construction_metadata.csv")

meta.lst <- ReadMetadata(file = "./data/construction_metadata.csv", encoding = "UTF-8", nrows = 665)
con.df <- meta.lst[["CONST"]]


#   ____                          _                    _   _____  _     ___   ____  _____ 
#  |  _ \   ___ __      __ _ __  | |  ___    __ _   __| | |  ___|/ \   / _ \ / ___||_   _|
#  | | | \ / _ \\ \ /\ / /| '_ \ | | / _ \  / _` | / _` | | |_  / _ \ | | | |\___ \  | |  
#  | |_| || (_) |\ V  V / | | | || || (_) || (_| || (_| | |  _|/ ___ \| |_| | ___) | | |  
#  |____/  \___/  \_/\_/  |_| |_||_| \___/  \__,_| \__,_| |_| /_/   \_\\___/ |____/  |_|  
#                                                                                         

# Download variables from FAOSTAT, parameters -----------------------------

faostatData.df <- meta.lst[["FAOSTAT"]]
dwnldFAO  <- FALSE # 

# Download variables from FAOSTAT --------------------------------------------

source("./code/FAOSTAT_functions/getFAOtoSYB.R")
source("./code/FAOSTAT_functions/getFAO.R")
source("./code/FAOSTAT_functions/printLab.R")

if (dwnldFAO) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df,
                  getFAOtoSYB(name = STS_ID, 
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              useCHMT = FALSE))
  FAO.df <- FAO.lst$aggregates; rm(FAO.lst)
  ## ...update list
  save(x = FAO.df, file = paste0("./data/processed/FAO", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./data/processed/FAO2015-07-10.RData")
}

#   ____                          _                    _  __        __ ____  
#  |  _ \   ___ __      __ _ __  | |  ___    __ _   __| | \ \      / /| __ ) 
#  | | | | / _ \\ \ /\ / /| '_ \ | | / _ \  / _` | / _` |  \ \ /\ / / |  _ \ 
#  | |_| || (_) |\ V  V / | | | || || (_) || (_| || (_| |   \ V  V /  | |_) |
#  |____/  \___/  \_/\_/  |_| |_||_| \___/  \__,_| \__,_|    \_/\_/   |____/ 
# 


# Download variables from WB ----------------------------------------------

# Download variables from WORLD BANK, parameters -----------------------------
downloadWB <- FALSE; CheckLogical(downloadWB)


worldbankData.df <- meta.lst[["WDI"]]

if (downloadWB) {
  ## Download data from WDI
  WB.lst <- with(worldbankData.df,
                 getWDItoSYB(indicator = WDINAME, name = STS_ID))
  ## ...update list
  save(x = WB.lst, file = paste0("./data/processed/WBdata", Sys.Date(), ".RData"))
} else {
  ## ...open list
  load(file = "./data/processed/WBdata2015-07-10.RData")
}
WB.df <- WB.lst$aggregates
WB.df <- WB.df[!duplicated(WB.df[c("ISO2_WB_CODE","Year")]),]
##
source("./code/FAOSTAT_functions/translateCountryCode.R")
FAOcountryProfile <- read.csv(url("https://raw.githubusercontent.com/mkao006/FAOSTATpackage/master/FAOcountryProfile.csv"))

WB.df <- translateCountryCode(data = WB.df, 
                              from = "ISO2_WB_CODE", to = "FAOST_CODE")
WB.df <- WB.df[, -grep("ISO2_WB_CODE|Country", colnames(WB.df))]
#rm(downloadWB); rm(WB.lst)

#    _                       _   __  __                              _ 
#   | |     ___    __ _   __| | |  \/  |  __ _  _ __   _   _   __ _ | |
#   | |    / _ \  / _` | / _` | | |\/| | / _` || '_ \ | | | | / _` || |
#   | |___| (_) || (_| || (_| | | |  | || (_| || | | || |_| || (_| || |
#   |_____|\___/  \__,_| \__,_| |_|  |_| \__,_||_| |_| \__,_| \__,_||_|
                                                                 

# Manual data -------------------------------------------------------------

# load("./data/processed/wbManualData.RData")
# load("./data/processed/AquastatManualData.RData")
# load("./data/processed/GlobalForestResourceAssessment.RData")
# load("./data/processed/BiofuelProduction.RData")
# load("./data/processed/Fishery.RData")
# load("./data/processed/UNPopManualData.RData")



###########################################################################
# 
# ____                                    _               
# |  _ \  _ __  ___    ___  ___  ___  ___ (_) _ __    __ _ 
# | |_) || '__|/ _ \  / __|/ _ \/ __|/ __|| || '_ \  / _` |
# | |__/ | |  | (_) || (__|  __/\__ \\__ \| || | | || (_| |
# |_|    |_|   \___/  \___|\___||___/|___/|_||_| |_| \__, |
#                                                    |___/ 
# 
###########################################################################

# Merging -----------------------------------------------------------------

initial.df = Reduce(function(x, y) merge(x, y, all = TRUE),
                    x = list(FAO.df # FAOSTAT, Population - Annual population
                             #WB.df, # Download variables from WB
                             #WBManualData.df, # not_update
                             #AquastatManualData.df, # updated 2015
                             #gfra.df, # not updated
                             #BiofuelProduction.df, # not update
                             #Fishery.df, # not updated  
                             #UNPopManualData.df), # added by Markus 20150401
                              ),
                    init = WB.df)
initial.df <- initial.df[initial.df[, "Year"] <= 2020,]

# Scale data to basic unit ------------------------------------------------
## -- Convert the characters formats as "thousand" into 1000

source("./code/FAOSTAT_functions/translateUnit.R")
source("./code/FAOSTAT_functions/scaleUnit.R")


meta.lst[["UNIT_MULT"]][, "UNIT_MULT"][meta.lst[["UNIT_MULT"]][, "UNIT_MULT"] == "NA"] <- NA
meta.lst[["UNIT_MULT"]][, "UNIT_MULT"] <- as.numeric(translateUnit(meta.lst[["UNIT_MULT"]][,"UNIT_MULT"]))

preConstr.df <- scaleUnit(initial.df, meta.lst[["UNIT_MULT"]])
rm(initial.df)

# Imputation --------------------------------------------------------------

# Construction ------------------------------------------------------------

## Manual

preConstr.df[, "MORTWEIGHT"] <- preConstr.df[, "SP.DYN.CBRT.IN"] * preConstr.df[, "OA.TPBS.POP.PPL.NO"] / 1000

# RF.FERT.NIPH.TN.NO ------------------------------------------------------

preConstr.df[, "RF.FERT.NIPH.TN.NO"] <- preConstr.df[, "RF.FERT.NI.TN.NO"] + preConstr.df[, "RF.FERT.PH.TN.NO"]

# RP.PEST.TOT.TN.NO -------------------------------------------------------
## NOTE (FILIPPO): we cannot do a simple sum of vector with the operator "+"
##                 because "+" as the function "sum" doesn't treat NA properly
RP.PEST.TOT.TN.NO.df <- 
  apply(preConstr.df[, c("RP.PEST.INS.TN.NO", "RP.PEST.MO.TN.NO",
                         "RP.PEST.HE.TN.NO", "RP.PEST.FB.TN.NO",
                         "RP.PEST.STF.TN.NO", "RP.PEST.STI.TN.NO")],
        MARGIN = 1, FUN = sum2)
preConstr.df[, "RP.PEST.TOT.TN.NO"] <- RP.PEST.TOT.TN.NO.df
rm(RP.PEST.TOT.TN.NO.df)

# QC.PRD.FRUNOGR.TN.NO ----------------------------------------------------

preConstr.df[, "QC.PRD.FRUNOGR.TN.NO"] <- preConstr.df[, "QC.PRD.FRU.TN.NO"] - preConstr.df[, "QD.PRD.WINE.TN.NO"]

# QC.RHRV.FRUNOGR.HA.NO ---------------------------------------------------

preConstr.df[, "QC.RHRV.FRUNOGR.HA.NO"] <-  preConstr.df[, "QC.RHRV.FRU.HA.NO"] - preConstr.df[, "QD.RHRV.VINE.HA.NO"]

# POP.TOT.BASE.0406 -------------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "OA.TPBS.POP.PPL.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "POP.TOT.BASE.0406"
base = base[, c("FAOST_CODE", "POP.TOT.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by = "FAOST_CODE", all.x = TRUE)

# QV.GPV.FOOD.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.GPV.FOOD.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.GPV.FOOD.BASE.0406"
base = base[, c("FAOST_CODE", "QV.GPV.FOOD.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.FOOD.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.FOOD.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.FOOD.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.FOOD.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.CRPS.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.CRPS.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.CRPS.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.CRPS.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.GPV.AGR.BASE.0406 ----------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.GPV.AGR.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.GPV.AGR.BASE.0406"
base = base[, c("FAOST_CODE", "QV.GPV.AGR.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.AGR.BASE.0406 ----------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.AGR.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.AGR.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.AGR.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.CRLS.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.CRLS.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.CRLS.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.CRLS.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.LVSTCK.BASE.0406 -------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.LVSTCK.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.LVSTCK.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.LVSTCK.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.NNFOOD.BASE.0406 -------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.NNFOOD.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.NNFOOD.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.NNFOOD.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# TP.NETVAL.CRLSPREP.USD.NO -----------------------------------------------

preConstr.df$TP.NETVAL.CRLSPREP.USD.NO = preConstr.df$TP.EXVAL.CRLSPREP.USD.NO - 
  preConstr.df$TP.IMVAL.CRLSPREP.USD.NO

# TP.NETVAL.MEATPREP.USD.NO -----------------------------------------------

preConstr.df$TP.NETVAL.MEATPREP.USD.NO = preConstr.df$TP.EXVAL.MEATPREP.USD.NO - 
  preConstr.df$TP.IMVAL.MEATPREP.USD.NO

# TP.NETVAL.FV.USD.NO -----------------------------------------------------

preConstr.df$TP.NETVAL.FV.USD.NO = preConstr.df$TP.EXVAL.FV.USD.NO - 
  preConstr.df$TP.IMVAL.FV.USD.NO

# TP.NETVAL.MILKEQ.USD.NO -------------------------------------------------

preConstr.df$TP.NETVAL.MILKEQ.USD.NO = preConstr.df$TP.EXVAL.MILKEQ.USD.NO - 
  preConstr.df$TP.IMVAL.MILKEQ.USD.NO

# TP.NETVAL.AFOVO.USD.NO --------------------------------------------------

preConstr.df$TP.NETVAL.AFOVO.USD.NO = preConstr.df$TP.EXVAL.ANFATS.USD.NO + 
  preConstr.df$TP.EXVAL.OILSEEDS.USD.NO + preConstr.df$TP.EXVAL.VEGOIL.USD.NO -
  preConstr.df$TP.IMVAL.ANFATS.USD.NO + preConstr.df$TP.IMVAL.OILSEEDS.USD.NO -
  preConstr.df$TP.IMVAL.VEGOIL.USD.NO

# TP.NETVAL.BEV.USD.NO ----------------------------------------------------

preConstr.df$TP.NETVAL.BEV.USD.NO = preConstr.df$TP.EXVAL.BEV.USD.NO - 
  preConstr.df$TP.IMVAL.BEV.USD.NO

# TP.NETVAL.CTCS.USD.NO ---------------------------------------------------

preConstr.df$TP.NETVAL.CTCS.USD.NO = preConstr.df$TP.EXVAL.CTCS.USD.NO - 
  preConstr.df$TP.IMVAL.CTCS.USD.NO

# TP.NETVAL.SUGHON.USD.NO -------------------------------------------------

preConstr.df$TP.NETVAL.SUGHON.USD.NO = preConstr.df$TP.EXVAL.SUGHON.USD.NO - 
  preConstr.df$TP.IMVAL.SUGHON.USD.NO

# GLI.CHPF.TOT.ECO2EQ.NO --------------------------------------------------

preConstr.df$GLI.CHPF.TOT.ECO2EQ.NO = preConstr.df$GL.CL.TOT.NERCO2EQ.NO + 
  preConstr.df$GL.GL.TOT.NERCO2EQ.NO

# GHG.AFOLU.TOT.ECO2EQ.NO -------------------------------------------------

preConstr.df$GHG.AFOLU.TOT.ECO2EQ.NO = preConstr.df$GHG.TOT.ALL.GG.NO + 
  preConstr.df$GL.LU.TOT.NERCO2EQ.NO

# GN.UI.EA.TJPIN.NO -------------------------------------------------------

preConstr.df[, "GN.UI.EA.TJPIN.NO"] <- 
  preConstr.df[, "GN.TE.CIA.TJ.NO"]/preConstr.df[, "QV.GPV.AGR.ID.NO"]*1000000

# FI.PRD.TOT.TN.NO --------------------------------------------------------

# preConstr.df$FI.PRD.TOT.TN.NO = preConstr.df$FI.PRD.AQ.TN.NO + 
#   preConstr.df$FI.PRD.CAPT.TN.NO

# FI.NETVAL.FISH.USD.NO ---------------------------------------------------

# preConstr.df$FI.NETVAL.FISH.USD.NO = preConstr.df$FI.EXVAL.FISH.USD.NO - 
#   preConstr.df$FI.IMVAL.FISH.USD.NO

# TP.EXVAL.FOODWF.USD.NO --------------------------------------------------

# preConstr.df$TP.EXVAL.FOODWF.USD.NO = preConstr.df$TP.EXVAL.FOOD.USD.NO + 
#   preConstr.df$FI.EXVAL.FISH.USD.NO

# TP.IMVAL.FOODWF.USD.NO --------------------------------------------------

# preConstr.df$TP.IMVAL.FOODWF.USD.NO = preConstr.df$TP.IMVAL.FOOD.USD.NO + 
#   preConstr.df$FI.IMVAL.FISH.USD.NO

# Clean the environment ---------------------------------------------------
rm(list = "base")

source("./code/FAOSTAT_functions/constructSYB.R")
source("./code/FAOSTAT_functions/shConstruct.R")
source("./code/FAOSTAT_functions/chConstruct.R")
source("./code/FAOSTAT_functions/grConstruct.R")
source("./code/FAOSTAT_functions/geogr.R")
source("./code/FAOSTAT_functions/chgr.R")

## Automatic
# f <- con.df[con.df[, "CONSTRUCTION_TYPE"] %in% c("share", "growth", "change", "index"),] # leave the manual construction outside
# 
# constructSYB(data = preConstr.df,
#              origVar1 = f$STS_ID_CONSTR1,
#              origVar2 = f$STS_ID_CONSTR2,
#              newVarName = f$STS_ID,
#              constructType = f$CONSTRUCTION_TYPE,
#              grFreq = f$GROWTH_RATE_FREQ,
#              grType = f$GROWTH_TYPE, 
#              baseYear = 2000)

preConstr.df <- filter(preConstr.df, FAOST_CODE <= 351)
preConstr.df <- filter(preConstr.df, Year >= 1990)
preConstr.df <- filter(preConstr.df, Year <= 2015)
preConstr.df <- preConstr.df[!duplicated(preConstr.df[c("FAOST_CODE","Year")]),]
# preConstr.df$Year <- as.numeric(preConstr.df$Year)
# preConstr.df$FAOST_CODE <- as.numeric(preConstr.df$FAOST_CODE)

fc <- data.frame(FAOST_CODE = unique(preConstr.df$FAOST_CODE))
fc <- left_join(fc,FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME")]) 


source("./code/read_functions/ReadConstruction.R")
con.df <- ReadConstruction(file = "./data/Construction2015.csv", encoding = "UTF-8", nrows = 665)

postConstr.lst <- with(con.df[con.df[, "CONSTRUCTION_TYPE"] %in% c("share", "growth", "change", "index"),], # leave the manual construction outside,
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
source("./code/aggregate_functions/CountryAggregation.R")

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
save(x = SYB.df, file = "./data/processed/SYB_pre_fsi.RData")
load(file = "./data/processed/SYB_pre_fsi.RData")


# Merge the FSI dataset ---------------------------------------------------

## Merge the dataset
load(file = "./data/processed/fsi.RData")
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

save(x = SYB.df, file = "./data/processed/SYB.RData")

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

save(x = con.df, file = "./data/processed/Construction.RData")
save(x = meta.lst, file = "./data/processed/Metadata.RData")

###########################################################################
## End
###########################################################################
