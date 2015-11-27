#!/usr/bin/env r
t1 <- Sys.time()
###########################################################################
## This script generates the dataset for the Statistical Yearbooks
###########################################################################

root.dir <- "~/btsync/faosync/syb_database/"
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
library(RJSONIO)

library(FAOSTAT)

# Source functions --------------------------------------------------------
source("./code/misc_functions/CheckLogical.R")
source("./code/misc_functions/CheckValues.R")
source("./code/misc_functions/Sourcehttps.R")
source("./code/misc_functions/sum2.R")
source("./code/read_functions/ReadMetadata.R")
source("./code/read_functions/ReadConstruction.R")


# source("./code/FAOSTAT_functions/chConstruct.R")
# source("./code/FAOSTAT_functions/chgr.R")
# source("./code/FAOSTAT_functions/constructSYB.R")
# source("./code/FAOSTAT_functions/geogr.R")
# source("./code/FAOSTAT_functions/getFAO.R")
# source("./code/FAOSTAT_functions/getFAOtoSYB.R")
# source("./code/FAOSTAT_functions/getWDI.R")
# source("./code/FAOSTAT_functions/getWDImetaData.R")
# source("./code/FAOSTAT_functions/getWDItoSYB.R")
# source("./code/FAOSTAT_functions/grConstruct.R")
# source("./code/FAOSTAT_functions/printLab.R")
# source("./code/FAOSTAT_functions/scaleUnit.R")
# source("./code/FAOSTAT_functions/shConstruct.R")
# source("./code/FAOSTAT_functions/translateCountryCode.R")
# source("./code/FAOSTAT_functions/translateUnit.R")
# 
# source("./code/FAOSTAT_functions//FAOSTAT_functions/Aggregation.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/CHMT.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/ebind.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/FAOcheck.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/FAOcountryProfile.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/FAOmetaTable.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/FAOregionProfile.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/FAOsearch.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/FAOSTAT-package.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/fillCountryCode.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/indConstruct.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/lsgr.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/mergeSYB.R")
# source("./code/FAOSTAT_functions//FAOSTAT_functions/overlap.R")
# 
# 
# library(RJSONIO)
# library(MASS)
# library(classInt)
# library(labeling)
# library(scales)



## -- Sourcings FAOSYBpackage from SYBdatabase folder. These used to sourced from Filippos github repo


###########################################################################
## User inputs
###########################################################################

# Country profile ---------------------------------------------------------

FAOcountryProfile <- read.csv("~/btsync/faosync/pocketbooks/regional15/input/data/FAOcountryProfile.csv", stringsAsFactors = FALSE)


# Recode the Short Name Variables
## Abbreviate names
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Latin America and the Caribbean"          & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Latin America\nand the Caribbean"
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Developed countries"                      & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developed\ncountries"
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Developing countries"                     & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developing\ncountries"

FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Vincent and the Grenadines"             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Vincent\nand the\nGrenadines"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Antigua and Barbuda"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Antigua and\nBarbuda"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Trinidad and Tobago"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Trinidad and\nTobago"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Republic of Moldova"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Republic of\nMoldova"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Helena, Ascension and Tristan da Cunha" & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Helena"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Northern Mariana Islands"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "N. Mariana\nIslands"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Wallis and Futuna Islands"                    & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Wallis and\nFutuna Is."
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United Arab Emirates"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "United Arab\nEmirates"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Turks and Caicos Islands"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Turks and\nCaicos Is."
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Central African Republic"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Central African\nRepublic"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Sao Tome and Principe"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Sao Tome and\nPrincipe"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United States of America"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "United States\nof America"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Iran (Islamic Republic of)"                   & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Iran\n(Islamic Republic of)"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Bosnia and Herzegovina"                       & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Bosnia and\nHerzegovina"
# FAOcountryProfile["SHORT_NAME"][FAOcountryProfile[, "FAOST_CODE"] == 107]                                                                                        <- "Côte d'Ivoire"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Falkland Islands (Malvinas)"                  & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Falkland Islands\n(Malvinas)"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Papua New Guinea"                             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Papua New\nGuinea"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "American Samoa"                               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "American\nSamoa"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Western Sahara"                               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Western\nSahara"

FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Union of Soviet Socialist Republic"           & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Soviet Union"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Micronesia (Federated States of)"             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Micronesia"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Svalbard and Jan Mayen Islands"               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Svalbard"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Occupied Palestinian Territory"               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Occupied\nPalestinian Territory"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United States Virgin Islands"                 & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "U.S. Virgin Islands"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Pierre and Miquelon"                    & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Pierre\nand Miquelon"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Serbia and Montenegro"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Serbia and\nMontenegro"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Kitts and Nevis"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Kitts\nand Nevis"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Netherlands Antilles"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Netherlands\nAntilles"
#FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == ""                                            & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- ""

# Source script from yearbook processing
run_only_regions <- TRUE
source('~/btsync/faosync/pocketbooks/regional15/input/code/define_regions.R')


# RAF
FAOcountryProfile$FAO_RAF_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF"]]),]$FAOST_CODE,                     "RAFregion",         FAOcountryProfile$FAO_RAF_REG)
## subregs
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Central_Africa"]]),]$FAOST_CODE,  "RAFCentralAfrica",  FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Eastern_Africa"]]),]$FAOST_CODE,  "RAFEastAfrica",     FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Northern_Africa"]]),]$FAOST_CODE, "RAFNorthAfrica",    FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Southern_Africa"]]),]$FAOST_CODE, "RAFSouthernAfrica", FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Western_Africa"]]),]$FAOST_CODE,  "RAFWestAfrica",     FAOcountryProfile$FAO_RAF_SUB_REG)

# RAP
FAOcountryProfile$FAO_RAP_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP"]]),]$FAOST_CODE,                               "RAPregion",              FAOcountryProfile$FAO_RAP_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Central_Asia"]]),]$FAOST_CODE,              "RAPCentralAsia",         FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Eastern_Asia"]]),]$FAOST_CODE,              "RAPEastAsia",            FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_South_Eastern_Asia"]]),]$FAOST_CODE,        "RAPSoutheastAsia",       FAOcountryProfile$FAO_RAP_SUB_REG)
## New ones
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Western_Asia"]]),]$FAOST_CODE,              "RAPWesternAsia",         FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Southern_Asia"]]),]$FAOST_CODE,             "RAPSouthernAsia",        FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Austriala_and_New_Zealand"]]),]$FAOST_CODE, "RAPAustraliaNewZealand", FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Melanesia"]]),]$FAOST_CODE,                 "RAPMelanesia",           FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Micronesia"]]),]$FAOST_CODE,                "RAPMicronesia",          FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Polynesia"]]),]$FAOST_CODE,                 "RAPPolynesia",           FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Russian_Federation"]]),]$FAOST_CODE,        "RAPRussianFederation",   FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_France"]]),]$FAOST_CODE,                    "RAPFrance",              FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_United_States"]]),]$FAOST_CODE,             "RAPUnitedStates",        FAOcountryProfile$FAO_RAP_SUB_REG)

# REU
FAOcountryProfile$FAO_REU_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU"]]),]$FAOST_CODE,                            "REUregion",               FAOcountryProfile$FAO_REU_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_South_Eastern_Europe"]]),]$FAOST_CODE,   "REUSouthEasternEurope",   FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_EU_other_and_EFTA"]]),]$FAOST_CODE,      "REUOtherAndEFTA",         FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_Caucasus_and_Turkey"]]),]$FAOST_CODE,    "REUCaucAndTurkey",        FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_CIS_Europe"]]),]$FAOST_CODE,             "REUCISeurope",            FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_EU_Central_and_Eastern"]]),]$FAOST_CODE, "REUCentralEasternEurope", FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_Israel"]]),]$FAOST_CODE,                 "REUIsrael",               FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_Central_Asia"]]),]$FAOST_CODE,           "REUIsrael",               FAOcountryProfile$FAO_REU_SUB_REG)

# RNE
FAOcountryProfile$FAO_RNE_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RNE"]]),]$FAOST_CODE,                                               "RNEregion", FAOcountryProfile$FAO_RNE_REG)
FAOcountryProfile$FAO_RNE_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RNE_Gulf_Cooperation_Council_States_and_Yemen"]]),]$FAOST_CODE, "RNEgccsy",  FAOcountryProfile$FAO_RNE_SUB_REG)
FAOcountryProfile$FAO_RNE_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RNE_Other_Near_East_countries"]]),]$FAOST_CODE,                 "RNEome",    FAOcountryProfile$FAO_RNE_SUB_REG)
FAOcountryProfile$FAO_RNE_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RNE_North_Africa"]]),]$FAOST_CODE,                              "RNEna",     FAOcountryProfile$FAO_RNE_SUB_REG)


# Construction and metadata files -----------------------------------------
con.df <- ReadConstruction(file = "./input_data/Construction2015.csv", 
                           encoding = "UTF-8")
# save(x = con.df, file = "./Data/Processed/Construction.RData")
meta.lst <- ReadMetadata(file = "./input_data/Metadata2015.csv", 
                         encoding = "UTF-8")
# save(x = meta.lst, file = "./Data/Processed/Metadata.RData")
# con.df <- meta.lst[["CONST"]]




# Create a new folder for today
date <- paste(Sys.Date(),format(Sys.time(), "%H"),sep="-")
if (!file.exists(paste0("./output_data/",date))) dir.create(paste0("./output_data/",date))

#   ____                          _                    _   _____  _     ___   ____  _____ 
#  |  _ \   ___ __      __ _ __  | |  ___    __ _   __| | |  ___|/ \   / _ \ / ___||_   _|
#  | | | \ / _ \\ \ /\ / /| '_ \ | | / _ \  / _` | / _` | | |_  / _ \ | | | |\___ \  | |  
#  | |_| || (_) |\ V  V / | | | || || (_) || (_| || (_| | |  _|/ ___ \| |_| | ___) | | |  
#  |____/  \___/  \_/\_/  |_| |_||_| \___/  \__,_| \__,_| |_| /_/   \_\\___/ |____/  |_|  
#                                                                                         


###########################################################################
## Data collection
###########################################################################

# Download variables from FAOSTAT, parameters -----------------------------

faostatData.df <- meta.lst[["FAOSTAT"]]
dwnldOA <- FALSE # Population
dwnldRL <- TRUE # Resources, Resources - Land
dwnldRF <- TRUE # Resources - Fertilizers
dwnldRP <- TRUE # Resources - Pesticides
dwnldCS <- TRUE # Investments - Capital stock
dwnldRM <- TRUE # Investments - Machinery
dwnldIG <- TRUE # Government expenditures
dwnldA <- TRUE # ASTI
dwnldQC <- TRUE # Production - Crops
dwnldQA <- TRUE # Production - Live animals
dwnldQD <- TRUE # Production - Crops processed
dwnldQL <- TRUE # Production - Livestock primary
dwnldQP <- TRUE # Production - Livestock processed
dwnldQV <- TRUE # Production - Value of agricultural production
dwnldQI <- TRUE # Production indices
dwnldTP <- TRUE # Trade - Crops and livestock products
dwnldTI <- TRUE # Trade - Trade indices
dwnldFO <- TRUE # Forestry
dwnldGHG <- TRUE # Greenhouse gases
dwnldFB <- TRUE # Food balance sheets
dwnldCOF <- TRUE # Coffeebook indicators

downloadWB <- TRUE; CheckLogical(downloadWB)


replication_date <- "2015-11-25-11"

if (!file.exists(paste0("./output_data/",date))) dir.create(paste0("./output_data/",date))

# FAOSTAT, Population - Annual population ---------------------------------

if (dwnldOA) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "OA",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOoa.df <- FAO.lst$entity; rm(dwnldOA); rm(FAO.lst)
  ## ...update list
  save(x = FAOoa.df, file = paste0("./output_data/",date,"/FAOoa", date, ".RData"))
} else {
  ## ...open list
  load(file = "./output_data/2015-10-08/FAOoa2015-08-18.RData") # to get the pop in employment..
  # load(file = paste0("./output_data/",replication_date,"/FAOoa",replication_date,".RData"))
}

# FAOSTAT, Resources - Land -----------------------------------------------

if (dwnldRL) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RL",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrl.df <- FAO.lst$entity; rm(dwnldRL); rm(FAO.lst)
  ## ...update list
  save(x = FAOrl.df, file = paste0("./output_data/",date,"/FAOrl", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOrl2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOrl",replication_date,".RData"))
}

# FAOSTAT, Resources - Fertilizers ----------------------------------------

if (dwnldRF) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RF",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrf.df <- FAO.lst$entity; rm(dwnldRF); rm(FAO.lst)
  ## ...update list
  save(x = FAOrf.df, file = paste0("./output_data/",date,"/FAOrf", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOrf2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOrf",replication_date,".RData"))
}

# FAOSTAT, Resources - Pesticides -----------------------------------------

if (dwnldRP) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] %in% c("RP","EP"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrp.df <- FAO.lst$entity; rm(dwnldRP); rm(FAO.lst)
  ## ...update list
  save(x = FAOrp.df, file = paste0("./output_data/",date,"/FAOrp", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOrp2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOrp",replication_date,".RData"))
}

# FAOSTAT, Investments - Capital stock ------------------------------------

if (dwnldCS) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "CS",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOcs.df <- FAO.lst$entity; rm(dwnldCS); rm(FAO.lst)
  ## ...update list
  save(x = FAOcs.df, file = paste0("./output_data/",date,"/FAOcs", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOcs2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOcs",replication_date,".RData"))
}

# FAOSTAT, Investments - Machinery ----------------------------------------

if (dwnldRM) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "RM",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOrm.df <- FAO.lst$entity; rm(dwnldRM); rm(FAO.lst)
  ## ...update list
  save(x = FAOrm.df, file = paste0("./output_data/",date,"/FAOrm", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOrm2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOrm",replication_date,".RData"))
}

# FAOSTAT, Government expenditures ----------------------------------------

if (dwnldIG) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "IG",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOig.df <- FAO.lst$entity; rm(dwnldIG); rm(FAO.lst)
  ## ...update list
  save(x = FAOig.df, file = paste0("./output_data/",date,"/FAOig", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOig2014-12-05.RData") # so the last time this was built succesfully
  load(file = paste0("./output_data/",replication_date,"/FAOig",replication_date,".RData"))
}

# FAOSTAT, ASTI -----------------------------------------------------------

if (dwnldA) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] %in% c("AA", "AE", "AR"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOa.df <- FAO.lst$entity; rm(dwnldA); rm(FAO.lst)
  ## ...update list
  save(x = FAOa.df, file = paste0("./output_data/",date,"/FAOa", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOa2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOa",replication_date,".RData"))
}

# FAOSTAT, Production - Crops ---------------------------------------------

if (dwnldQC) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QC",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqc.df <- FAO.lst$entity; rm(dwnldQC); rm(FAO.lst)
  ## ...update list
  save(x = FAOqc.df, file = paste0("./output_data/",date,"/FAOqc", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOqc2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOqc",replication_date,".RData"))
}

# FAOSTAT, Production - Live animals --------------------------------------

if (dwnldQA) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QA",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqa.df <- FAO.lst$entity; rm(dwnldQA); rm(FAO.lst)
  ## ...update list
  save(x = FAOqa.df, file = paste0("./output_data/",date,"/FAOqa", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOqa2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOqa",replication_date,".RData"))
}

# FAOSTAT, Production - Crops processed -----------------------------------

if (dwnldQD) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QD",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqd.df <- FAO.lst$entity; rm(dwnldQD); rm(FAO.lst)
  ## ...update list
  save(x = FAOqd.df, file = paste0("./output_data/",date,"/FAOqd", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOqd2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOqd",replication_date,".RData"))
}

# FAOSTAT, Production - Livestock primary ---------------------------------

if (dwnldQL) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QL",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOql.df <- FAO.lst$entity; rm(dwnldQL); rm(FAO.lst)
  ## ...update list
  save(x = FAOql.df, file = paste0("./output_data/",date,"/FAOql", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOql2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOql",replication_date,".RData"))
}

# FAOSTAT, Production - Livestock processed -------------------------------

if (dwnldQP) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QP",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqp.df <- FAO.lst$entity; rm(dwnldQP); rm(FAO.lst)
  ## ...update list
  save(x = FAOqp.df, file = paste0("./output_data/",date,"/FAOqp", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOqp2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOqp",replication_date,".RData"))
}

# FAOSTAT, Production - Value of agricultural production ------------------

if (dwnldQV) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QV",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqv.df <- FAO.lst$entity; rm(dwnldQV); rm(FAO.lst)
  ## ...update list
  save(x = FAOqv.df, file = paste0("./output_data/",date,"/FAOqv", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOqv2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOqv",replication_date,".RData"))
}

# FAOSTAT, Production - Production indices --------------------------------

if (dwnldQI) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "QI",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOqi.df <- FAO.lst$entity; rm(dwnldQI); rm(FAO.lst)
  ## ...update list
  save(x = FAOqi.df, file = paste0("./output_data/",date,"/FAOqi", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOqi2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOqi",replication_date,".RData"))
}

# FAOSTAT, Trade - Crops and livestock products ---------------------------

if (dwnldTP) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "TP",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOtp.df <- FAO.lst$entity; rm(dwnldTP); rm(FAO.lst)
  ## ...update list
  save(x = FAOtp.df, file = paste0("./output_data/",date,"/FAOtp", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOtp2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOtp",replication_date,".RData"))
}

# FAOSTAT, Trade - Trade indices ------------------------------------------

if (dwnldTI) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "TI",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOti.df <- FAO.lst$entity; rm(dwnldTI); rm(FAO.lst)
  ## ...update list
  save(x = FAOti.df, file = paste0("./output_data/",date,"/FAOti", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOti2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOti",replication_date,".RData"))
}

# FAOSTAT, Forestry -------------------------------------------------------

if (dwnldFO) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "FO",],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOfo.df <- FAO.lst$entity; rm(dwnldFO); rm(FAO.lst)
  ## ...update list
  save(x = FAOfo.df, file = paste0("./output_data/",date,"/FAOfo", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOfo2015-10-08.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOfo",replication_date,".RData"))
}

# FAOSTAT, Greenhouse gases -----------------------------------------------

if (dwnldGHG) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] %in% c("GT", "GF", "GL", "GN", "GI"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOghg.df <- FAO.lst$entity; rm(dwnldGHG); rm(FAO.lst)
  for (i in 3:NCOL(FAOghg.df)) {
    FAOghg.df[, i] <- as.numeric(FAOghg.df[, i])
  }
  ## ...update list
  save(x = FAOghg.df, file = paste0("./output_data/",date,"/FAOghg", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOghg2015-10-08.RData") # use this to get the data for forest net-emissions!
  load(file = paste0("./output_data/",replication_date,"/FAOghg",replication_date,".RData"))
}

# FAOSTAT, Food balance sheets --------------------------------------------
# if (dwnldOA) {
#   ## Download data from FAOSTAT
#   FAO.lst <- with(faostatData.df[faostatData.df[, "SQL_DOMAIN_CODE"] == "OA",],
#                   getFAOtoSYB(name = STS_ID,
#                               domainCode = SQL_DOMAIN_CODE,
#                               elementCode = SQL_ELEMENT_CODE,
#                               itemCode = SQL_ITEM_CODE,
#                               # productionDB = FALSE,
#                               useCHMT = FALSE))
#   FAOoa.df <- FAO.lst$entity; rm(dwnldOA); rm(FAO.lst)
#   ## ...update list
#   save(x = FAOoa.df, file = paste0("./output_data/",date,"/FAOoa", date, ".RData"))
# } else {
#   ## ...open list
#   load(file = "./output_data/2015-10-08/FAOoa2015-10-08.RData")
# }

# FAOSTAT, Coffee - Coffee book variables ---------------------------------
if (dwnldCOF) {
  ## Download data from FAOSTAT
  FAO.lst <- with(faostatData.df[faostatData.df[, "TOPIC"] %in% c("coffee"),],
                  getFAOtoSYB(name = STS_ID,
                              domainCode = SQL_DOMAIN_CODE,
                              elementCode = SQL_ELEMENT_CODE,
                              itemCode = SQL_ITEM_CODE,
                              # productionDB = FALSE,
                              useCHMT = FALSE))
  FAOcof.df <- FAO.lst$entity; rm(dwnldFB); rm(FAO.lst)
  for (i in 3:NCOL(FAOcof.df)) {
    FAOcof.df[, i] <- as.numeric(FAOcof.df[, i])
  }
  ## ...update list
  save(x = FAOcof.df, file = paste0("./output_data/",date,"/FAOcof", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/FAOcof2015-10-13.RData")
  load(file = paste0("./output_data/",replication_date,"/FAOcof",replication_date,".RData"))
}


#   ____                          _                    _  __        __ ____  
#  |  _ \   ___ __      __ _ __  | |  ___    __ _   __| | \ \      / /| __ ) 
#  | | | | / _ \\ \ /\ / /| '_ \ | | / _ \  / _` | / _` |  \ \ /\ / / |  _ \ 
#  | |_| || (_) |\ V  V / | | | || || (_) || (_| || (_| |   \ V  V /  | |_) |
#  |____/  \___/  \_/\_/  |_| |_||_| \___/  \__,_| \__,_|    \_/\_/   |____/ 
# 

# Download variables from WB ----------------------------------------------


if (downloadWB) {
  ## Download data from WDI
  WB.lst1 <- with(meta.lst[["WDI"]][1:40,],
                  getWDItoSYB(indicator = WDINAME, name = STS_ID))
  ## ...update list
  save(x = WB.lst1, file = paste0("./output_data/",date,"/WBdata1", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/WBdata12015-11-05.RData")
  load(file = paste0("./output_data/",replication_date,"/WBdata1",replication_date,".RData"))
}
if (downloadWB) {
  ## Download data from WDI
  WB.lst2 <- with(meta.lst[["WDI"]][41:75,],
                  getWDItoSYB(indicator = WDINAME, name = STS_ID))
  ## ...update list
  save(x = WB.lst2, file = paste0("./output_data/",date,"/WBdata2", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/WBdata22015-11-05.RData")
  load(file = paste0("./output_data/",replication_date,"/WBdata2",replication_date,".RData"))
}
if (downloadWB) {
  ## Download data from WDI
  WB.lst3 <- with(meta.lst[["WDI"]][76:nrow(meta.lst[["WDI"]]),],
                  getWDItoSYB(indicator = WDINAME, name = STS_ID))
  ## ...update list
  save(x = WB.lst3, file = paste0("./output_data/",date,"/WBdata3", date, ".RData"))
} else {
  ## ...open list
  # load(file = "./output_data/2015-10-08/WBdata32015-11-05.RData")
  load(file = paste0("./output_data/",replication_date,"/WBdata3",replication_date,".RData"))
}
WB.df1 <- WB.lst1$entity
WB.df2 <- WB.lst2$entity
WB.df3 <- WB.lst3$entity

WB.df <- dplyr::left_join(WB.df1,WB.df2)
WB.df <- dplyr::left_join(WB.df,WB.df3)

WB.df <- WB.df[!duplicated(WB.df[c("ISO2_WB_CODE","Year")]),]
##



WB.df <- translateCountryCode(data = WB.df, 
                              from = "ISO2_WB_CODE", to = "FAOST_CODE")
WB.df <- WB.df[, -grep("ISO2_WB_CODE|Country", colnames(WB.df))]

# Markus fixes just to get thing working
WB.df <- WB.df[WB.df$FAOST_CODE < 400,]
WB.df <- WB.df[!is.na(WB.df$FAOST_CODE),]




  # save(x = FAO.df, file = paste0("./output_data/",date,"/FAO.RData"))
  # load(file = "./output_data/2015-11-17/FAO.RData")


# Manual data -------------------------------------------------------------

load("./input_data/processed/wbManualData.RData") #
load("./input_data/processed/AquastatManualData.RData") #
load("./input_data/processed/GlobalForestResourceAssessment.RData") #
load("./input_data/processed/BiofuelProduction2015-11-20.RData") #
# 
load("./input_data/processed/Fishery.RData") #
load("./input_data/processed/UNPopManualData.RData") #

# New indicators for pocketbook project!
# forestry assesments
load("./input_data/processed/fra2015.RData") #
load("./input_data/processed/regional1.RData") #
load("./input_data/processed/prod_ind_2015.RData") #
load("./input_data/processed/df_area_harvested.RData") #
load("./input_data/processed/fish2015.RData") #
load("./input_data/processed/fertilizers.RData") #
load("./input_data/processed/prod_ind_weights.RData") #
load("./input_data/processed/oda_brian_2015.RData")


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
                             # FAOfb.df, # FAOSTAT, Food balance sheets
                             FAOcof.df, # FAOSTAT, coffee
                             WBManualData.df, # not_update
                             AquastatManualData.df, # updated 2015
                             fra2015.df,
                             regional1.df,
                             prod_ind_2015.df,
                             df_area_harvested.df,
                             fish2015.df,
                             fertilizers.df,
                             prod_ind_weights.df,
                             oda_brian_2015.df,
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
initial.df <- initial.df[initial.df[, "FAOST_CODE"] <= 400,]


# Scale data to basic unit ------------------------------------------------
## -- Convert the characters formats as "thousand" into 1000

meta.lst[["UNIT_MULT"]][, "UNIT_MULT"] <- as.numeric(translateUnit(meta.lst[["UNIT_MULT"]]$UNIT_MULT))


preConstr.df <- scaleUnit(initial.df, meta.lst[["UNIT_MULT"]])
rm(initial.df)

## Manual Construction

###########################################################################
## This script is used to manually construct some variables
###########################################################################

# MORTWEIGHT --------------------------------------------------------------

preConstr.df[, "MORTWEIGHT"] <- 
  preConstr.df[, "SP.DYN.CBRT.IN"] * preConstr.df[, "OA.TPBS.POP.PPL.NO"] / 1000

# # LABPARTFEWEIGHT ---------------------------------------------------------
# 
# preConstr.df[, "LABPARTFEWEIGHT"] <- 
#   preConstr.df[, "SP.POP.1564.FE.IN"] + preConstr.df[, "SP.POP.65UP.FE.IN"]
# 
# # LABPARTMAWEIGHT ---------------------------------------------------------
# 
# preConstr.df[, "LABPARTMAWEIGHT"] <- 
#   preConstr.df[, "SP.POP.1564.MA.IN"] + preConstr.df[, "SP.POP.65UP.MA.IN"]

# RF.FERT.NIPH.TN.NO ------------------------------------------------------

preConstr.df[, "RF.FERT.NIPH.TN.NO"] <- 
  preConstr.df[, "RF.FERT.NI.TN.NO"] + preConstr.df[, "RF.FERT.PH.TN.NO"]

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

preConstr.df[, "QC.PRD.FRUNOGR.TN.NO"] <- 
  preConstr.df[, "QC.PRD.FRU.TN.NO"] - preConstr.df[, "QD.PRD.WINE.TN.NO"]

# QC.RHRV.FRUNOGR.HA.NO ---------------------------------------------------

preConstr.df[, "QC.RHRV.FRUNOGR.HA.NO"] <- 
  preConstr.df[, "QC.RHRV.FRU.HA.NO"] - preConstr.df[, "QD.RHRV.VINE.HA.NO"]

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

# vegetable oils
base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "prod_val_vegoil")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "prod_val_vegoil.BASE.0406"
base = base[, c("FAOST_CODE", "prod_val_vegoil.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# Roots and tubers weights
base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "prod_val_roots")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "prod_val_roots.BASE.0406"
base = base[, c("FAOST_CODE", "prod_val_roots.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)


# Fruits and vegetables
base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "prod_val_vege")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "prod_val_vege.BASE.0406"
base = base[, c("FAOST_CODE", "prod_val_vege.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# Milk
base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "prod_val_milk")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "prod_val_milk.BASE.0406"
base = base[, c("FAOST_CODE", "prod_val_milk.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# Meat
base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "prod_val_meat")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "prod_val_meat.BASE.0406"
base = base[, c("FAOST_CODE", "prod_val_meat.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# sugar
base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "prod_val_sugar")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "prod_val_sugar.BASE.0406"
base = base[, c("FAOST_CODE", "prod_val_sugar.BASE.0406")]
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

# preConstr.df$GLI.CHPF.TOT.ECO2EQ.NO = preConstr.df$GL.CL.TOT.NERCO2EQ.NO + 
#   preConstr.df$GL.GL.TOT.NERCO2EQ.NO

# GHG.AFOLU.TOT.ECO2EQ.NO -------------------------------------------------

preConstr.df$GHG.AFOLU.TOT.ECO2EQ.NO = preConstr.df$GHG.TOT.ALL.GG.NO + 
  preConstr.df$GL.LU.TOT.NERCO2EQ.NO

# GN.UI.EA.TJPIN.NO -------------------------------------------------------

preConstr.df[, "GN.UI.EA.TJPIN.NO"] <- 
  preConstr.df[, "GN.TE.CIA.TJ.NO"]/preConstr.df[, "QV.GPV.AGR.ID.NO"]*1000000

# FI.PRD.TOT.TN.NO --------------------------------------------------------

preConstr.df$FI.PRD.TOT.TN.NO = preConstr.df$FI.PRD.AQ.TN.NO + 
  preConstr.df$FI.PRD.CAPT.TN.NO

# FI.NETVAL.FISH.USD.NO ---------------------------------------------------

preConstr.df$FI.NETVAL.FISH.USD.NO = preConstr.df$FI.EXVAL.FISH.USD.NO - 
  preConstr.df$FI.IMVAL.FISH.USD.NO

# TP.EXVAL.FOODWF.USD.NO --------------------------------------------------

preConstr.df$TP.EXVAL.FOODWF.USD.NO = preConstr.df$TP.EXVAL.FOOD.USD.NO + 
  preConstr.df$FI.EXVAL.FISH.USD.NO

# TP.IMVAL.FOODWF.USD.NO --------------------------------------------------

preConstr.df$TP.IMVAL.FOODWF.USD.NO = preConstr.df$TP.IMVAL.FOOD.USD.NO + 
  preConstr.df$FI.IMVAL.FISH.USD.NO

# Clean the environment ---------------------------------------------------

# rm(list = "base")


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

preAgg.df <- preAgg.df[preAgg.df$FAOST_CODE <= 351,]

# preAgg.df <- preAgg.df[!duplicated(preAgg.df[c("FAOST_CODE","Year")]),]

# Adjustment in scaling ---------------------------------------------------

manScalVars <- subset(con.df, select = c("STS_ID", "SCALING"), subset = !is.na(SCALING))
#manScalVars <- con.df[!(is.na(con.df[["SCALING"]])), c("STS_ID", "SCALING")] # Markus fix

###########################
# Commenting this out as does not work..

# for (i in 1:NROW(manScalVars)) {
#   preAgg.df[, manScalVars[i, "STS_ID"]] <- preAgg.df[, manScalVars[i, "STS_ID"]] * manScalVars[i, "SCALING"]
# }
# rm(list = c("manScalVars", "i"))

#####################################

# Aggregations ------------------------------------------------------------
# source("./Rcode/Final/aggregate_functions/EconomicAggregates.R")
# source("./Rcode/Final/aggregate_functions/FAOAggregates.R")
# source("./Rcode/Final/aggregate_functions/SofiAggregates.R")


## Country aggregation
source("./code/aggregate_functions/CountryAggregation.R")

## China aggregation
source("./code/aggregate_functions/ChinaAggregates.R")

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
  source("./code/aggregate_functions/M49aggregates.R")
} else load()

## FAO aggregates
# Sourcehttps(source("./Rcode/Final/ComplementaryScripts/FAOAggregates.R")
source("./code//aggregate_functions/FAOAggregates.R")

# aja tänne asti!!
## Economic aggregates
# Sourcehttps(source("./Rcode/Final/ComplementaryScripts/EconomicAggregates.R")
## rbind the datasets

postAgg.df <- rbind(country.df, M49.df,FAOregions.df)

# postAgg.df <- rbind(country.df, M49.df)
# rm(list = c("country.df", "M49.df")) # because M49.df takes a loooooong time to run

# Check for NaN, Inf, -Inf ------------------------------------------------

postAgg.df <- CheckValues(dataset = postAgg.df, columns = colnames(postAgg.df)[
  -grep("FAOST_CODE|Year|Area|POU_DISTR|FAO_TABLE_NAME", colnames(postAgg.df))])

SYB.df <- postAgg.df

save(x = SYB.df, file = paste0("./output_data/",date,"/SYB",date,".RData"))
# save(x = SYB.df, file = paste0("./Data/Processed/SYB",date,".RData"))
# save(x = SYB.df, file = "./Data/Processed/SYB.RData")
load(file = paste0("./output_data/",date,"/SYB",date,".RData"))

# Merge the FSI dataset ---------------------------------------------------

## Merge the dataset
# load(file = "./Data/Processed/fsi.RData")
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
# SYB.df <- merge(SYB.df, fsi.df, all = FALSE, by = c("FAOST_CODE", "Year"))
# rm(fsi.df)
# save(x = SYB.df, file = paste0("./Data/Processed/SYB",date,".RData"))
# 
# ## Merge metadata and construction file
# fsicon.df <- ReadConstruction(file = "FSIconstruction15.csv", 
#                               encoding = "UTF-8", nrows = 287)
# fsimeta.lst <- ReadMetadata(file = "FSImetadata15.csv", 
#                             encoding = "UTF-8", nrows = 287)
# fsimeta.df <- fsimeta.lst[["FULL"]]
# 
# fsicon.df <- fsicon.df[fsicon.df[, "STS_ID"] %in% fsiVar,]
# fsimeta.df <- fsimeta.df[fsimeta.df[, "STS_ID"] %in% fsiVar,]
# 
# meta.lst[["FULL"]][["X"]] <- NULL
# meta.lst[["FULL"]] <- rbind(meta.lst[["FULL"]], fsimeta.df)
# con.df <- rbind(con.df, 
#                 fsicon.df[, -grep("MODULE_FSI|MODULE_POU|MODULE_DES", 
#                                   colnames(fsicon.df))])
# 
# save(x = con.df, file = "./Data/Processed/Construction.RData")
# save(x = meta.lst, file = "./Data/Processed/Metadata.RData")

###########################################################################
## End
###########################################################################

###########################################################################
## cumulative
all_missing_datas <- list.files(path = "./output_data",pattern = "missing_data.csv", recursive = T,full.names = T)
check <- meta.lst$FULL[1:2]
for (i in all_missing_datas){
 d <- read.csv(i, stringsAsFactors = FALSE)
 varname <- str_replace_all(i, "/missing_data.csv", "")
 varname <- str_replace_all(varname, "./output_data/", "")
 varname <- str_replace_all(varname, "-", "")
 d$X <- NULL
 names(d)[3] <- paste0("D",varname)
 check <- left_join(check,d[1:3], by = c("STS_ID" = "STS_ID",
                                         "TITLE_STS" = "TITLE_STS"))
}

ff <- apply(SYB.df[,3:ncol(SYB.df)-1], 2, function(x) tail(table(x, useNA="ifany"),1)/nrow(SYB.df)*100)
fff <- as.data.frame(ff)
varname <- paste0("D",str_replace_all(date, "-", ""))
names(fff) <- varname
fff$STS_ID <- row.names(fff)
d <- left_join(check,fff)

colorize_syb <- function(x){
  
  d$col_1 <- 'background-color: #fff7f3'
  d$col_2 <- 'background-color: #fff7f3'
  d[x] <- round(d[x],2)
  d[[ paste0("col_",x) ]][d[x] <    5]  <- 'background-color: #00FF00'
  d[[ paste0("col_",x) ]][d[x] >=  10]  <- 'background-color: #0DF200'
  d[[ paste0("col_",x) ]][d[x] >=  15]  <- 'background-color: #1AE600'
  d[[ paste0("col_",x) ]][d[x] >=  20]  <- 'background-color: #26D900'
  d[[ paste0("col_",x) ]][d[x] >=  25]  <- 'background-color: #33CC00'
  d[[ paste0("col_",x) ]][d[x] >=  30]  <- 'background-color: #40BF00'
  d[[ paste0("col_",x) ]][d[x] >=  35]  <- 'background-color: #4CB200'
  d[[ paste0("col_",x) ]][d[x] >=  40]  <- 'background-color: #59A600'
  d[[ paste0("col_",x) ]][d[x] >=  45]  <- 'background-color: #669900'
  d[[ paste0("col_",x) ]][d[x] >=  50]  <- 'background-color: #738C00'
  d[[ paste0("col_",x) ]][d[x] >=  55]  <- 'background-color: #808000'
  d[[ paste0("col_",x) ]][d[x] >=  60]  <- 'background-color: #8C7300'
  d[[ paste0("col_",x) ]][d[x] >=  65]  <- 'background-color: #996600'
  d[[ paste0("col_",x) ]][d[x] >=  70]  <- 'background-color: #A65900'
  d[[ paste0("col_",x) ]][d[x] >=  75]  <- 'background-color: #B24D00'
  d[[ paste0("col_",x) ]][d[x] >=  80]  <- 'background-color: #BF4000'
  d[[ paste0("col_",x) ]][d[x] >=  85]  <- 'background-color: #CC3300'
  d[[ paste0("col_",x) ]][d[x] >=  90]  <- 'background-color: #D92600'
  d[[ paste0("col_",x) ]][d[x] >=  95]  <- 'background-color: #E61900'
  d[[ paste0("col_",x) ]][is.na(d[x])] <- 'background-color: #696969;'
  return(d)
}

for (i in names(d[-1:-2])){
  d <- colorize_syb(i)
}

library(htmlTable)
ncolcols <- length(names(d)[grepl("col_", names(d))])
print(htmlTable(as.matrix(d[1:ncolcols]), css.cell = as.matrix(d[(ncolcols+1):(2*ncolcols)])),
      file=paste0("./output_data/",date,"/missing_data.html"))
write.csv(d, file=paste0("./output_data/",date,"/missing_data.csv"))

# End timing!
t2 <- Sys.time()
duration <- t2 - t1
writeLines(format(duration), con = paste0("./output_data/",date,"/duration.txt"))




