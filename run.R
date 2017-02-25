#!/usr/bin/env r
t1 <- Sys.time()
###########################################################################
## This script generates the dataset for the Statistical Yearbooks
###########################################################################

root.dir <- "~/faosync/pocketbooks/pocketbook_database/"
setwd(root.dir)


# Needed libraries --------------------------------------------------------
pkg_list <- c("plyr","dplyr","reshape2","data.table","readr","RJSONIO","FAOSTAT")
lapply(pkg_list, library, character.only = TRUE)


###########################################################################
## Settings
###########################################################################

# temptemp
addnewmetavars <- FALSE
if (addnewmetavars){
  mm <- read_csv("./input_data/Metadata2015.csv")
  mm <- mm %>% filter(!STS_ID %in% c("ILO_EMP_2EMP_SEX_AGE_NB","ILO_female_emp_agri")) %>% select(-OWNED)
  cc <- read_csv("./input_data/Construction2015.csv")
  cc <- cc %>% filter(!STS_ID %in% c("ILO_EMP_2EMP_SEX_AGE_NB","ILO_female_emp_agri"))

  ilorow <- data_frame(STS_ID = c("ILO_EMP_2EMP_SEX_AGE_NB","ILO_female_emp_agri"),
                       TITLE_STS = c("Employed people in 1000","Share of femele employment in agriculture of total female employment"),
                       UNIT_MULT  = c("NA","NA"),
                       UNIT_MEASURE = c("people","percent"),
                       OWNER = c("ILO","ILO"),
                       SOURCE = c("ILO","ILO"),
                       DATA_TYPE = c("processed","processed"),
                       TOPIC = c("demography","demography")
  )
  
  mm %>% 
    bind_rows(.,ilorow) %>%
    write.csv(., "./input_data/Metadata2015.csv", row.names = FALSE)
  
  # Employment by sex and age -- ILO estimates and projections, Nov. 2016 (thousands)
  # Employment by sex and economic activity -- ILO estimates and projections, Nov. 2016 (thousands)
  
  ilorow <- data_frame(STS_ID = c("ILO_EMP_2EMP_SEX_AGE_NB","ILO_female_emp_agri"),
                       STS_ID_WEIGHT = c(NA,"OA.TPBS.POP.PPL.NO"),
                       AGGREGATION = c("sum","weighted.mean"))

  cc %>% 
    bind_rows(.,ilorow) %>%
    write.csv(., "./input_data/Construction2015.csv", row.names = FALSE)
}





# Source functions --------------------------------------------------------
source("./code/misc_functions/CheckLogical.R")
source("./code/misc_functions/CheckValues.R")
source("./code/misc_functions/Sourcehttps.R")
source("./code/misc_functions/sum2.R")
source("./code/read_functions/ReadMetadata.R")
source("./code/read_functions/ReadConstruction.R")



source("./code/FAOSTAT_functions/chConstruct.R")
source("./code/FAOSTAT_functions/chgr.R")
source("./code/FAOSTAT_functions/constructSYB.R")
source("./code/FAOSTAT_functions/geogr.R")
source("./code/FAOSTAT_functions/getFAO.R")
source("./code/FAOSTAT_functions/getFAOtoSYB.R")
source("./code/FAOSTAT_functions/getWDI.R")
source("./code/FAOSTAT_functions/getWDImetaData.R")
source("./code/FAOSTAT_functions/getWDItoSYB.R")
source("./code/FAOSTAT_functions/grConstruct.R")
source("./code/FAOSTAT_functions/printLab.R")
source("./code/FAOSTAT_functions/scaleUnit.R")
source("./code/FAOSTAT_functions/shConstruct.R")
source("./code/FAOSTAT_functions/translateCountryCode.R")
source("./code/FAOSTAT_functions/translateUnit.R")
# 
source("./code/FAOSTAT_functions/Aggregation.R")
source("./code/FAOSTAT_functions/CHMT.R")
source("./code/FAOSTAT_functions/ebind.R")
source("./code/FAOSTAT_functions/FAOcheck.R")
source("./code/FAOSTAT_functions/FAOcountryProfile.R")
source("./code/FAOSTAT_functions/FAOmetaTable.R")
source("./code/FAOSTAT_functions/FAOregionProfile.R")
source("./code/FAOSTAT_functions/FAOsearch.R")
source("./code/FAOSTAT_functions/FAOSTAT-package.R")
source("./code/FAOSTAT_functions/fillCountryCode.R")
source("./code/FAOSTAT_functions/indConstruct.R")
source("./code/FAOSTAT_functions/lsgr.R")
source("./code/FAOSTAT_functions/mergeSYB.R")
source("./code/FAOSTAT_functions/overlap.R")
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

FAOcountryProfile <- read.csv("~/faosync/pocketbooks/pocketbook/input/data/FAOcountryProfile.csv", stringsAsFactors = FALSE)


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
source('~/faosync/pocketbooks/pocketbook/input/code/define_regions.R')


# RAF
FAOcountryProfile$FAO_RAF_REG <- NA
FAOcountryProfile$FAO_RAF_SUB_REG <- NA
FAOcountryProfile$FAO_RAF_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF"]]),]$FAOST_CODE,                     "RAFregion",         FAOcountryProfile$FAO_RAF_REG)
## subregs
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Central_Africa"]]),]$FAOST_CODE,  "RAFCentralAfrica",  FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Eastern_Africa"]]),]$FAOST_CODE,  "RAFEastAfrica",     FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Northern_Africa"]]),]$FAOST_CODE, "RAFNorthAfrica",    FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Southern_Africa"]]),]$FAOST_CODE, "RAFSouthernAfrica", FAOcountryProfile$FAO_RAF_SUB_REG)
FAOcountryProfile$FAO_RAF_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAF_Western_Africa"]]),]$FAOST_CODE,  "RAFWestAfrica",     FAOcountryProfile$FAO_RAF_SUB_REG)

# RAP
FAOcountryProfile$FAO_RAP_REG <- NA
FAOcountryProfile$FAO_RAP_SUB_REG <- NA
FAOcountryProfile$FAO_RAP_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP"]]),]$FAOST_CODE,                               "RAPregion",              FAOcountryProfile$FAO_RAP_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Central_Asia"]]),]$FAOST_CODE,              "RAPCentralAsia",         FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Eastern_Asia"]]),]$FAOST_CODE,              "RAPEastAsia",            FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_South_Eastern_Asia"]]),]$FAOST_CODE,        "RAPSoutheastAsia",       FAOcountryProfile$FAO_RAP_SUB_REG)
## New ones
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Oceania"]]),]$FAOST_CODE,                    "RAPOceania",            FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Western_Asia"]]),]$FAOST_CODE,              "RAPWesternAsia",         FAOcountryProfile$FAO_RAP_SUB_REG)
FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Southern_Asia"]]),]$FAOST_CODE,             "RAPSouthernAsia",        FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Austriala_and_New_Zealand"]]),]$FAOST_CODE, "RAPAustraliaNewZealand", FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Melanesia"]]),]$FAOST_CODE,                 "RAPMelanesia",           FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Micronesia"]]),]$FAOST_CODE,                "RAPMicronesia",          FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Polynesia"]]),]$FAOST_CODE,                 "RAPPolynesia",           FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_Russian_Federation"]]),]$FAOST_CODE,        "RAPRussianFederation",   FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_France"]]),]$FAOST_CODE,                    "RAPFrance",              FAOcountryProfile$FAO_RAP_SUB_REG)
# FAOcountryProfile$FAO_RAP_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["RAP_United_States"]]),]$FAOST_CODE,             "RAPUnitedStates",        FAOcountryProfile$FAO_RAP_SUB_REG)

# New regiona for a ALL FAO RAP MEMBERS ie. WAP - removed 20160502
# FAOcountryProfile$FAO_WAP_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["WAP"]]),]$FAOST_CODE,                               "WAPregion",              FAOcountryProfile$FAO_RAP_REG)


# REU
FAOcountryProfile$FAO_REU_REG <- NA
FAOcountryProfile$FAO_REU_SUB_REG <- NA

FAOcountryProfile$FAO_REU_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU"]]),]$FAOST_CODE,                            "REUregion",               FAOcountryProfile$FAO_REU_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_South_Eastern_Europe"]]),]$FAOST_CODE,   "REUSouthEasternEurope",   FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_EU_other_and_EFTA"]]),]$FAOST_CODE,      "REUEUOtherAndEFTA",       FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_Caucasus_and_Turkey"]]),]$FAOST_CODE,    "REUCaucAndTurkey",        FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_CIS_Europe"]]),]$FAOST_CODE,             "REUCISeurope",            FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_EU_Central_and_Eastern"]]),]$FAOST_CODE, "REUEUCentralandEastern",  FAOcountryProfile$FAO_REU_SUB_REG)
# FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_Israel"]]),]$FAOST_CODE,                 "REUIsrael",               FAOcountryProfile$FAO_REU_SUB_REG)
FAOcountryProfile$FAO_REU_SUB_REG <- ifelse(FAOcountryProfile$FAOST_CODE %in% region_key[which(region_key[["REU_Central_Asia"]]),]$FAOST_CODE,           "REUCentralAsia",          FAOcountryProfile$FAO_REU_SUB_REG)
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
session_path <- paste0("./output_data/",date)
dir.create(session_path)



###########################################################################
## BULKDOWNLOAD WHOLE LOT
###########################################################################

#  ____  _   _ _     _  ______   _____        ___   _ _     ___    _    ____  
# | __ )| | | | |   | |/ |  _ \ / _ \ \      / | \ | | |   / _ \  / \  |  _ \ 
# |  _ \| | | | |   | ' /| | | | | | \ \ /\ / /|  \| | |  | | | |/ _ \ | | | |
# | |_) | |_| | |___| . \| |_| | |_| |\ V  V / | |\  | |__| |_| / ___ \| |_| |
# |____/ \___/|_____|_|\_|____/ \___/  \_/\_/  |_| \_|_____\___/_/   \_|____/ 
#                                                                              


want_to_bulk <- FALSE
library(tidyverse)
library(stringr)

if (want_to_bulk){
  
  # _____  _     ___   ____  _____   _   _____ 
  # |  ___|/ \   / _ \ / ___||_   _| / \ |_   _|
  # | |_  / _ \ | | | |\___ \  | |  / _ \  | |  
  # |  _|/ ___ \| |_| | ___) | | | / ___ \ | |  
  # |_| /_/   \_\\___/ |____/  |_|/_/   \_\|_|  
  
  
  #' Download the zipfile from FAOSTAT

    unlink("~/local_data/faostat/csv/", recursive = TRUE, force = TRUE)
    dir.create("~/local_data/faostat/csv/", showWarnings = FALSE, recursive = TRUE)
    dir.create("~/local_data/faostat/rds/", showWarnings = FALSE, recursive = TRUE)
    dir.create("~/local_data/faostat/metadata/", showWarnings = FALSE, recursive = TRUE)
    
    download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/FAOSTAT.zip", 
                  destfile = paste0("~/local_data/faostat/FAOSTAT",Sys.Date(),".zip"))
    unzip(zipfile = "~/local_data/faostat/FAOSTAT2017-02-19.zip", exdir = "~/local_data/faostat/csv")
    zipps <- list.files("~/local_data/faostat/csv/", ".zip", full.names = TRUE)
    for (i in zipps){
      unzip(zipfile = i, exdir = "~/local_data/faostat/csv")
    }
    #' lowercase filenames
    csvs <- list.files("~/local_data/faostat/csv/", ".csv", full.names = TRUE)
    for (i in csvs){
      file.rename(from = i, to = tolower(i))
    }
    for (i in zipps){
      file.remove(i)
    }

  # read FAOSTAT
  path <- list.files("~/local_data/faostat/csv/", ".csv", full.names = TRUE)
  name <- sub(".csv", "", list.files("~/local_data/faostat/csv/", ".csv", full.names = FALSE))
  cat <- sub("_.+","",name)
  subcat <- sub("E_.+","",name)
  subcat <- sub("_$","",subcat)
  
  # 'create data with datafile names for the following loops-by-file processes
  csv_data <- data_frame(cat=cat,
                         subcat=subcat,
                         filename=name,
                         filepath=path)
  csv_data$id <- 1:nrow(csv_data)
  
  saveRDS(csv_data, file = "~/local_data/faostat/metadata/csv_data.RDS")
  
  #' first, the key table
  vardata <- tribble(
    ~old, ~new,
    "area","country",
    "areacode","countrycode",
    "months","element",
    "source","element",
    "sourcecode","elementcode",
    "indicator","item",
    "indicatorcode","itemcode",
    "monthscode","elementcode",
    "currency","element",
    "isocurrencycode","elementcode"
  )
  
  #' 1st loop: load all files, extract column names for sanitation AND 
  #' save original files as RDS to speed up the rest of the process.
  #' 
  #' we create a one line per data with list column consisting of the vars names
  dat <- data_frame()
  meta_base <- data_frame()
  # for(i in 60:nrow(csv_data)){
  for(i in 1:nrow(csv_data)){
    if (i %in% c(36 # excange rate
                 )) next()
    df <- read_csv(csv_data$filepath[i])
    names(df) <- tolower(gsub(" |\\.", "", names(df)))
    names(df) <- ifelse(names(df) %in% vardata$old, 
                        vardata$new[match(names(df), vardata$old)],names(df))
    if (any(grepl("-",df$year))) df$year <- as.character(as.integer(gsub("-.+$", "", df$year)) + 1)
    df %>% 
      select(-contains("year"), -contains("country"),-contains("flag")) %>% 
      distinct() %>% 
      mutate(unit = as.character(unit)) %>% 
      bind_rows(meta_base, .) -> meta_base
    
    if ("year" %in% names(df)){
      df <- df %>% select(ends_with("code"),year,value,unit)
    } else {
      df <- df %>% select(ends_with("code"),value,unit)
    }
    df %>% 
      mutate(value = as.numeric(value),
             id = csv_data$id[i]) %>% 
      mutate_if(is.integer, as.character) -> df
    df <- df %>% mutate_if(grepl("code", names(.)), as.integer) %>% 
      mutate(id = as.integer(id))
    dat <- bind_rows(dat,df)
  }
  dat %>% mutate(year = as.integer(year)) -> dat
  saveRDS(dat, "~/local_data/faostat/rds/faostat.RDS")
  
  #' there are many not necessary variables, get rid of them and find the distinctive cases
  meta_base %>% 
    select(-value,-note,-reportercountries,-partnercountries,-survey,-breakdownvariablecode,-breakdownvariable,-breadownbysexofthehouseholdheadcode,-breadownbysexofthehouseholdhead,-measurecode,-measure) %>% 
    distinct() %>% saveRDS(., "~/local_data/faostat/metadata/meta_faostat.RDS")
  
#  __        __ ___   ____   _      ____    ____     _     _   _  _  __
#  \ \      / // _ \ |  _ \ | |    |  _ \  | __ )   / \   | \ | || |/ /
#   \ \ /\ / /| | | || |_) || |    | | | | |  _ \  / _ \  |  \| || ' / 
#    \ V  V / | |_| ||  _ < | |___ | |_| | | |_) |/ ___ \ | |\  || . \ 
#     \_/\_/   \___/ |_| \_\|_____||____/  |____//_/   \_\|_| \_||_|\_\

  unlink("~/local_data/wdi/csv/", force = TRUE, recursive = TRUE)  
  dir.create("~/local_data/wdi/csv/", showWarnings = FALSE, recursive = TRUE)
    dir.create("~/local_data/wdi/rds/", showWarnings = FALSE, recursive = TRUE)
    download.file("http://databank.worldbank.org/data/download/WDI_csv.zip",
                  destfile = paste0("~/local_data/wdi/WDI_csv",Sys.Date(),".zip"))

    unzip("~/local_data/wdi/WDI_csv2017-02-19.zip", 
          exdir = "~/local_data/wdi/csv/")

  # read WDI
  wdi_raw <- read_csv("~/local_data/wdi/csv/WDI_Data.csv")
  names(wdi_raw) <- tolower(names(wdi_raw))
  names(wdi_raw) <- str_replace_all(names(wdi_raw), " ", ".")
  wdi_raw$indicator.name <- NULL
  wdi_raw$country.name <- NULL
  wdi_raw <- gather(wdi_raw,
                    Year,
                    value,
                    3:ncol(wdi_raw))
  wdi_raw <- wdi_raw[!is.na(wdi_raw$value),]
  wdi_raw <- spread(wdi_raw, 
                    indicator.code,
                    value)
  wdi_raw$Year <- as.integer(wdi_raw$Year)
  wdi_data <- wdi_raw
  # file.remove("~/local_data/wdi/csv/WDI_Data.csv")
  
  saveRDS(wdi_data, file="~/local_data/wdi/rds/WDI_Data.RDS")
  
  #  ___  _      ___  
  # |_ _|| |    / _ \ 
  #  | | | |   | | | |
  #  | | | |___| |_| |
  # |___||_____|\___/ 

  dir.create("~/local_data/ilo/csv/", showWarnings = FALSE, recursive = TRUE)
  dir.create("~/local_data/ilo/rds/", showWarnings = FALSE, recursive = TRUE)
  download.file("http://www.ilo.org/ilostat-files/WEB_bulk_download/bulk_ILOEST_EN.7z", 
                destfile = paste0("~/local_data/ilo/bulk_ILOEST_EN",Sys.Date(),".7z"))  
  system("dtrx -n -f ~/local_data/ilo/bulk_ILOEST_EN2017-02-19.7z")
  file.copy("./bulk_ILOEST_EN.csv", "~/local_data/ilo/csv/")
  file.remove("./bulk_ILOEST_EN.csv")
# PROCESS ILO

d <- read_csv("~/local_data/ilo/csv/bulk_ILOEST_EN.csv")
# "Employment by sex and age -- ILO estimates and projections, Nov. 2016 (thousands)"
d %>% filter(indicator == "EMP_2EMP_SEX_AGE_NB",
             !grepl("^X", ref_area),
             sex =="SEX_T",
             classif1 == "AGE_YTHADULT_YGE25") %>% 
  # select(collection.label) %>% distinct() %>% View()
  select(ref_area,time,obs_value) %>% 
  mutate(FAOST_CODE = countrycode::countrycode(ref_area, 
                                               origin = "iso3c", 
                                               destination = "fao")) %>% 
  rename(Year = time,
         ILO_EMP_2EMP_SEX_AGE_NB = obs_value) %>% 
  select(-ref_area) -> ilo1

# Employment by sex and economic activity -- ILO estimates and projections, Nov. 2016 (thousands)
d %>% filter(indicator == "EMP_2EMP_SEX_ECO_NB",
             !grepl("^X", ref_area),
             sex =="SEX_F",
             classif1 %in% c("ECO_SECTOR_AGR","ECO_SECTOR_TOTAL")) %>% 
  select(ref_area,classif1,time,obs_value) %>% 
  spread(., classif1, obs_value) %>% 
  mutate(ILO_female_emp_agri = ECO_SECTOR_AGR / ECO_SECTOR_TOTAL * 100,
         FAOST_CODE = countrycode::countrycode(ref_area, 
                                                            origin = "iso3c", 
                                                            destination = "fao")) %>% 
  rename(Year = time) %>% 
  select(FAOST_CODE,Year,ILO_female_emp_agri) -> ilo2

ilo_data <- full_join(ilo1,ilo2)
saveRDS(ilo_data, file="~/local_data/ilo/rds/ilo_data.RDS")

}
    


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
meta_full <- meta.lst[["FULL"]]
dwnldOA  <- FALSE # Population # FALSE DEFAULT
dwnldFO  <- FALSE # Forestry # FALSE DEFAULT
dwnldGHG <- FALSE # Greenhouse gases # FALSE DEFAULT
dwnldFB  <- FALSE # Food balance sheets # FALSE DEFAULT

dwnldRL  <- TRUE # Resources, Resources - Land
dwnldRF  <- TRUE # Resources - Fertilizers
dwnldRP  <- TRUE # Resources - Pesticides
dwnldCS  <- FALSE # Investments - Capital stock
dwnldRM  <- FALSE # Investments - Machinery
dwnldIG  <- FALSE # Government expenditures
dwnldA   <- TRUE # ASTI
dwnldQC  <- TRUE # Production - Crops
dwnldQA  <- TRUE # Production - Live animals
dwnldQD  <- TRUE # Production - Crops processed
dwnldQL  <- TRUE # Production - Livestock primary
dwnldQP  <- TRUE # Production - Livestock processed
dwnldQV  <- TRUE # Production - Value of agricultural production
dwnldQI  <- TRUE # Production indices
dwnldTP  <- TRUE # Trade - Crops and livestock products
dwnldTI  <- TRUE # Trade - Trade indices
dwnldCOF <- FALSE # Coffeebook indicators
downloadWB <- TRUE; CheckLogical(downloadWB)

# replication_date <- "2016-09-15-09" # used to work
replication_date <- "2016-12-15-20" # used to work
# replication_date <- "2016-02-08-23" 


if (!file.exists("~/local_data/faostat/rds/faostat_dat3.RDS")){
  
  ## New bulk download function
  fao <- readRDS("~/local_data/faostat/rds/faostat.RDS")
  meta <- readRDS("~/faosync/syb_bulk_database/metadata/meta_faostat.RDS")
  csv_data <- readRDS("~/faosync/syb_bulk_database/metadata/csv_data.RDS")
  meta$elementcode <- as.character(meta$elementcode)
  meta$itemcode <- as.character(meta$itemcode)
  # For FAOST_CODEs
  faostcodedata <- readRDS("~/local_data/faostat/rds/inputs_fertilizers_e_all_data_(normalized).RDS")
  
  fao$subcat <- csv_data$subcat[match(fao$id, csv_data$id)]
  fao$subcat <- ifelse(grepl("emissions", fao$subcat), "emissions", fao$subcat)
  
  vars <- faostatData.df# [faostatData.df[, "SQL_DOMAIN_CODE"] %in% "TP",]
  varz <- lapply(vars, as.character)
  ff <- readRDS("~/local_data/faostat/rds/food_security_data_e_all_data_(norm).RDS")
  
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "OA"] <- "population_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "RL"] <- "inputs_land_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "RF"] <- "inputs_fertilizers_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "RP"] <- "inputs_pesticides_use_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "EP"] <- "inputs_pesticides_use_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "CS"] <- "investment_capitalstock_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "RM"] <- "investment_machinery_e_all_data"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "IG"] <- "investment_governmentexpenditure_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "AA"] <- "asti_researchers_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "AE"] <- "asti_research_spending_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "AR"] <- "asti_researchers_e_all_data_(norm)" # a LOT less
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QC"] <- "production_crops_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QA"] <- "production_livestock_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QD"] <- "production_cropsprocessed_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QL"] <- "production_livestockprimary_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QP"] <- "production_livestockprocessed_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QV"] <- "value_of_production_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QI"] <- "production_indices_e_all_data_(norm)"
  # 
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "QI"] <- "production_cropsprocessed_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "TP"] <- "trade_crops_livestock_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "TI"] <- "trade_indices_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "FO"] <- "forestry_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "GT"] <- "emissions"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "GF"] <- "emissions"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "GL"] <- "emissions"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "GN"] <- "emissions"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "GI"] <- "emissions"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "CP"] <- "prices_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "PP"] <- "prices_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "PI"] <- "price_indices_e_all_data_(normalized)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "CC"] <- "food_security_data_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "CL"] <- "food_security_data_e_all_data_(norm)"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "EE"] <- "environment_energy_e_all_data"
  vars$subcat[vars$SQL_DOMAIN_CODE %in% "FS"] <- "food_security_data_e_all_data_(norm)"
  
  slice_fao <- function(name="T.V.FEFS.PCT3D", 
                        elementCode=6121,
                        itemCode=21033, 
                        subcat="food_security_data_e_all_data_(norm)") {
    require(dplyr)
    tmpp <- fao %>% filter(itemcode == itemCode,
                           elementcode == elementCode,
                           subcat == subcat)
    if (nrow(tmpp) == 0){
      tmpp <- data_frame(countrycode = unique(faostcodedata$countrycode),
                         year = unique(faostcodedata$countrycode),
                         value = NA)
    }
    tmpp <- tmpp[c("countrycode","year","value")]
    names(tmpp) <- c("FAOST_CODE","Year",name)
    tmpp <- tmpp %>% filter(FAOST_CODE <= 400) %>%  
      mutate(Year = as.integer(Year)) #%>% 
    #filter(Year >= 2010:2016)
    return(tmpp)
  }
  
  # for (i in 1:6) {
  for (i in 1:100) {
    new <- slice_fao(name = vars$STS_ID[i],
                     elementCode = vars$SQL_ELEMENT_CODE[i], 
                     itemCode = vars$SQL_ITEM_CODE[i],
                     subcat= vars$subcat[i])
    if (any(new$FAOST_CODE %in% new$Year)) next()
    # meta[meta$itemcode %in% vars$SQL_ITEM_CODE[1] & meta$elementcode %in% vars$SQL_ELEMENT_CODE[1],]
    if (i == 1) {
      dat <- new
    } else {
      dat <- full_join(dat,new,by = c("FAOST_CODE" = "FAOST_CODE","Year" = "Year"))  
    }
  }
  # [1] 66823   102
  dat <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]
  saveRDS(dat, "~/local_data/faostat/rds/faostat_dat1.RDS")
  # for (i in 101:105) {
  for (i in 101:200) {
    new <- slice_fao(name = vars$STS_ID[i],
                     elementCode = vars$SQL_ELEMENT_CODE[i], 
                     itemCode = vars$SQL_ITEM_CODE[i],
                     subcat= vars$subcat[i])
    if (any(new$FAOST_CODE %in% new$Year)) next()
    # meta[meta$itemcode %in% vars$SQL_ITEM_CODE[1] & meta$elementcode %in% vars$SQL_ELEMENT_CODE[1],]
    if (i == 101) {
      dat <- new
    } else {
      dat <- full_join(dat,new,by = c("FAOST_CODE" = "FAOST_CODE","Year" = "Year"))  
    }
  }
  dat <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]
  saveRDS(dat, "~/local_data/faostat/rds/faostat_dat2.RDS")
  for (i in 201:nrow(vars)) {
    new <- slice_fao(name = vars$STS_ID[i],
                     elementCode = vars$SQL_ELEMENT_CODE[i], 
                     itemCode = vars$SQL_ITEM_CODE[i],
                     subcat= vars$subcat[i])
    if (any(new$FAOST_CODE %in% new$Year)) next()
    # meta[meta$itemcode %in% vars$SQL_ITEM_CODE[1] & meta$elementcode %in% vars$SQL_ELEMENT_CODE[1],]
    if (i == 201) {
      dat <- new
    } else {
      dat <- full_join(dat,new,by = c("FAOST_CODE" = "FAOST_CODE","Year" = "Year"))  
    }
  }
  dat <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]
  saveRDS(dat, "~/local_data/faostat/rds/faostat_dat3.RDS")
}



library(tidyverse)
readRDS("~/local_data/faostat/rds/faostat_dat1.RDS") %>% 
  full_join(.,readRDS("~/local_data/faostat/rds/faostat_dat2.RDS")) %>% 
  full_join(.,readRDS("~/local_data/faostat/rds/faostat_dat3.RDS")) -> faost_all.df


#   ____                          _                    _  __        __ ____  
#  |  _ \   ___ __      __ _ __  | |  ___    __ _   __| | \ \      / /| __ ) 
#  | | | | / _ \\ \ /\ / /| '_ \ | | / _ \  / _` | / _` |  \ \ /\ / / |  _ \ 
#  | |_| || (_) |\ V  V / | | | || || (_) || (_| || (_| |   \ V  V /  | |_) |
#  |____/  \___/  \_/\_/  |_| |_||_| \___/  \__,_| \__,_|    \_/\_/   |____/ 
# 

# Download variables from WB ----------------------------------------------

wdi_df <- readRDS("~/local_data/wdi/rds/WDI_Data.RDS")
wdi_vars <- meta.lst[["WDI"]]

wdi_vars %>% filter(!WDINAME %in% c('SP.POP.1564.FE.IN', 'SP.POP.65UP.FE.IN', 'SP.POP.1564.MA.IN', 'SP.POP.65UP.MA.IN','EE.BOD.TOTL.KG')) -> wdi_vars

wdi_df[c("country.code","Year",wdi_vars$WDINAME[1:40])] -> tmp1
wdi_df[c("country.code","Year",wdi_vars$WDINAME[41:nrow(wdi_vars)])] -> tmp2

WB.df <- dplyr::full_join(tmp1,tmp2)

WB.df <- WB.df[!duplicated(WB.df[c("country.code","Year")]),]
##
## Lets use basic "match" for country code mathcing
# WB.df2 <- translateCountryCode(data = WB.df,
#                               from = "ISO2_WB_CODE", 
#                               to = "FAOST_CODE", oldCode = "country.code")
# WB.df$FAOST_CODE <- countrycode::countrycode(sourcevar = WB.df$country.code, origin = "wb", destination = "fao")

WB.df$FAOST_CODE <-   FAOcountryProfile$FAOST_CODE[match(WB.df$country.code, FAOcountryProfile$ISO3_WB_CODE)]

WB.df <- WB.df %>% select(-country.code)

# Markus fixes just to get thing working
WB.df <- WB.df[WB.df$FAOST_CODE < 400,]
WB.df <- WB.df[!is.na(WB.df$FAOST_CODE),]

saveRDS(WB.df, "~/local_data/wdi/rds/WB_tmp.RDS")

# save(x = FAO.df, file = paste0(session_path,"/FAO.RData"))
# load(file = "./output_data/2015-11-17/FAO.RData")


ilo.df <- readRDS("~/local_data/ilo/rds/ilo_data.RDS")
ilo.df <- ilo.df[c(3,1,2,4)]
ilo.df <- na.omit(ilo.df)
# lets add the ilo files to metadata




# Source manual data functions ------------------------------------------------

if (!file.exists("./input_data/processed/AquastatManualData.RData")) source(paste0(root.dir,"/code/manualdata_scripts/AquastatManualData.R"))             else load("./input_data/processed/AquastatManualData.RData")
# rm(fillCountryCode)
library(FAOSTAT)
if (!file.exists("./input_data/processed/BiofuelProduction.RData"))  source(paste0(root.dir,"/code/manualdata_scripts/BiodiselProduction.R"))             else load("./input_data/processed/BiofuelProduction.RData")

if (!file.exists("./input_data/processed/df_area_harvested.RData"))  source(paste0(root.dir,"/code/manualdata_scripts/area_harvested_2015.R"))            else load("./input_data/processed/df_area_harvested.RData")
if (!file.exists("./input_data/processed/fertilizers.RData"))        source(paste0(root.dir,"/code/manualdata_scripts/fertilizers.R"))                    else load("./input_data/processed/fertilizers.RData")
if (!file.exists("./input_data/processed/fish2015.RData"))           source(paste0(root.dir,"/code/manualdata_scripts/fisheries_2015.R"))                 else load("./input_data/processed/fish2015.RData")
if (!file.exists("./input_data/processed/fra2015.RData"))            source(paste0(root.dir,"/code/manualdata_scripts/fra2015.R"))                        else load("./input_data/processed/fra2015.RData") #

if (!file.exists("./input_data/processed/oda_brian_2015.RData"))    source(paste0(root.dir,"/code/manualdata_scripts/oda_data_from_brian_2015.R"))        else load("./input_data/processed/oda_brian_2015.RData")
if (!file.exists("./input_data/processed/regional1.RData"))         source(paste0(root.dir,"/code/manualdata_scripts/overweight2015.R"))                  else load("./input_data/processed/regional1.RData") #
if (!file.exists("./input_data/processed/prod_ind_2015.RData"))     source(paste0(root.dir,"/code/manualdata_scripts/production_indices_2015.R"))         else load("./input_data/processed/prod_ind_2015.RData") #
if (!file.exists("./input_data/processed/prod_ind_weights.RData"))  source(paste0(root.dir,"/code/manualdata_scripts/production_indices_2015_weights.R")) else load("./input_data/processed/prod_ind_weights.RData") #

if (!file.exists("./input_data/processed/UNPopManualData.RData"))  source(paste0(root.dir,"/code/manualdata_scripts/UNPopulationStats.R")) else load("./input_data/processed/UNPopManualData.RData") #
if (!file.exists("./input_data/processed/wbManualData.RData"))     source(paste0(root.dir,"/code/manualdata_scripts/WBManualData.R"))      else load("./input_data/processed/wbManualData.RData") #

# if (!file.exists("./input_data/processed/RWBclimate.R")) source(paste0(root.dir,"/code/manualdata_scripts/RWBclimate.R"))
# if (!file.exists("./input_data/processed/GlobalForestResourceAssessment.RData")) source(paste0(root.dir,"/code/manualdata_scripts/GlobalForestResourceAssessment.R")) else load("./input_data/processed/GlobalForestResourceAssessment.RData") #
# if (!file.exists("./input_data/processed/Fishery.RData"))            source(paste0(root.dir,"/code/manualdata_scripts/FisheryData.R"))                    else load("./input_data/processed/Fishery.RData") #


###########################################################################
## Processing
###########################################################################

# Merging -----------------------------------------------------------------

initial.df = Reduce(function(x, y) merge(x, y, all = TRUE),
                    x = list(# FAOoa.df, # FAOSTAT, Population - Annual population
                      # FAOrl.df, # FAOSTAT, Resources - Land
                      # FAOrf.df, # FAOSTAT, Resources - Fertilizers
                      # FAOrp.df, # FAOSTAT, Resources - Pesticides
                      # FAOcs.df, # FAOSTAT, Investments - Capital stock
                      # FAOrm.df, # FAOSTAT, Investments - Machinery
                      # FAOig.df, # FAOSTAT, Government expenditures
                      # FAOa.df, # FAOSTAT, ASTI
                      # FAOqc.df, # FAOSTAT, Production - Crops
                      # FAOqa.df, # FAOSTAT, Production - Live animals
                      # FAOqd.df, # FAOSTAT, Production - Crops processed
                      # FAOql.df, # FAOSTAT, Production - Livestock primary
                      # FAOqp.df, # FAOSTAT, Production - Livestock processed
                      # FAOqv.df, # FAOSTAT, Production - Value of agricultural production
                      # FAOqi.df, # FAOSTAT, Production - Production indices
                      # FAOtp.df, # FAOSTAT, Trade - Crops and livestock products
                      # FAOti.df, # FAOSTAT, Trade - Trade indices
                      # FAOfo.df, # FAOSTAT, Forestry
                      # FAOghg.df,# FAOSTAT, Greenhouse gases
                      # # FAOfb.df, # FAOSTAT, Food balance sheets
                      # FAOcof.df, # FAOSTAT, coffee
                     ilo.df,
                      faost_all.df,
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
                      # gfra.df, # not updated
                      BiofuelProduction.df, # not update
                      # Fishery.df, # not updated
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

# meta.lst[["UNIT_MULT"]]$UNIT_MULT <- ifelse(meta.lst[["UNIT_MULT"]]$UNIT_MULT == "NA", NA, meta.lst[["UNIT_MULT"]]$UNIT_MULT)

meta.lst[["UNIT_MULT"]][, "UNIT_MULT"] <- as.numeric(translateUnit(meta.lst[["UNIT_MULT"]]$UNIT_MULT))


preConstr.df <- scaleUnit(initial.df, meta.lst[["UNIT_MULT"]])
# rm(initial.df)

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

# preConstr.df$FI.PRD.TOT.TN.NO = preConstr.df$FI.PRD.AQ.TN.NO + 
#   preConstr.df$FI.PRD.CAPT.TN.NO
# 
# # FI.NETVAL.FISH.USD.NO ---------------------------------------------------
# 
# preConstr.df$FI.NETVAL.FISH.USD.NO = preConstr.df$FI.EXVAL.FISH.USD.NO - 
#   preConstr.df$FI.IMVAL.FISH.USD.NO
# 
# # TP.EXVAL.FOODWF.USD.NO --------------------------------------------------
# 
# preConstr.df$TP.EXVAL.FOODWF.USD.NO = preConstr.df$TP.EXVAL.FOOD.USD.NO + 
#   preConstr.df$FI.EXVAL.FISH.USD.NO
# 
# # TP.IMVAL.FOODWF.USD.NO --------------------------------------------------
# 
# preConstr.df$TP.IMVAL.FOODWF.USD.NO = preConstr.df$TP.IMVAL.FOOD.USD.NO + 
#   preConstr.df$FI.IMVAL.FISH.USD.NO

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


con.df <- con.df %>% filter(!grepl("CH$|CH1$", STS_ID))
# 
# # View(con.df[con.df[, "CONSTRUCTION_TYPE"] %in% c("share", "growth", "change", "index"),])
# 
# ## Automatic
# postConstr.lst <- with(con.df[con.df[, "CONSTRUCTION_TYPE"] %in% c("share", "growth", "change", "index"),], # leave the manual construction outside
#                        constructSYB(data = preConstr.df,
#                                     origVar1 = STS_ID_CONSTR1,
#                                     origVar2 = STS_ID_CONSTR2,
#                                     newVarName = STS_ID,
#                                     constructType = CONSTRUCTION_TYPE,
#                                     grFreq = GROWTH_RATE_FREQ,
#                                     grType = GROWTH_TYPE, 
#                                     baseYear = 2000))

tmpx <- con.df[con.df[, "CONSTRUCTION_TYPE"] %in% c("share", "growth", "change", "index"),] # leave the manual construction outside
postConstr.lst <- constructSYB(data = preConstr.df,
                                    origVar1 = tmpx$STS_ID_CONSTR1,
                                    origVar2 = tmpx$STS_ID_CONSTR2,
                                    newVarName = tmpx$STS_ID,
                                    constructType = tmpx$CONSTRUCTION_TYPE,
                                    grFreq = tmpx$GROWTH_RATE_FREQ,
                                    grType = tmpx$GROWTH_TYPE, 
                                    baseYear = 2000)


preAgg.df <- postConstr.lst$data 
#rm(list = c("preConstr.df", "postConstr.lst"))

preAgg.df <- preAgg.df[preAgg.df$FAOST_CODE <= 351,]

# Some odd countries appeared from the bulk data with FAOST_CODES 261 266 268 269 297 348 - EXLCUIND THEM

preAgg.df <- preAgg.df[!preAgg.df$FAOST_CODE %in% c(261,266,268,269,297,348),]

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




# Aggregations ------------------------------------------------------------
## Country aggregation
source("./code/aggregate_functions/CountryAggregation.R")

## China aggregation
source("./code/aggregate_functions/ChinaAggregates.R")

############# MULTICORE ################################

# Save the whole lot before the time consuming 
session_path <- paste0("~/faosync/pocketbooks/pocketbook_database",sub("\\.", "", session_path))
writeLines(session_path, con = "./input_data/session_path.txt") # This is for the paraller aggregation scripts to read at the beginning
save.image(file = paste0(session_path,"/pre_agg_image.RData"))



# session_path <- "./output_data/2017-02-06-19"

# system("gnome-terminal") # 1.
# system("gnome-terminal") # 2.
# system("gnome-terminal") # 3.
# system("gnome-terminal") # 4.

# cd ~/faosync/pocketbooks/pocketbook_database/

system("~/faosync/pocketbooks/pocketbook_database/code/aggregate_functions/parallel_aggregate.sh")

# 
# ## Economic Aggregates
# source("./code/aggregate_functions/EconomicAggregates.R")
# 
# ## Sofi Aggregates
# source("./code/aggregate_functions/SofiAggregates.R")
# 
# ## M49 aggregates
# source("./code/aggregate_functions/M49aggregates.R")
# 
# ## FAO aggregates
# source("./code/aggregate_functions/FAOAggregates.R")

########### RE-START single-core ##################
session_path <- readLines(con = "./input_data/session_path.txt")
load(paste0(session_path,"/pre_agg_image.RData"))
lapply(pkg_list, library, character.only = TRUE)

# Load the computed aggregates!!
EconomicAggregates.df <- readRDS(file = paste0(session_path,"/EconomicAggregates.df.RDS"))
sofiAggs.df           <- readRDS(file = paste0(session_path,"/sofiAggs.df.RDS"))
M49.df                <- readRDS(file = paste0(session_path,"/M49.df.RDS"))
FAOregions.df         <- readRDS(file = paste0(session_path,"/FAOregions.df.RDS"))

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
country.df[country.df[, "FAOST_CODE"] %in% OldCountries$FAOST_CODE, "Area"] = "Old territory"
rm(OldCountries)
## Add country names
country.df <- merge(country.df, FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")], 
                    by = "FAOST_CODE", all.x = FALSE)
# Sourcehttps(source("./Rcode/Final/ComplementaryScripts/FAOAggregates.R")

# aja tänne asti!!
## Economic aggregates
# Sourcehttps(source("./Rcode/Final/ComplementaryScripts/EconomicAggregates.R")
## rbind the datasets

postAgg.df <- rbind(country.df, 
                    M49.df,
                    FAOregions.df,
                    EconomicAggregates.df#,sofiAggs.df
)

# postAgg.df <- rbind(country.df, M49.df)
# rm(list = c("country.df", "M49.df")) # because M49.df takes a loooooong time to run

# Check for NaN, Inf, -Inf ------------------------------------------------

postAgg.df <- CheckValues(dataset = postAgg.df, columns = colnames(postAgg.df)[
  -grep("FAOST_CODE|Year|Area|POU_DISTR|FAO_TABLE_NAME", colnames(postAgg.df))])

SYB.df <- postAgg.df

# debugging begins ---
# SYB.df %>% filter(grepl("RNE",FAOST_CODE), Year %in% c(1998:2002,2010:2015))  %>%
#            select(FAOST_CODE,Year,SL.AGR.EMPL.ZS,SL.AGR.EMPL.FE.ZS)
# debugging ends ---


save(x = SYB.df, file = paste0(session_path,"/SYB",date,".RData"))
# remove other files from output_data
flies <- list.files(session_path, full.names = TRUE)
file.remove(flies[!grepl("SYB", flies)])

# flies <- list.files("./output_data/", recursive = TRUE, full.names = TRUE)
# file.remove(flies[!grepl("SYB", flies)])

# save(x = SYB.df, file = paste0("./Data/Processed/SYB",date,".RData"))
# save(x = SYB.df, file = "./Data/Processed/SYB.RData")
load(file = paste0(session_path,"/SYB",date,".RData"))

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
# load("/home/aurelius/faosync/pocketbooks/pocketbook_database/output_data/2015-11-28-01/SYB2015-11-28-01.RData")
meta.lst <- ReadMetadata(file = "./input_data/Metadata2015.csv", 
                         encoding = "UTF-8")
###########################################################################
## cumulative
all_missing_datas <- list.files(path = "./output_data",pattern = "missing_data.csv", recursive = T,full.names = T)
check <- meta.lst$FULL[1:2]
check2 <- meta.lst$FULL[1:2]
for (i in all_missing_datas){
  d <- read.csv(i, stringsAsFactors = FALSE)
  varname <- stringr::str_replace_all(i, "/missing_data.csv", "")
  varname <- stringr::str_replace_all(varname, "./output_data/", "")
  varname <- stringr::str_replace_all(varname, "-", "")
  d$X <- NULL
  names(d)[3] <- paste0("D",varname)
  check <- left_join(check,d[1:3], by = c("STS_ID" = "STS_ID",
                                          "TITLE_STS" = "TITLE_STS"))
}

ff <- apply(SYB.df[,3:ncol(SYB.df)-1], 2, function(x) tail(table(x, useNA="ifany"),1)/nrow(SYB.df)*100)
fff <- as.data.frame(ff)
varname <- paste0("D",stringr::str_replace_all(date, "-", ""))
names(fff) <- varname
fff$STS_ID <- row.names(fff)
gg <- left_join(check2,fff)
write.csv(gg, file=paste0(session_path,"/missing_data.csv"))
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
      file=paste0(session_path,"/missing_data.html"))


# End timing!
t2 <- Sys.time()
duration <- t2 - t1
writeLines(format(duration), con = paste0(session_path,"/duration.txt"))




