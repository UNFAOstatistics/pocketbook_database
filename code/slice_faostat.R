
#  ____  _ _            _____ _    ___  ____ _____  _  _____ 
# / ___|| (_) ___ ___  |  ___/ \  / _ \/ ___|_   _|/ \|_   _|
# \___ \| | |/ __/ _ \ | |_ / _ \| | | \___ \ | | / _ \ | |  
#  ___) | | | (_|  __/ |  _/ ___ \ |_| |___) || |/ ___ \| |  
# |____/|_|_|\___\___| |_|/_/   \_\___/|____/ |_/_/   \_\_|  
# 
# Construction and metadata files -----------------------------------------
source("./code/read_functions/ReadMetadata.R")
source("./code/read_functions/ReadConstruction.R")
con.df <- ReadConstruction(file = "./input_data/Construction2015.csv", 
                           encoding = "UTF-8")
meta.lst <- ReadMetadata(file = "./input_data/Metadata2015.csv", 
                         encoding = "UTF-8")
faostatData.df <- meta.lst[["FAOSTAT"]]
meta_full <- meta.lst[["FULL"]]

library(tidyverse)
library(stringr)

## New bulk download function
fao <- readRDS("~/local_data/faostat/rds/faostat.RDS")
meta <- readRDS("~/local_data/faostat/metadata/meta_faostat.RDS")
csv_data <- readRDS("~/local_data/faostat/metadata/csv_data.RDS")
meta$elementcode <- as.character(meta$elementcode)
meta$itemcode <- as.character(meta$itemcode)
# For FAOST_CODEs
uniquefaocode <- unique(fao$countrycode)[unique(fao$countrycode) < 5000]
uniquefaoyear <- unique(fao$year)

fao$subcat <- csv_data$subcat[match(fao$id, csv_data$id)]
fao$subcat <- ifelse(grepl("emissions", fao$subcat), "emissions", fao$subcat)

vars <- faostatData.df# [faostatData.df[, "SQL_DOMAIN_CODE"] %in% "TP",]
varz <- lapply(vars, as.character)

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
    tmpp <- data_frame(countrycode = uniquefaocode,
                       year = 2000,
                       value = NA)
  }
  tmpp <- tmpp[c("countrycode","year","value")]
  names(tmpp) <- c("FAOST_CODE","Year",name)
  tmpp <- tmpp %>% filter(FAOST_CODE <= 400) %>%  
    mutate(Year = as.integer(Year)) #%>% 
  #filter(Year >= 2010:2016)
  return(tmpp)
}

# name = vars$STS_ID[i]
# elementCode = vars$SQL_ELEMENT_CODE[i]
# itemCode = vars$SQL_ITEM_CODE[i]
# subcat= vars$subcat[i]

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
