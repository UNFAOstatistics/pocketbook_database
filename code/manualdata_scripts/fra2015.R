library(dplyr)
library(countrycode)

download.file("http://www.fao.org/fileadmin/user_upload/FRA/spreadsheet/FRA_data/BULK.zip", "./Data/Raw/fra.zip")
unzip('./Data/Raw/fra.zip',exdir = "./Data/Raw/fra/")

dat <- read.csv("./Data/Raw/fra/1.FOREST AREA AND CHARACTERISTICS.csv", stringsAsFactors = FALSE)
dat <- dat %>% select(Country,Year,PrimFor,PlantFor,NatRegFor) %>% na.omit()
dat$FAOST_CODE <- countrycode(dat$Country, "iso3c", "fao")
dat <- dat[c("FAOST_CODE","Year","PrimFor","PlantFor","NatRegFor")]

fra2015.df <- dat[!is.na(dat$FAOST_CODE),]

save(x = fra2015.df, file = "./Data/Processed/fra2015.RData")
