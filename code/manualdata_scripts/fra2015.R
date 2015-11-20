library(dplyr)
library(readxl)
library(countrycode)

download.file("http://www.fao.org/fileadmin/user_upload/FRA/spreadsheet/FRA_data/BULK.zip", "./input_data/raw/fra2015/fra.zip")
unzip('./input_data/raw/fra2015/fra.zip',exdir = "./input_data/raw/fra2015/")

# dat <- read.csv("./input_data/raw/fra2015/1.FOREST AREA AND CHARACTERISTICS.csv", stringsAsFactors = FALSE) # .csv's were ditched?!
dat <- read_excel("./input_data/raw/fra2015/BULK.xlsx") # 
dat <- dat %>% select(Country,Year,PrimFor,PlantFor,NatRegFor) %>% na.omit()
dat$FAOST_CODE <- countrycode(dat$Country, "iso3c", "fao")
dat <- dat[c("FAOST_CODE","Year","PrimFor","PlantFor","NatRegFor")]

fra2015.df <- dat[!is.na(dat$FAOST_CODE),]

save(x = fra2015.df, file = "./input_data/processed/fra2015.RData")
