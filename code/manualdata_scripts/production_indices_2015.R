# New production indices computed by Amanda
library(tidyr)
library(stringr)
library(readr)


download.file("http://faostat3.fao.org/faostat-bulkdownloads/Production_Indices_E_All_Data_(Norm).zip",
              destfile = "input_data/raw/Production_Indices_E_All_Data_(Norm).zip")
unzip(zipfile = "./input_data/raw/Production_Indices_E_All_Data_(Norm).zip",
      exdir = "./input_data/raw/")
dat <- read_csv("./input_data/raw/Production_Indices_E_All_Data_(Norm).csv")
names(dat) <- str_replace_all(names(dat), " ", "")

dat <- dat[dat$Item %in% c("Roots and Tubers,Total",
                           "Vegetables and Fruit Primary",
                           "Sugar, raw",
                           "Oilcrops Primary",
                           "Meat indigenous, total",
                           "Milk,Total") &
             dat$Element == "Net Production Index Number (2004-2006 = 100)",
           c("CountryCode","Year","Value","Item")]

names(dat) <- c("FAOST_CODE","Year","value","Item")
dat <- spread(dat, # data
              Item, # class-var
              value) # amount
names(dat) <- str_replace_all(names(dat), " ", ".")
names(dat) <- str_replace_all(names(dat), ",", ".")
names(dat) <- str_replace_all(names(dat), "\\.\\.", ".")
names(dat) <- paste0("prod_ind_",names(dat))

names(dat)[names(dat)=="prod_ind_FAOST_CODE"] <- "FAOST_CODE"
names(dat)[names(dat)=="prod_ind_Year"] <- "Year"

prod_ind_2015.df <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]

prod_ind_2015.df <- prod_ind_2015.df[prod_ind_2015.df$FAOST_CODE < 5000,]

save(x = prod_ind_2015.df, file = "./input_data/processed/prod_ind_2015.RData")
