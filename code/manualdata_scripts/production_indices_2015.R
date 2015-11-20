# New production indices computed by Amanda
library(tidyr)
library(stringr)

load("~/btsync/faosync/pocketbooks/GSPB15/database/Data/Raw/production_indices/Production_Indices_E_All_Data_(Norm).RData")

dat <- dat[dat$Item %in% c("Roots and Tubers,Total",
                           "Vegetables and Fruit Primary",
                           "Sugar, raw",
                           "Oilcrops Primary",
                           "Meat indigenous, total",
                           "Milk,Total") &
             dat$Element == "Net Production Index Number (2004-2006 = 100)",
           c("Country.Code","Year","Value","Item")]

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

save(x = prod_ind_2015.df, file = "./Data/Processed/prod_ind_2015.RData")
