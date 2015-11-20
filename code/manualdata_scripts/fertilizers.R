library(tidyr)
library(stringr)
library(dplyr)

load("~/btsync/faosync/pocketbooks/GSPB15/database/Data/Raw/fertilizers/Inputs_Fertilizers_E_All_Data_(Norm).RData")

dat <- dat[dat$Item %in% c("Phosphate Fertilizers (P205 total nutrients)",
                           "Potash Fertilizers (K20 total nutrients)",
                           "Nitrogen Fertilizers (N total nutrients)") &
             dat$Element == "Consumption in nutrients" &
             dat$Unit == "tonnes of nutrients",
           c("Country.Code","Year","Value","Item")]

names(dat) <- c("FAOST_CODE","Year","value","Item")
dat$Item <- as.character(dat$Item)
dat$Item[dat$Item == "Phosphate Fertilizers (P205 total nutrients)"] <- "phosphate_tonnes"
dat$Item[dat$Item == "Potash Fertilizers (K20 total nutrients)"] <- "potash_tonnes"
dat$Item[dat$Item == "Nitrogen Fertilizers (N total nutrients)"] <- "nitrogen_tonnes"

dat <- spread(dat, Item, value)

for (i in 1:ncol(dat)) {
  dat[[i]] <- as.numeric(dat[[i]])
}
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

fertilizers.df <- dat

fertilizers.df <- fertilizers.df[!duplicated(fertilizers.df[c("FAOST_CODE","Year")]),]

fertilizers.df <- fertilizers.df[fertilizers.df$FAOST_CODE < 5000,]

save(x = fertilizers.df, file = "./Data/Processed/fertilizers.RData")
