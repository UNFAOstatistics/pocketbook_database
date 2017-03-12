library(tidyr)
library(stringr)
library(dplyr)
library(readr)

# download.file("http://faostat3.fao.org/faostat-bulkdownloads/Inputs_Fertilizers_E_All_Data_(Norm).zip",
#               destfile = "input_data/raw/Inputs_Fertilizers_E_All_Data_(Norm).zip")
# unzip(zipfile = "./input_data/raw/Inputs_Fertilizers_E_All_Data_(Norm).zip",
#       exdir = "./input_data/raw/")
# dat <- read_csv("./input_data/raw/Inputs_Fertilizers_E_All_Data_(Norm).csv")
dat <- read_csv("~/local_data/faostat/csv/inputs_fertilizers_e_all_data_(normalized).csv")

names(dat) <- str_replace_all(names(dat), " ", ".")

dat <- dat[dat$Item %in% c("Phosphate Fertilizers (P205 total nutrients)",
                           "Potash Fertilizers (K20 total nutrients)",
                           "Nitrogen Fertilizers (N total nutrients)") &
             dat$Element == "Consumption in nutrients" &
             dat$Unit == "tonnes of nutrients",
           c("Area.Code","Year","Value","Item")]

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

# Many items have 0 instead of NA, i presume. Lets replace
# fertilizers.df[fertilizers.df == 0] <- NA

save(x = fertilizers.df, file = "./input_data/processed/fertilizers.RData")
