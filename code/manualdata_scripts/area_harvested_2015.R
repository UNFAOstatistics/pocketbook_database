## Area harvested

library(tidyr)
library(stringr)
library(dplyr)
library(lazyeval)

load("~/btsync/faosync/pocketbooks/GSPB15/database/Data/Raw/Production_Crops_E_All_Data.RData")

dat$Item <- as.character(dat$Item)

dat1 <- dat[grep("Total", dat$Item),]

df_area_harvested.df <- dat1 %>% filter(CountryCode < 5000) %>% group_by(CountryCode,Year) %>% dplyr::summarise(area_harvested = sum(Value, na.rm=TRUE))

names(df_area_harvested.df) <- c("FAOST_CODE","Year","area_harvested")

df_area_harvested.df <- df_area_harvested.df[!duplicated(df_area_harvested.df[c("FAOST_CODE","Year")]),]
df_area_harvested.df <- as.data.frame(df_area_harvested.df)

save(x = df_area_harvested.df, file = "./Data/Processed/df_area_harvested.RData")
