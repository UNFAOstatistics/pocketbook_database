library(gdata)
library(stringr)
library(tidyr)
library(dplyr)
library(FAOSTAT)


# ---- Young and old

# download.file(url="http://esa.un.org/unpd/wpp/Excel-Data/EXCEL_FILES/1_Population/WPP2012_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.XLS",
#               destfile="Data/Raw/WPP2012_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.XLS")

## ESTIMATES

# Saved the each sheet as a single .csv -file first in libreoffice calc 
un.pop.est <- read.csv("Data/Raw/WPP2012_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES_1.csv", skip=16)

dl <- gather(un.pop.est, "age", "n", 7:28);rm(un.pop.est)
# clean
dl$age <- str_replace_all(dl$age, "\\.", "_")
dl$age <- str_replace_all(dl$age, "__", "_")
dl$age <- str_replace_all(dl$age, "_", "-")
dl$age <- str_replace_all(dl$age, "X", "")
# values
dl$n <- str_replace_all(dl$n, " ", "")
dl$n <- factor(dl$n)
dl$n <- as.numeric(levels(dl$n))[dl$n]
# rename the id cols
names(dl)[1:6] <- c("index","variant","region","notes","countrycode","year")

## the young
est.young <- dl[dl$age %in% c("0-4","5-9","10-14"),] %>% group_by(countrycode,year) %>% summarise(n = sum(n))
# old age data is in different columns for different countries
est.old <- dl[!(dl$age %in% c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")),] %>% group_by(countrycode,year) %>% summarise(n = sum(n, na.rm = TRUE))
rm(dl)
## PROJECTIONS

# Saved the each sheet as a single .csv -file first in libreoffice calc 
un.pop.proj <- read.csv("Data/Raw/WPP2012_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES_2.csv", skip=16)

dl <- gather(un.pop.proj, "age", "n", 7:27);rm(un.pop.proj)
# clean
dl$age <- str_replace_all(dl$age, "\\.", "_")
dl$age <- str_replace_all(dl$age, "__", "_")
dl$age <- str_replace_all(dl$age, "_", "-")
dl$age <- str_replace_all(dl$age, "X", "")
# values
dl$n <- str_replace_all(dl$n, " ", "")
dl$n <- factor(dl$n)
dl$n <- as.numeric(levels(dl$n))[dl$n]
# rename the id cols
names(dl)[1:6] <- c("index","variant","region","notes","countrycode","year")

# summarise the young and the old
## the young
proj.young <- dl[dl$age %in% c("0-4","5-9","10-14"),] %>% group_by(countrycode,year) %>% summarise(n = sum(n))
# old age data is in different columns for different countries
proj.old <- dl[!(dl$age %in% c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")),] %>% group_by(countrycode,year) %>% summarise(n = sum(n, na.rm = TRUE))
rm(dl)

young.df <- as.data.frame(rbind(est.young,proj.young));rm(est.young);rm(proj.young)
old.df   <- as.data.frame(rbind(est.old,proj.old));rm(est.old);rm(proj.old)

young.df <- translateCountryCode(young.df, from = "UN_CODE", to = "FAOST_CODE", oldCode = "countrycode")
old.df <- translateCountryCode(old.df, from = "UN_CODE", to = "FAOST_CODE", oldCode = "countrycode")

# as for Missing FAOST_CODE I will just remove all rows with UN_CODE over 900
# missFAOcode <- unique(young.df[is.na(young.df$FAOST_CODE), "UN_CODE"])
young.df <- young.df[!(young.df$UN_CODE >= 900),]
old.df <- old.df[!(old.df$UN_CODE >= 900),]


names(young.df) <- c("UN_CODE","FAOST_CODE","Year","UN.POP.AGE014")
names(old.df)   <- c("UN_CODE","FAOST_CODE","Year","UN.POP.AGE64")

# ---- Rural and Urban

# download.file(url="http://esa.un.org/unpd/wpp/Excel-Data/EXCEL_FILES/1_Population/WPP2012_POP_F01_2_TOTAL_POPULATION_MALE.XLS",
#               destfile="Data/Raw/WPP2012_POP_F01_2_TOTAL_POPULATION_MALE.XLS")
# download.file(url="http://esa.un.org/unpd/wpp/Excel-Data/EXCEL_FILES/1_Population/WPP2012_POP_F01_3_TOTAL_POPULATION_FEMALE.XLS",
#               destfile="Data/Raw/WPP2012_POP_F01_3_TOTAL_POPULATION_FEMALE.XLS")


## ESTIMATES

# Saved the each sheet as a single .csv -file first in libreoffice calc 
un.pop.male.est <- read.csv("Data/Raw/WPP2012_POP_F01_2_TOTAL_POPULATION_MALE_1.csv", skip=16)
un.pop.female.est <- read.csv("Data/Raw/WPP2012_POP_F01_3_TOTAL_POPULATION_FEMALE_1.csv", skip=16)

dl.m <- gather(un.pop.male.est, "year", "n", 6:66);rm(un.pop.male.est)
dl.f <- gather(un.pop.female.est, "year", "n", 6:66);rm(un.pop.female.est)
# clean
clean_dl <- function(dl) {
  dl$year <- str_replace_all(dl$year, "X", "")
  dl$year <- factor(dl$year)
  dl$year <- as.numeric(levels(dl$year))[dl$year]
  
  # values
  dl$n <- str_replace_all(dl$n, " ", "")
  dl$n <- factor(dl$n)
  dl$n <- as.numeric(levels(dl$n))[dl$n]
  # rename the id cols
  names(dl)[1:6] <- c("index","variant","region","notes","countrycode","year")
  dl
}
dl.m <- clean_dl(dl.m)
dl.f <- clean_dl(dl.f)

dl.m.est <- dl.m[c("countrycode","year","n")];rm(df.m)
dl.f.est <- dl.f[c("countrycode","year","n")];rm(df.f)

## PROJECTIONS

# Saved the each sheet as a single .csv -file first in libreoffice calc 
un.pop.male.proj <- read.csv("Data/Raw/WPP2012_POP_F01_2_TOTAL_POPULATION_MALE_2.csv", skip=16)
un.pop.female.proj <- read.csv("Data/Raw/WPP2012_POP_F01_3_TOTAL_POPULATION_FEMALE_2.csv", skip=16)

dl.m <- gather(un.pop.male.proj, "year", "n", 6:96);rm(un.pop.male.proj)
dl.f <- gather(un.pop.female.proj, "year", "n", 6:96);rm(un.pop.female.proj)
# clean
dl.m <- clean_dl(dl.m)
dl.f <- clean_dl(dl.f)

dl.m.proj <- dl.m[c("countrycode","year","n")];rm(dl.m)
dl.f.proj <- dl.f[c("countrycode","year","n")];rm(dl.f)

male.pop.df <- as.data.frame(rbind(dl.m.est,dl.m.proj));rm(dl.m.est);rm(dl.m.proj)
female.pop.df <- as.data.frame(rbind(dl.f.est,dl.f.proj));rm(dl.f.est);rm(dl.f.proj)

male.pop.df <- translateCountryCode(male.pop.df, from = "UN_CODE", to = "FAOST_CODE", oldCode = "countrycode")
female.pop.df <- translateCountryCode(female.pop.df, from = "UN_CODE", to = "FAOST_CODE", oldCode = "countrycode")

# as for Missing FAOST_CODE I will just remove all rows with UN_CODE over 900
# missFAOcode <- unique(young.df[is.na(young.df$FAOST_CODE), "UN_CODE"])
male.pop.df <- male.pop.df[!(male.pop.df$UN_CODE >= 900),]
female.pop.df <- female.pop.df[!(female.pop.df$UN_CODE >= 900),]

names(male.pop.df) <- c("UN_CODE","FAOST_CODE","Year","UN.POP.MALE")
names(female.pop.df)   <- c("UN_CODE","FAOST_CODE","Year","UN.POP.FEMALE")

# Merge -------------------------------------------------------------------

UNPopManualData.df <- data.frame()
UNPopManualData.df <- 
  Reduce(function(x, y) merge(x, y, all = TRUE),
         x = list(old.df, young.df, female.pop.df, male.pop.df))
rm(list = c("old.df", "young.df", "female.pop.df", "male.pop.df"))
UNPopManualData.df$UN_CODE <- NULL
UNPopManualData.df$FAOST_CODE <- as.numeric(UNPopManualData.df$FAOST_CODE)
UNPopManualData.df$Year <- as.integer(UNPopManualData.df$Year)

UNPopManualData.df <- UNPopManualData.df[!duplicated(UNPopManualData.df[c("FAOST_CODE","Year")]),]

save(x = UNPopManualData.df, file = "./Data/Processed/UNPopManualData.RData")

# save(x = young.df, file = "./Data/Processed/UNPopYoung.RData");rm(young.df)
# save(x = old.df, file = "./Data/Processed/UNPopOld.RData");rm(old.df)
# 
# save(x = male.pop.df, file = "./Data/Processed/UNPopMale.RData");rm(male.pop.df)
# save(x = female.pop.df, file = "./Data/Processed/UNPopFemale.RData");rm(female.pop.df)