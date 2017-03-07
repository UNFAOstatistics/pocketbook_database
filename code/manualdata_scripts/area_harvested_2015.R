## Area harvested

library(tidyr)
library(stringr)
library(dplyr)
library(readr)

# download.file("http://faostat3.fao.org/faostat-bulkdownloads/Production_Crops_E_All_Data_(Norm).zip",
#               destfile = "input_data/raw/Production_Crops_E_All_Data_(Norm).zip")
# unzip(zipfile = "./input_data/raw/Production_Crops_E_All_Data_(Norm).zip",
#       exdir = "./input_data/raw/")
# dat <- read_csv("./input_data/raw/Production_Crops_E_All_Data_(Norm).csv")
dat <- read_csv("~/local_data/faostat/csv/production_crops_e_all_data_(normalized).csv")


names(dat) <- tolower(str_replace_all(names(dat), " ", ""))

dat$item <- as.character(dat$item)

dat1 <- dat[grepl("Total", dat$item),]

df_area_harvested.df <- dat1 %>% filter(areacode < 5000, element %in% "Area harvested") %>% 
  group_by(areacode,year) %>% 
  dplyr::summarise(area_harvested = sum(value, na.rm=TRUE))

names(df_area_harvested.df) <- c("FAOST_CODE","Year","area_harvested")

df_area_harvested.df <- df_area_harvested.df[!duplicated(df_area_harvested.df[c("FAOST_CODE","Year")]),]
df_area_harvested.df <- as.data.frame(df_area_harvested.df)

save(x = df_area_harvested.df, file = "./input_data/processed/df_area_harvested.RData")


