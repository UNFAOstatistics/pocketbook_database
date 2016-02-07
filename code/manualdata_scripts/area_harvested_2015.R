## Area harvested

library(tidyr)
library(stringr)
library(dplyr)
library(readr)

download.file("http://faostat3.fao.org/faostat-bulkdownloads/Production_Crops_E_All_Data_(Norm).zip",
              destfile = "input_data/raw/Production_Crops_E_All_Data_(Norm).zip")
unzip(zipfile = "./input_data/raw/Production_Crops_E_All_Data_(Norm).zip",
      exdir = "./input_data/raw/")
dat <- read_csv("./input_data/raw/Production_Crops_E_All_Data_(Norm).csv")
names(dat) <- str_replace_all(names(dat), " ", "")

dat$Item <- as.character(dat$Item)

dat1 <- dat[grepl("Total", dat$Item),]

df_area_harvested.df <- dat1 %>% filter(CountryCode < 5000, Element %in% "Area harvested") %>% 
  group_by(CountryCode,Year) %>% 
  dplyr::summarise(area_harvested = sum(Value, na.rm=TRUE))

# Troubleshooting the latest area harvested figures

# library(ggplot2)
ggplot(df_area_harvested.df, aes(x=Year,y=area_harvested,color=CountryCode,group=CountryCode)) +
  geom_line(alpha=.4) + scale_y_log10() + theme(legend.position="none")

# solution
df_area_harvested.df <- df_area_harvested.df %>% filter(Year != 2014)

names(df_area_harvested.df) <- c("FAOST_CODE","Year","area_harvested")

df_area_harvested.df <- df_area_harvested.df[!duplicated(df_area_harvested.df[c("FAOST_CODE","Year")]),]
df_area_harvested.df <- as.data.frame(df_area_harvested.df)

save(x = df_area_harvested.df, file = "./input_data/processed/df_area_harvested.RData")


