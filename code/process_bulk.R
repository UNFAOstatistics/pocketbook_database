###########################################################################
## BULKDOWNLOAD WHOLE LOT
###########################################################################
bulk_date <- Sys.Date()
root.dir <- "~/faosync/pocketbooks/pocketbook_database/"
setwd(root.dir)

# Construction and metadata files -----------------------------------------
source("./code/read_functions/ReadMetadata.R")
source("./code/read_functions/ReadConstruction.R")
con.df <- ReadConstruction(file = "./input_data/Construction2015.csv", 
                           encoding = "UTF-8")
meta.lst <- ReadMetadata(file = "./input_data/Metadata2015.csv", 
                         encoding = "UTF-8")
faostatData.df <- meta.lst[["FAOSTAT"]]
meta_full <- meta.lst[["FULL"]]

#  ____  _   _ _     _  ______   _____        ___   _ _     ___    _    ____  
# | __ )| | | | |   | |/ |  _ \ / _ \ \      / | \ | | |   / _ \  / \  |  _ \ 
# |  _ \| | | | |   | ' /| | | | | | \ \ /\ / /|  \| | |  | | | |/ _ \ | | | |
# | |_) | |_| | |___| . \| |_| | |_| |\ V  V / | |\  | |__| |_| / ___ \| |_| |
# |____/ \___/|_____|_|\_|____/ \___/  \_/\_/  |_| \_|_____\___/_/   \_|____/ 
#                                                                              


library(tidyverse)
library(stringr)
# _____  _     ___   ____  _____   _   _____ 
# |  ___|/ \   / _ \ / ___||_   _| / \ |_   _|
# | |_  / _ \ | | | |\___ \  | |  / _ \  | |  
# |  _|/ ___ \| |_| | ___) | | | / ___ \ | |  
# |_| /_/   \_\\___/ |____/  |_|/_/   \_\|_|  


#' Download the zipfile from FAOSTAT

unlink("~/local_data/faostat/csv/", recursive = TRUE, force = TRUE)
dir.create("~/local_data/faostat/csv/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/local_data/faostat/rds/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/local_data/faostat/metadata/", showWarnings = FALSE, recursive = TRUE)

download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/FAOSTAT.zip", 
              destfile = paste0("~/local_data/faostat/FAOSTAT",bulk_date,".zip"))
# unzip(zipfile = "~/local_data/faostat/FAOSTAT2017-02-19.zip", exdir = "~/local_data/faostat/csv")
# unzip(zipfile = "~/local_data/faostat/FAOSTAT2017-03-26.zip", exdir = "~/local_data/faostat/csv")
unzip(zipfile = paste0("~/local_data/faostat/FAOSTAT",bulk_date,".zip"), 
      exdir = "~/local_data/faostat/csv")
zipps <- list.files("~/local_data/faostat/csv/", ".zip", full.names = TRUE)
for (i in zipps){
  unzip(zipfile = i, exdir = "~/local_data/faostat/csv")
}
#' lowercase filenames
csvs <- list.files("~/local_data/faostat/csv/", ".csv", full.names = TRUE)
for (i in csvs){
  file.rename(from = i, to = tolower(i))
}
for (i in zipps){
  file.remove(i)
}

# read FAOSTAT
path <- list.files("~/local_data/faostat/csv/", ".csv", full.names = TRUE)
name <- sub(".csv", "", list.files("~/local_data/faostat/csv/", ".csv", full.names = FALSE))
cat <- sub("_.+","",name)
subcat <- sub("E_.+","",name)
subcat <- sub("_$","",subcat)

# 'create data with datafile names for the following loops-by-file processes
csv_data <- data_frame(cat=cat,
                       subcat=subcat,
                       filename=name,
                       filepath=path)
csv_data$id <- 1:nrow(csv_data)

saveRDS(csv_data, file = "~/local_data/faostat/metadata/csv_data.RDS")

#' first, the key table
vardata <- tribble(
  ~old, ~new,
  "area","country",
  "areacode","countrycode",
  "months","element",
  "source","element",
  "sourcecode","elementcode",
  "indicator","item",
  "indicatorcode","itemcode",
  "monthscode","elementcode",
  "currency","element",
  "isocurrencycode","elementcode"
)

#' 1st loop: load all files, extract column names for sanitation AND 
#' save original files as RDS to speed up the rest of the process.
#' 
#' we create a one line per data with list column consisting of the vars names
dat <- data_frame()
meta_base <- data_frame()
# for(i in 60:nrow(csv_data)){

## DATAS from 1:40
for(i in 1:40){
  if (i %in% c(36 # excange rate
  )) next()
  df <- read_csv(csv_data$filepath[i])
  names(df) <- tolower(gsub(" |\\.", "", names(df)))
  names(df) <- ifelse(names(df) %in% vardata$old, 
                      vardata$new[match(names(df), vardata$old)],names(df))
  if (any(grepl("-",df$year))) df$year <- as.character(as.integer(gsub("-.+$", "", df$year)) + 1)
  df %>% 
    select(-contains("year"), -contains("country"),-contains("flag")) %>% 
    distinct() %>% 
    mutate(unit = as.character(unit)) %>% 
    bind_rows(meta_base, .) -> meta_base
  
  if ("year" %in% names(df)){
    df <- df %>% select(ends_with("code"),year,value,unit)
  } else {
    df <- df %>% select(ends_with("code"),value,unit)
  }
  df %>% 
    mutate(value = as.numeric(value),
           id = csv_data$id[i]) %>% 
    mutate_if(is.integer, as.character) -> df
  df <- df %>% mutate_if(grepl("code", names(.)), as.integer) %>% 
    mutate(id = as.integer(id))
  dat <- bind_rows(dat,df)
}
dat1 <- dat
meta_base1 <- meta_base

## DATAS from 41:nrow(csv_data)
dat <- data_frame()
meta_base <- data_frame()
for(i in 41:nrow(csv_data)){
  if (i %in% c(36 # excange rate
  )) next()
  df <- read_csv(csv_data$filepath[i])
  names(df) <- tolower(gsub(" |\\.", "", names(df)))
  names(df) <- ifelse(names(df) %in% vardata$old, 
                      vardata$new[match(names(df), vardata$old)],names(df))
  if (any(grepl("-",df$year))) df$year <- as.character(as.integer(gsub("-.+$", "", df$year)) + 1)
  df %>% 
    select(-contains("year"), -contains("country"),-contains("flag")) %>% 
    distinct() %>% 
    mutate(unit = as.character(unit)) %>% 
    bind_rows(meta_base, .) -> meta_base
  
  if ("year" %in% names(df)){
    df <- df %>% select(ends_with("code"),year,value,unit)
  } else {
    df <- df %>% select(ends_with("code"),value,unit)
  }
  df %>% 
    mutate(value = as.numeric(value),
           id = csv_data$id[i]) %>% 
    mutate_if(is.integer, as.character) -> df
  df <- df %>% mutate_if(grepl("code", names(.)), as.integer) %>% 
    mutate(id = as.integer(id))
  dat <- bind_rows(dat,df)
}
dat2 <- dat
meta_base2 <- meta_base

bind_rows(dat1,dat2) %>% mutate(year = as.integer(year)) -> dat
saveRDS(dat, "~/local_data/faostat/rds/faostat.RDS")

#' there are many not necessary variables, get rid of them and find the distinctive cases
bind_rows(meta_base1,meta_base2) %>% 
  select(-value,-note,-reportercountries,-partnercountries,-survey,-breakdownvariablecode,-breakdownvariable,-breadownbysexofthehouseholdheadcode,-breadownbysexofthehouseholdhead,-measurecode,-measure) %>% 
  distinct() %>% saveRDS(., "~/local_data/faostat/metadata/meta_faostat.RDS")

#  __        __ ___   ____   _      ____    ____     _     _   _  _  __
#  \ \      / // _ \ |  _ \ | |    |  _ \  | __ )   / \   | \ | || |/ /
#   \ \ /\ / /| | | || |_) || |    | | | | |  _ \  / _ \  |  \| || ' / 
#    \ V  V / | |_| ||  _ < | |___ | |_| | | |_) |/ ___ \ | |\  || . \ 
#     \_/\_/   \___/ |_| \_\|_____||____/  |____//_/   \_\|_| \_||_|\_\

unlink("~/local_data/wdi/csv/", force = TRUE, recursive = TRUE)  
dir.create("~/local_data/wdi/csv/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/local_data/wdi/rds/", showWarnings = FALSE, recursive = TRUE)
download.file("http://databank.worldbank.org/data/download/WDI_csv.zip",
              destfile = paste0("~/local_data/wdi/WDI_csv",bulk_date,".zip"))

unzip(paste0("~/local_data/wdi/WDI_csv",bulk_date,".zip"), 
      exdir = "~/local_data/wdi/csv/")

# read WDI
wdi_raw <- read_csv("~/local_data/wdi/csv/WDI_Data.csv")
names(wdi_raw) <- tolower(names(wdi_raw))
names(wdi_raw) <- str_replace_all(names(wdi_raw), " ", ".")
wdi_raw$indicator.name <- NULL
wdi_raw$country.name <- NULL
wdi_raw <- gather(wdi_raw,
                  Year,
                  value,
                  3:ncol(wdi_raw))
wdi_raw <- wdi_raw[!is.na(wdi_raw$value),]
wdi_raw <- spread(wdi_raw, 
                  indicator.code,
                  value)
wdi_raw$Year <- as.integer(wdi_raw$Year)
wdi_data <- wdi_raw
# file.remove("~/local_data/wdi/csv/WDI_Data.csv")

saveRDS(wdi_data, file="~/local_data/wdi/rds/WDI_Data.RDS")

#  ___  _      ___  
# |_ _|| |    / _ \ 
#  | | | |   | | | |
#  | | | |___| |_| |
# |___||_____|\___/ 

dir.create("~/local_data/ilo/csv/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/local_data/ilo/rds/", showWarnings = FALSE, recursive = TRUE)
download.file("http://www.ilo.org/ilostat-files/WEB_bulk_download/bulk_ILOEST_EN.7z", 
              destfile = paste0("~/local_data/ilo/bulk_ILOEST_EN",bulk_date,".7z"))  
system(paste0("dtrx -n -f ~/local_data/ilo/bulk_ILOEST_EN",bulk_date,".7z"))
file.copy("./bulk_ILOEST_EN.csv", "~/local_data/ilo/csv/")
file.remove("./bulk_ILOEST_EN.csv")
# PROCESS ILO

d <- read_csv("~/local_data/ilo/csv/bulk_ILOEST_EN.csv")
# "Employment by sex and age -- ILO estimates and projections, Nov. 2016 (thousands)"
d %>% filter(indicator == "EMP_2EMP_SEX_AGE_NB",
             !grepl("^X", ref_area),
             sex =="SEX_T",
             classif1 == "AGE_YTHADULT_YGE25") %>% 
  # select(collection.label) %>% distinct() %>% View()
  select(ref_area,time,obs_value) %>% 
  mutate(FAOST_CODE = countrycode::countrycode(ref_area, 
                                               origin = "iso3c", 
                                               destination = "fao")) %>% 
  dplyr::rename(Year = time,
                ILO_EMP_2EMP_SEX_AGE_NB = obs_value) %>% 
  select(-ref_area) -> ilo1

# Employment by sex and economic activity -- ILO estimates and projections, Nov. 2016 (thousands)
d %>% filter(indicator == "EMP_2EMP_SEX_ECO_NB",
             !grepl("^X", ref_area),
             sex =="SEX_F",
             classif1 %in% c("ECO_SECTOR_AGR","ECO_SECTOR_TOTAL")) %>% 
  select(ref_area,classif1,time,obs_value) %>% 
  spread(., classif1, obs_value) %>% 
  mutate(ILO_female_emp_agri = ECO_SECTOR_AGR / ECO_SECTOR_TOTAL * 100,
         FAOST_CODE = countrycode::countrycode(ref_area, origin = "iso3c", destination = "fao")) %>% 
  dplyr::rename(Year = time) %>% 
  select(FAOST_CODE,Year,ILO_female_emp_agri) -> ilo2

d %>% filter(indicator == "EMP_2EMP_SEX_ECO_NB",
             !grepl("^X", ref_area),
             sex =="SEX_M",
             classif1 %in% c("ECO_SECTOR_AGR","ECO_SECTOR_TOTAL")) %>% 
  select(ref_area,classif1,time,obs_value) %>% 
  spread(., classif1, obs_value) %>% 
  mutate(ILO_male_emp_agri = ECO_SECTOR_AGR / ECO_SECTOR_TOTAL * 100,
         FAOST_CODE = countrycode::countrycode(ref_area, origin = "iso3c", destination = "fao")) %>% 
  dplyr::rename(Year = time) %>% 
  select(FAOST_CODE,Year,ILO_male_emp_agri) -> ilo3

ilo_data <- full_join(ilo1,ilo2)
ilo_data <- full_join(ilo_data,ilo3)
saveRDS(ilo_data, file="~/local_data/ilo/rds/ilo_data.RDS")
