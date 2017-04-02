# MALE FEMALE STUFF
library(tidyverse)
library(stringr)


# Overweight
d <- read.csv("http://apps.who.int/gho/athena/data/data-coded.csv?target=GHO/NCD_BMI_25A&filter=AGEGROUP:*;COUNTRY:*;SEX:*", stringsAsFactors = FALSE)
d %>% 
  as_tibble() %>% 
  select(COUNTRY,YEAR,SEX,Numeric) %>% 
  spread(., key = SEX, value = Numeric) %>% 
  # filter(YEAR >= 1989, COUNTRY %in% c("FIN","ITA","SRB")) %>%
  mutate(FAOST_CODE = countrycode::countrycode(COUNTRY, "iso3c", "fao")) %>% 
  select(-COUNTRY) %>% 
  rename(overweight_FMLE = FMLE,
         overweight_MLE = MLE,
         Year = YEAR) -> overweight


# Obesity

d <- read.csv("http://apps.who.int/gho/athena/data/data-coded.csv?target=GHO/NCD_BMI_30A&filter=AGEGROUP:*;COUNTRY:*;SEX:*", stringsAsFactors = FALSE)
d %>% 
  as_tibble() %>% 
  select(COUNTRY,YEAR,SEX,Numeric) %>% 
  spread(., key = SEX, value = Numeric) %>% 
  # filter(YEAR >= 1989, COUNTRY %in% c("FIN","ITA","SRB")) %>%
  mutate(FAOST_CODE = countrycode::countrycode(COUNTRY, "iso3c", "fao")) %>% 
  select(-COUNTRY) %>% 
  rename(obesity_FMLE = FMLE,
         obesity_MLE = MLE,
         Year = YEAR) -> obesity


# # rur_pop_share_male_female
## Excel is this: http://www.un.org/en/development/desa/population/publications/dataset/urban/URPAS_2014_ALL.xlsx
if (!file.exists("~/local_data/URPAS_2014_ALL.xlsx")){
  download.file("http://www.un.org/en/development/desa/population/publications/dataset/urban/URPAS_2014_ALL.xlsx", destfile = "~/local_data/URPAS_2014_ALL.xlsx")
}
d <- readxl::read_excel("~/local_data/URPAS_2014_ALL.xlsx")
d %>% 
   # filter(LocationName %in% c("Serbia","Italy","Finland")) %>% 
  select(LocationName,Year,Sex,AreaType,Total) %>%
  spread(., key = AreaType, value = Total) %>% 
  group_by(LocationName,Year) %>% 
  mutate(tot_pop = sum(Total)) %>% 
  ungroup() %>% 
  select(-Total,-Urban) %>% 
  spread(., key = Sex, value = Rural) %>% 
  mutate(male = Male/tot_pop*100,
         female = Female/tot_pop*100) %>% 
  select(-tot_pop,-Female,-Male) %>% 
  rename(rural_female_share = female,
         rural_male_share = male) -> dat2r

d %>% 
  # filter(LocationName %in% c("Serbia","Italy","Finland")) %>% 
  select(LocationName,Year,Sex,AreaType,Total) %>%
  spread(., key = AreaType, value = Total) %>% 
  group_by(LocationName,Year) %>% 
  mutate(tot_pop = sum(Total)) %>% 
  ungroup() %>% 
  select(-Total,-Rural) %>% 
  spread(., key = Sex, value = Urban) %>% 
  mutate(male = Male/tot_pop*100,
         female = Female/tot_pop*100) %>% 
  select(-tot_pop,-Female,-Male) %>% 
  rename(urban_female_share = female,
         urban_male_share = male) -> dat2u

rur_pop_share.df <- full_join(dat2r,dat2u)
rur_pop_share.df$FAOST_CODE <- countrycode::countrycode(rur_pop_share.df$LocationName, "country.name", "fao")
rur_pop_share.df$LocationName <- NULL

regional1.df <- full_join(overweight,obesity)
regional1.df <- full_join(regional1.df,rur_pop_share.df)

regional1.df <- regional1.df[!duplicated(regional1.df[c("FAOST_CODE","Year")]),]

regional1.df <- regional1.df[!is.na(regional1.df$FAOST_CODE),]
regional1.df <- regional1.df %>% select(FAOST_CODE,Year,everything())
save(x = regional1.df, file = "input_data/processed/regional1.RData")
