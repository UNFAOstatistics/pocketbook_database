library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(tidyr)


# note by Markus 20170218

# Code breaks for some reason when this new data is introduced. gli


# label = Chart 17: Aid  commitment flows to agriculture, forestry and fishing, share of total aid in % (1995-2013)
# dat <- read_excel("Data/Raw/oda_data_from_brian_2005.xlsx",sheet = 1, skip = 3)
# dat1 <- dat %>% select(year,recipientcode,dfa_share_commit_tot) %>% 
#    rename(Year = year,FAOST_CODE = recipientcode)
# dat1 <- dat1[!duplicated(dat1[c("FAOST_CODE","Year")]),]


# if (!file.exists("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).Rdata")){
#   download.file("http://faostat3.fao.org/faostat-bulkdownloads/Development_Assistance_to_Agriculture_E_All_Data_(Norm).zip",
#                 destfile = "~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).zip")
#   unzip("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).zip",
#         exdir = "~/fao_temp/data/")
#   d <- read_csv("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).csv")
#   save(d, file="~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).Rdata")
# } else load("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).Rdata")
## As using bulk now

d <- readRDS("~/local_data/faostat/rds/development_assistance_to_agriculture_e_all_data_(normalized).RDS")

# label = Chart20 : Aid commitment flow to Agriculture, Forestry and Fishing, (1995-2013),  2013 USD in million
# dat <- read_excel("./input_data/raw/oda_brian_2015/oda_data_from_brian_2005.xlsx",sheet = 3, skip = 3)
# dat3 <- dat %>% select(year,donor,recipientcode,dfa_commit_usd2013) %>% 
#   filter(recipientcode <= 351) %>% 
#   rename(Year = year,FAOST_CODE = recipientcode)
# dat3 <- spread(data=dat3, key = donor, value = dfa_commit_usd2013)
# dat3 <- dat3 %>% rename(bilat_don_agr = `Bilateral Donors, Total`,
#                         multilat_don_agr = `Multilateral Donors, Total`,
#                         privat_don_agr = `Private Donors, Total`)


dat3 <- d %>% filter(itemcode == 22040,
                  elementcode == 6137,
                  donorcode %in% c(690,691,692),
                  year >= 1995,
                  recipientcountrycode < 400,
                  purpose == "Agriculture, forestry, fishing") %>% 
  select(recipientcountrycode,year,donor,value) %>% 
  spread(data=., key = donor, value = value) %>% 
  rename(bilat_don_agr = `Bilateral Donors`,
         multilat_don_agr = `Multilateral Donors`,
         privat_don_agr = `Private Donors`,
         FAOST_CODE = recipientcountrycode,
         Year = year)



# label = Chart 19: DFA Agriculture Orientation Index, highest and lowest values, average (2009-2013)
# dat <- read_excel("./input_data/raw/oda_brian_2015/oda_data_from_brian_2005.xlsx",sheet = 2, skip = 3)
# dat2 <- dat %>% select(year,recipientcode,dfa_AOI_commit) %>% 
#   rename(Year = year,FAOST_CODE = recipientcode)
# dat2 <- dat2[!duplicated(dat2[c("FAOST_CODE","Year")]),]

dat2 <- d %>% filter(itemcode == 22040,
                     elementcode == 6112,
                     donorcode %in% c(702),
                     year >= 1995,
                     recipientcountrycode < 400,
                     purpose == "Agriculture, forestry, fishing") %>% 
  select(recipientcountrycode,year,value) %>% 
  rename(dfa_AOI_commit = value,
         FAOST_CODE = recipientcountrycode,
         Year = year)


# d <- left_join(dat3,FAOcountryProfile)

# 
# dat <- read_csv("./input_data/raw/oda_brian_2015/Regional_Yearbook_Total_ODA_by_country_1995_2013.csv")
# dat4 <- dat %>% group_by(year,recipientcode) %>% mutate(total_oda_usd2013 = sum(dfa_commit_usd2013, na.rm=T)) %>% 
#             select(year,recipientcode,recipient,dfa_subsector,dfa_commit_usd2013,total_oda_usd2013) %>% 
#             filter(dfa_subsector %in% "Agriculture, Forestry & Fishing, Total") %>% 
#             mutate(dfa_share_commit_tot = dfa_commit_usd2013 / total_oda_usd2013 * 100) %>% 
#   select(year,recipientcode,dfa_commit_usd2013,dfa_share_commit_tot,total_oda_usd2013) %>% 
#   rename(FAOST_CODE = recipientcode,Year = year)

# Eli siis tsekkaa tuosta yläpuolelta - summaa kaikki ja jaa sillä agri!


dat4 <- d %>% filter(itemcode == 22040,
                     elementcode == 6137,
                     donorcode %in% c(702),
                     year >= 1995,
                     recipientcountrycode < 400) %>% 
  group_by(year,recipientcountrycode) %>% 
  mutate(total_oda_usd2013 = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  select(recipientcountrycode,year,value,total_oda_usd2013,purpose) %>% 
  filter(purpose %in% "Agriculture, forestry, fishing") %>% 
  mutate(dfa_share_commit_tot = value / total_oda_usd2013 * 100) %>% 
  rename(dfa_commit_usd2013 = value) %>% 
  select(-purpose) %>% 
  rename(FAOST_CODE = recipientcountrycode,
         Year = year)




# merge
dat <- merge(dat2,dat3, by=c("FAOST_CODE","Year"),all=TRUE)
oda_brian_2015.df <- merge(dat,dat4, by=c("FAOST_CODE","Year"),all=TRUE)

oda_brian_2015.df <- oda_brian_2015.df[!is.na(oda_brian_2015.df$Year),]
oda_brian_2015.df <- oda_brian_2015.df[!is.na(oda_brian_2015.df$FAOST_CODE),]

oda_brian_2015.df <- oda_brian_2015.df[oda_brian_2015.df$FAOST_CODE <= 351,]

oda_brian_2015.df <- oda_brian_2015.df[!duplicated(oda_brian_2015.df[c("FAOST_CODE","Year")]),]
oda_brian_2015.df0 <- oda_brian_2015.df


dat2 %>% 
  left_join(.,dat3) %>% 
  left_join(., dat4) %>% 
  filter(!is.na(Year),
         !is.na(FAOST_CODE),
         FAOST_CODE <= 351,
         FAOST_CODE != 286) %>% # This new country code broke the whole system.... I hate this system so much...
  distinct(FAOST_CODE,Year,.keep_all = TRUE) -> oda_brian_2015.df
save(x = oda_brian_2015.df, file = "./input_data/processed/oda_brian_2015.RData")
