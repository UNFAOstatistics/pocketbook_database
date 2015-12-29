library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(tidyr)

# label = Chart 17: Aid  commitment flows to agriculture, forestry and fishing, share of total aid in % (1995-2013)
# dat <- read_excel("Data/Raw/oda_data_from_brian_2005.xlsx",sheet = 1, skip = 3)
# dat1 <- dat %>% select(year,recipientcode,dfa_share_commit_tot) %>% 
#    rename(Year = year,FAOST_CODE = recipientcode)
# dat1 <- dat1[!duplicated(dat1[c("FAOST_CODE","Year")]),]


if (!file.exists("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).Rdata")){
  download.file("http://faostat3.fao.org/faostat-bulkdownloads/Development_Assistance_to_Agriculture_E_All_Data_(Norm).zip",
                destfile = "~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).zip")
  unzip("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).zip",
        exdir = "~/fao_temp/data/")
  d <- read_csv("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).csv")
  save(d, file="~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).Rdata")
} else load("~/fao_temp/data/Development_Assistance_to_Agriculture_E_All_Data_(Norm).Rdata")


# label = Chart20 : Aid commitment flow to Agriculture, Forestry and Fishing, (1995-2013),  2013 USD in million
# dat <- read_excel("./input_data/raw/oda_brian_2015/oda_data_from_brian_2005.xlsx",sheet = 3, skip = 3)
# dat3 <- dat %>% select(year,donor,recipientcode,dfa_commit_usd2013) %>% 
#   filter(recipientcode <= 351) %>% 
#   rename(Year = year,FAOST_CODE = recipientcode)
# dat3 <- spread(data=dat3, key = donor, value = dfa_commit_usd2013)
# dat3 <- dat3 %>% rename(bilat_don_agr = `Bilateral Donors, Total`,
#                         multilat_don_agr = `Multilateral Donors, Total`,
#                         privat_don_agr = `Private Donors, Total`)

names(d) <- gsub(" ", "", names(d))
dat3 <- d %>% filter(ItemCode == 22040,
                  ElementCode == 6137,
                  DonorCode %in% c(690,691,692),
                  Year %in% 1995:2013,
                  RecipientCountryCode <= 351,
                  Purpose == "Agriculture, forestry, fishing") %>% 
  select(RecipientCountryCode,Year,Donor,Value) %>% 
  spread(data=., key = Donor, value = Value) %>% 
  rename(bilat_don_agr = `Bilateral Donors`,
         multilat_don_agr = `Multilateral Donors`,
         privat_don_agr = `Private Donors`,
         FAOST_CODE = RecipientCountryCode)



# label = Chart 19: DFA Agriculture Orientation Index, highest and lowest values, average (2009-2013)
dat <- read_excel("./input_data/raw/oda_brian_2015/oda_data_from_brian_2005.xlsx",sheet = 2, skip = 3)
dat2 <- dat %>% select(year,recipientcode,dfa_AOI_commit) %>% 
  rename(Year = year,FAOST_CODE = recipientcode)
dat2 <- dat2[!duplicated(dat2[c("FAOST_CODE","Year")]),]



# d <- left_join(dat3,FAOcountryProfile)

# 
dat <- read_csv("./input_data/raw/oda_brian_2015/Regional_Yearbook_Total_ODA_by_country_1995_2013.csv")
dat4 <- dat %>% group_by(year,recipientcode) %>% mutate(total_oda_usd2013 = sum(dfa_commit_usd2013, na.rm=T)) %>% 
            select(year,recipientcode,recipient,dfa_subsector,dfa_commit_usd2013,total_oda_usd2013) %>% 
            filter(dfa_subsector %in% "Agriculture, Forestry & Fishing, Total") %>% 
            mutate(dfa_share_commit_tot = dfa_commit_usd2013 / total_oda_usd2013 * 100) %>% 
  select(year,recipientcode,dfa_commit_usd2013,dfa_share_commit_tot,total_oda_usd2013) %>% 
  rename(FAOST_CODE = recipientcode,Year = year)


# merge
dat <- merge(dat2,dat3, by=c("FAOST_CODE","Year"),all=TRUE)
oda_brian_2015.df <- merge(dat,dat4, by=c("FAOST_CODE","Year"),all=TRUE)

oda_brian_2015.df <- oda_brian_2015.df[!is.na(oda_brian_2015.df$Year),]
oda_brian_2015.df <- oda_brian_2015.df[!is.na(oda_brian_2015.df$FAOST_CODE),]

oda_brian_2015.df <- oda_brian_2015.df[oda_brian_2015.df$FAOST_CODE <= 351,]

oda_brian_2015.df <- oda_brian_2015.df[!duplicated(oda_brian_2015.df[c("FAOST_CODE","Year")]),]

save(x = oda_brian_2015.df, file = "./input_data/processed/oda_brian_2015.RData")

