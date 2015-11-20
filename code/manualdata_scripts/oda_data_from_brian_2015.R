# new weights for 
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

# label = Chart 19: DFA Agriculture Orientation Index, highest and lowest values, average (2009-2013)
dat <- read_excel("Data/Raw/oda_data_from_brian_2005.xlsx",sheet = 2, skip = 3)
dat2 <- dat %>% select(year,recipientcode,dfa_AOI_commit) %>% 
  rename(Year = year,FAOST_CODE = recipientcode)
dat2 <- dat2[!duplicated(dat2[c("FAOST_CODE","Year")]),]

# label = Chart20 : Aid commitment flow to Agriculture, Forestry and Fishing, (1995-2013),  2013 USD in million
dat <- read_excel("Data/Raw/oda_data_from_brian_2005.xlsx",sheet = 3, skip = 3)
dat3 <- dat %>% select(year,donor,recipientcode,dfa_commit_usd2013) %>% 
  filter(recipientcode <= 351) %>% 
  rename(Year = year,FAOST_CODE = recipientcode)
dat3 <- spread(data=dat3, key = donor, value = dfa_commit_usd2013)
dat3 <- dat3 %>% rename(bilat_don_agr = `Bilateral Donors, Total`,
                        multilat_don_agr = `Multilateral Donors, Total`,
                        privat_don_agr = `Private Donors, Total`)

# 
dat <- read_csv("Data/Raw/Regional_Yearbook_Total_ODA_by_country_1995_2013.csv")
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

save(x = oda_brian_2015.df, file = "./Data/Processed/oda_brian_2015.RData")

