# MALE FEMALE STUFF
library(dplyr)
library(tidyr)
library(readr)
library(stringr)


# Overweight
dat1 <- read.csv("http://apps.who.int/gho/athena/data/data-coded.csv?target=GHO/NCD_BMI_25A&filter=AGEGROUP:*;COUNTRY:*;SEX:*")
dat1$FAOST_CODE <- countrycode::countrycode(dat1$COUNTRY, "iso3c", "fao")
dat1 <- dat1 %>%  select(FAOST_CODE,YEAR,SEX,Numeric) %>%  mutate(Numeric = round(Numeric,0))
overweight <- spread(dat1, SEX, Numeric)
names(overweight) <- c("FAOST_CODE","Year","overweight_BOTH","overweight_FMLE","overweight_MLE")

# dat1 <- dat1 %>% mutate(overweight = paste(MLE,FMLE, sep="/")) %>% select(FAOST_CODE,YEAR,overweight)
# names(dat1) <- c("FAOST_CODE","Year","overweight")
# dat1 <- dat1[!is.na(dat1$FAOST_CODE),]
# dat1 <- dat1[!is.na(dat1$overweight),]
# syb.df <- merge(syb.df,dat1,by=c("FAOST_CODE","Year"), all.x=TRUE)

# Obesity
dat1 <- read.csv("http://apps.who.int/gho/athena/data/data-coded.csv?target=GHO/NCD_BMI_30A&filter=AGEGROUP:*;COUNTRY:*;SEX:*")
dat1$FAOST_CODE <- countrycode::countrycode(dat1$COUNTRY, "iso3c", "fao")
dat1 <- dat1 %>%  select(FAOST_CODE,YEAR,SEX,Numeric) %>%  mutate(Numeric = round(Numeric,0))
obesity <- spread(dat1, SEX, Numeric)
names(obesity) <- c("FAOST_CODE","Year","obesity_BOTH","obesity_FMLE","obesity_MLE")

# dat1 <- dat1 %>% mutate(obesity = paste(MLE,FMLE, sep="/")) %>% select(FAOST_CODE,YEAR,obesity)
# names(dat1) <- c("FAOST_CODE","Year","obesity")
# dat1 <- dat1[!is.na(dat1$FAOST_CODE),]
# dat1 <- dat1[!is.na(dat1$obesity),]
# syb.df <- merge(syb.df,dat1,by=c("FAOST_CODE","Year"), all.x=TRUE)

# tot_pop_male_female
# dat <- syb.df %>% select(FAOST_CODE,Year,OA.TPM.POP.PPL.NO,OA.TPF.POP.PPL.NO)
# dat <- dat[!is.na(dat$OA.TPF.POP.PPL.NO),]
# dat <- dat[!is.na(dat$OA.TPM.POP.PPL.NO),]
# dat$OA.TPF.POP.PPL.NO <- round(dat$OA.TPF.POP.PPL.NO / 1000000,1)
# dat$OA.TPM.POP.PPL.NO <- round(dat$OA.TPM.POP.PPL.NO / 1000000,1)
# dat$tot_pop_male_female <- paste(dat$OA.TPM.POP.PPL.NO,dat$OA.TPF.POP.PPL.NO,sep="/")
# dat1 <- dat[c("FAOST_CODE","Year","tot_pop_male_female")]
# syb.df <- merge(syb.df,dat1,by=c("FAOST_CODE","Year"), all.x=TRUE)

# # rur_pop_share_male_female
dat <- read_csv("~/btsync/faosync/syb_database/input_data/raw/rur_rub_pocketbook_data.csv")
dat <- dat[c(1,2,3,4,22)]
names(dat) <- c("SHORT_NAME","Year","Sex","Area","Total")
dat <- dat[!duplicated(dat[c("SHORT_NAME","Year","Sex","Area")]),]
# dat <- merge(dat,FAOcountryProfile[c("SHORT_NAME","FAOST_CODE")],by="SHORT_NAME") # does not work outside pocketbook as not short_name var 
dat$FAOST_CODE <- countrycode::countrycode(dat$SHORT_NAME, "country.name", "fao")
dat <- dat[!is.na(dat$FAOST_CODE),]

dat$Total <- str_replace_all(dat$Total, ",", "")
dat$Total <- factor(dat$Total)
dat$Total <- as.numeric(levels(dat$Total))[dat$Total]

dat1 <- spread(dat, Sex, Total)
dat1$tot_pop <- dat1$Female + dat1$Male

dat2 <- dat1 %>% group_by(FAOST_CODE,Year) %>% mutate(total_pop = sum(tot_pop)) %>%
  mutate(female_share = Female / total_pop * 100,
         male_share = Male / total_pop * 100)

dat2r <- dat2 %>% filter(Area == "Rural") %>% select(FAOST_CODE,Year,female_share,male_share)
dat2u <- dat2 %>% filter(Area == "Urban") %>% select(FAOST_CODE,Year,female_share,male_share)
names(dat2r) <- c("FAOST_CODE","Year","rural_female_share","rural_male_share")
names(dat2u) <- c("FAOST_CODE","Year","urban_female_share","urban_male_share")

dat3 <- left_join(dat2u,dat2r)

rur_pop_share.df <- dat3

# dat3$rural_pop_gender_shares <- paste(round(dat3$rural_male_share,1),round(dat3$rural_female_share,1), sep="/")
# 
# dat3 <- dat3[c("FAOST_CODE","Year","rural_pop_gender_shares")]
# syb.df <- merge(syb.df,dat3,by=c("FAOST_CODE","Year"), all.x=TRUE)

# agr_employment_male_female
# dat <- syb.df %>% select(FAOST_CODE,Year,SL.AGR.EMPL.FE.ZS,SL.AGR.EMPL.MA.ZS)
# dat <- dat[!is.na(dat$SL.AGR.EMPL.FE.ZS),]
# dat <- dat[!is.na(dat$SL.AGR.EMPL.MA.ZS),]
# 
# dat$agr_employment_male_female <- paste(round(dat$SL.AGR.EMPL.MA.ZS,1),
#                                         round(dat$SL.AGR.EMPL.FE.ZS,1)
#                                         ,sep="/")
# dat1 <- dat[c("FAOST_CODE","Year","agr_employment_male_female")]

# syb.df <- merge(syb.df,dat1,by=c("FAOST_CODE","Year"), all.x=TRUE)

# merge

regional1.df <- full_join(overweight,obesity)
regional1.df <- full_join(regional1.df,rur_pop_share.df)

regional1.df <- regional1.df[!duplicated(regional1.df[c("FAOST_CODE","Year")]),]

regional1.df <- regional1.df[!is.na(regional1.df$FAOST_CODE),]
save(x = regional1.df, file = "input_data/processed/regional1.RData")
