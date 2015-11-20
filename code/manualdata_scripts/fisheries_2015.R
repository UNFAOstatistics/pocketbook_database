library(tidyr)
library(stringr)
library(dplyr)
library(readxl)
library(readr)


#' # Data manipulation for fisheries data
#'
#' This doc downloads and munges the fisheries data for SYB process
#'

#' ## Download and munge the raw data
#' Before downloading I saved the sheet `Final table` into a csv-file as the excel formattings were too compilicated for R to digest correctly

# load key data sets
# load(file = "./database/Data/Processed/SYB.RData") # comment if not spinning!!
# sybdata.df <- SYB.df; rm(SYB.df) # comment if not spinning!!
# load("./database/Data/Processed/FAOcountryProfile.RData") # comment if not spinning!!

#### AQUACULTURE
dat <- read_excel("./input_data/raw/fishery2015/Fisheries_bycountry_1990-2013.xlsx", sheet=1, skip=1)

# select vars to drop
drops <- names(dat)[grepl("^Symbol", names(dat))]
dat <- dat[,!(names(dat) %in% drops)]
# Convert into long
dl <- gather(dat,
             "Year",
             "aquaculture_fish_production",
             3:26)
# keep selected columns
dl <- dl[-2]
names(dl)[names(dl)=="Country (UN code)"] <- "UN_CODE"
dl$Year <- factor(dl$Year)
dl$Year <- as.numeric(levels(dl$Year))[dl$Year]
aqua_prod <- dl

#### CAPTURE
dat <- read_excel("./input_data/raw/fishery2015//Fisheries_bycountry_1990-2013.xlsx", sheet=2, skip=1)

# select vars to drop
drops <- names(dat)[grepl("^Symbol", names(dat))]
dat <- dat[,!(names(dat) %in% drops)]
# Convert into long
dl <- gather(dat,
             "Year",
             "capture_fish_production",
             3:26)
# keep selected columns
dl <- dl[-2]
names(dl)[names(dl)=="Country (UN code)"] <- "UN_CODE"
dl$Year <- factor(dl$Year)
dl$Year <- as.numeric(levels(dl$Year))[dl$Year]
captu_prod <- dl

# TOTAL
dat <- read_excel("./input_data/raw/fishery2015//Fisheries_bycountry_1990-2013.xlsx", sheet=3, skip=1)

# select vars to drop
drops <- names(dat)[grepl("^Symbol", names(dat))]
dat <- dat[,!(names(dat) %in% drops)]
# Convert into long
dl <- gather(dat,
             "Year",
             "total_fish_production",
             3:26)
# keep selected columns
names(dl)[names(dl)=="Country (Country)"] <- "UN_CODE"
dl <- dl[-2]
dl$Year <- factor(dl$Year)
dl$Year <- as.numeric(levels(dl$Year))[dl$Year]
tot_prod <- dl

# rm(dat2)
dat2 <- merge(aqua_prod,captu_prod,by=c("UN_CODE","Year"))
dat2 <- merge(dat2,tot_prod,by=c("UN_CODE","Year"))


# Convert uncodes into FAOSTAT we use
FAOcountryProfile <- read_csv("./input_data/FAOcountryProfile.csv")
UN_CODE <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UN_CODE")]))

dat2 <- merge(dat2,UN_CODE,by="UN_CODE", all.x=TRUE)

# compute the aggregates for China
tmp <- dat2 %>% filter(UN_CODE %in% c(156, # China
                                      344, # China, Hong Kong SAR
                                      446, # China, Macao SAR
                                      214 # Taiwan, Province of China
)) %>% group_by(Year) %>% dplyr::summarise(aquaculture_fish_production = sum(aquaculture_fish_production),
                                           capture_fish_production = sum(capture_fish_production),
                                           total_fish_production = sum(total_fish_production))

tmp$FAOST_CODE <- 351
tmp$UN_CODE <- NA

# Add the China aggregates to the data
dat2 <- rbind(dat2,tmp)
# Remove the individual components
dat2 <- dat2[!(dat2$UN_CODE %in% c(156,344,446,214)),]
dat2$UN_CODE <- NULL
dat2 <- dat2[!is.na(dat2$FAOST_CODE),]

# rescale into million tons
dat2$aquaculture_fish_production <- dat2$aquaculture_fish_production / 1000000
dat2$capture_fish_production <- dat2$capture_fish_production / 1000000
dat2$total_fish_production <- dat2$total_fish_production / 1000000

#' ## m49 macro aggregates

# get unique FAOST_CODE and equivalaent m49 region codes
M49macroReg <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_MACRO_REG_CODE","UNSD_SUB_REG_CODE")]))

# add those to  data
dat2 <- merge(dat2,M49macroReg,by="FAOST_CODE",all.x=TRUE)

# recode caribbean and latin american countries to belong into same group
dat2$UNSD_MACRO_REG_CODE[dat2$UNSD_SUB_REG_CODE == 5206] <- 5205
dat2$UNSD_MACRO_REG_CODE[dat2$UNSD_SUB_REG_CODE == 5207] <- 5205
# Compute the regional sums
m49 <- dat2 %>% group_by(UNSD_MACRO_REG_CODE,Year) %>% dplyr::summarise(aquaculture_fish_production = sum(aquaculture_fish_production),
                                                                        capture_fish_production = sum(capture_fish_production),
                                                                        total_fish_production = sum(total_fish_production))
names(m49)[names(m49)=="UNSD_MACRO_REG_CODE"] <- "FAOST_CODE"


#' ## m49world macro aggregates

m49world <- dat2 %>% group_by(Year) %>% dplyr::summarise(aquaculture_fish_production = sum(aquaculture_fish_production),
                                                         capture_fish_production = sum(capture_fish_production),
                                                         total_fish_production = sum(total_fish_production))
m49world$FAOST_CODE <- 5000


#' pile up the country level, region level and world level data

dat2$UNSD_MACRO_REG_CODE <- NULL
dat2$UNSD_SUB_REG_CODE <- NULL

dat2 <- rbind(dat2,m49)
dat2 <- rbind(dat2,m49world)

#' # subset on AQUACULT & CAPTURE prior to 2013 from FAOSTAT
#' probably should have taken also years 2011 and 2012 also from your excel


#' ## Compute the production quantity index

# total production
# compute the 2004-2006 avg
prod04_06 <- dat2 %>% filter(Year %in% c(2004,2005,2006)) %>% group_by(FAOST_CODE) %>% dplyr::summarise(prod_100 = mean(total_fish_production, na.rm=TRUE))
dat2 <- merge(dat2,prod04_06,by="FAOST_CODE",all.x=TRUE)
# compute the index
dat2$production_quantity_index <- dat2$total_fish_production / dat2$prod_100 * 100
dat2 <- dat2[!is.infinite(dat2$production_quantity_index),]

dat2$prod_100 <- NULL

fish2015.df <- dat2

#' # Net fish trade

dat <- read_excel("./input_data/raw/fishery2015/Trade1990_2012_ESSJun2015.xlsx", sheet=1, skip = 1)

# select vars to drop
drops <- names(dat)[grepl("^Symbol", names(dat))]
dat <- dat[,!(names(dat) %in% drops)]
# Convert into long
dl <- gather(dat,
             "Year",
             "net_fish_trade",
             6:28)
# keep selected columns
dl <- dl[c(-3,-4,-5)]
names(dl)[names(dl)=="UN code"] <- "UN_CODE"
dl$Year <- factor(dl$Year)
dl$Year <- as.numeric(levels(dl$Year))[dl$Year]

# Recode into FAOST_CODE
dl <- merge(dl,FAOcountryProfile[c("FAOST_CODE","UN_CODE")],by="UN_CODE")

# Aggregate China
tmp <- dl %>% filter(UN_CODE %in% c(156, # China
                                    344, # China, Hong Kong SAR
                                    446, # China, Macao SAR
                                    214  # Taiwan, Province of China
)) %>%  group_by(Year) %>% dplyr::summarise(net_fish_trade = sum(net_fish_trade))
tmp$FAOST_CODE <- 351
tmp$UN_CODE <- NA
tmp$Country <- "China"

# pile up new China with rest of the data and remove the components of China
dl <- rbind(dl,tmp)
dl <- dl[!(dl$UN_CODE %in% c(156,344,446,214)),]

dl$UN_CODE <- NULL
dl$Country <- NULL

#' ### m49 macro aggregates
M49macroReg <- unique(na.omit(FAOcountryProfile[, c("FAOST_CODE", "UNSD_MACRO_REG_CODE","UNSD_SUB_REG_CODE")]))
dl <- merge(dl,M49macroReg,by="FAOST_CODE",all.x=TRUE)

dl$UNSD_MACRO_REG_CODE[dl$UNSD_SUB_REG_CODE == 5206] <- 5205
dl$UNSD_MACRO_REG_CODE[dl$UNSD_SUB_REG_CODE == 5207] <- 5205

#' ### m49 macro aggregates
m49 <- dl %>% group_by(UNSD_MACRO_REG_CODE,Year) %>% dplyr::summarise(net_fish_trade = sum(net_fish_trade,na.rm=TRUE))
names(m49) <- c("FAOST_CODE","Year","net_fish_trade")

m49world <- dl %>% group_by(Year) %>% dplyr::summarise(net_fish_trade = sum(net_fish_trade,na.rm=TRUE))
names(m49world) <- c("Year","net_fish_trade")
m49world$FAOST_CODE <- 5000

dl$UNSD_MACRO_REG_CODE <- NULL
dl$UNSD_SUB_REG_CODE <- NULL
dl <- rbind(dl,m49)
dl <- rbind(dl,m49world)


fish2015.df <- merge(fish2015.df,dl,by=c("FAOST_CODE","Year"), all.x=TRUE)

fish2015.df <- fish2015.df[!duplicated(fish2015.df[c("FAOST_CODE","Year")]),]

fish2015.df <- fish2015.df[fish2015.df$FAOST_CODE < 5000,]
fish2015.df <- fish2015.df[!is.na(fish2015.df$FAOST_CODE),]

save(x = fish2015.df, file = "./input_data/processed/fish2015.RData")
