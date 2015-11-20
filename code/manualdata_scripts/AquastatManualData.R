###########################################################################
## This script processes the AQUASTAT variables
###########################################################################

## NOTE (FILIPPO): Data come directly from Karen Frenken
## China 357 is in theory 351. However, given that these cannot be aggregated,
## the dataset will skip the aggregation step. This means we need to provide
## China a FAOST_CODE that correspond to the M49 China definition. 

## NOTE (MARKUS, 20150306): Data come directly from Karen Frenken as an xls with 2 sheets
## The structure of the .xls is imitating the structure of the tables.
## Therefore some munging is needed to get the data into per indicator per .csv format
## for to be able to read in the data..

# ------- rough munging

library(gdata)
library(stringr)
library(tidyr)
library(FAOSTAT)

# ------- manual version of fillCountryCode that does not require var SHORT_NAME

##' A function to get country code when not available in data.
##'
##' This function can be useful when a dataset provided does not have
##' a country code available.
##'
##' @param country The column name of the data which contains
##' the country name
##' @param data The data frame to be matched
##' @param outCode The output country code system, defaulted to FAO standard.
##' @export

fillCountryCode = function(country, data, outCode = "FAOST_CODE"){
  unqCountry = unique(data[, country])
  n = length(unqCountry)
  countryCODE = rep(NA, n)
  for(i in 1:n){
    ind = which(as.matrix(FAOcountryProfile[,
                                            c("OFFICIAL_FAO_NAME", #"SHORT_NAME", 
                                              "FAO_TABLE_NAME",
                                              "UNOFFICIAL1_NAME", "UNOFFICIAL2_NAME", "UNOFFICIAL3_NAME")]) ==
                  unqCountry[i], arr.ind = TRUE)
    which.row = ind[, 1]
    if(length(unique(which.row)) == 1)
      countryCODE[i] = FAOcountryProfile[unique(which.row), outCode]
  }
  if(anyDuplicated(na.omit(countryCODE)))
    warning(paste0("Duplicated ", outCode, " matched, double check the data"))
  if(any(is.na(countryCODE)))
    warning(paste0("Certain ", outCode , " were not matched."))
  warning("Please check the correct China has been specified.")
  def = data.frame(unqCountry, countryCODE)
  colnames(def) = c(country, outCode)
  merge(x = data, y = def, by = country, all.x = TRUE)
}

utils::globalVariables(names = c("FAOcountryProfile"))


# AQ.WAT.WATPCP.MC.NO ---------------------------------------------------------

xls1 <- read.xls("./input_data/raw/AquastatManualData//SYB_AQUASTAT_Water_Tables_20150304.xls", sheet = 1)
var1 <- xls1[-1:-3,1:7]
names(var1) <- c("Country","y1990","y1993","y2000","y2003","y2010","y2013")
var1l <- gather(var1,year,value,2:7)
var1l$value <- as.factor(str_replace_all(var1l$value, "\\ ", ""))
var1l$value <- as.numeric(levels(var1l$value))[var1l$value]
var1l$year <- as.factor(str_replace_all(var1l$year, "y", ""))
var1l$year <- as.numeric(levels(var1l$year))[var1l$year]
var1l$Country <- as.character(var1l$Country)
dat1l <- fillCountryCode("Country", var1l)
missFAOcode <- as.character(unique(dat1l[is.na(dat1l$FAOST_CODE), "Country"]))
# 
manualFAOcode <- c(35, 357, 120, 276,231)
manual.df <- data.frame(Country = missFAOcode, NEW_FAOST_CODE = manualFAOcode)
dat1l <- merge(dat1l, manual.df, by = "Country", all.x = TRUE)
rm(list = c("missFAOcode", "manualFAOcode", "manual.df"))
dat1l$FAOST_CODE <- ifelse(is.na(dat1l$FAOST_CODE), dat1l$NEW_FAOST_CODE, dat1l$FAOST_CODE)
wrpc.df <- dat1l[, c("FAOST_CODE", "year", "value")]
names(wrpc.df) <- c("FAOST_CODE", "Year", "AQ.WAT.WATPCP.MC.NO")


# AQ.WAT.IRRPOT.HA.NO ---------------------------------------------------------

xls1 <- read.xls("./input_data/raw/AquastatManualData//SYB_AQUASTAT_Water_Tables_20150304.xls", sheet = 1)
var1 <- xls1[-1:-3,c(1,9)]
names(var1) <- c("Country","y2012")
var1l <- gather(var1,year,value,2)
var1l$value <- as.factor(str_replace_all(var1l$value, "\\ ", ""))
var1l$value <- as.numeric(levels(var1l$value))[var1l$value]
var1l$year <- as.factor(str_replace_all(var1l$year, "y", ""))
var1l$year <- as.numeric(levels(var1l$year))[var1l$year]
dat1l <- fillCountryCode("Country", var1l)
missFAOcode <- as.character(unique(dat1l[is.na(dat1l$FAOST_CODE), "Country"]))
# Cape Verde, China, Laos and Sudan
manualFAOcode <- c(35, 357, 120, 276,231)
manual.df <- data.frame(Country = missFAOcode, NEW_FAOST_CODE = manualFAOcode)
dat1l <- merge(dat1l, manual.df, by = "Country", all.x = TRUE)
rm(list = c("missFAOcode", "manualFAOcode", "manual.df"))
dat1l$FAOST_CODE <- ifelse(is.na(dat1l$FAOST_CODE), dat1l$NEW_FAOST_CODE, dat1l$FAOST_CODE)
ip.df <- dat1l[, c("FAOST_CODE", "year", "value")]
names(ip.df) <- c("FAOST_CODE", "Year", "AQ.WAT.IRRPOT.HA.NO")


# AQ.WAT.EQIRR.HA.NO ---------------------------------------------------------

xls1 <- read.xls("./input_data/raw/AquastatManualData/SYB_AQUASTAT_Water_Tables_20150304.xls", sheet = 1)
var1 <- xls1[-1:-3,c(1,10)]
names(var1) <- c("Country","y2012")
var1l <- gather(var1,year,value,2)
var1l$value <- as.factor(str_replace_all(var1l$value, "\\ ", ""))
var1l$value <- as.numeric(levels(var1l$value))[var1l$value]
var1l$year <- as.factor(str_replace_all(var1l$year, "y", ""))
var1l$year <- as.numeric(levels(var1l$year))[var1l$year]
dat1l <- fillCountryCode("Country", var1l)
missFAOcode <- as.character(unique(dat1l[is.na(dat1l$FAOST_CODE), "Country"]))
# Cape Verde, China, Laos and Sudan
manualFAOcode <- c(35, 357, 120, 276,231)
manual.df <- data.frame(Country = missFAOcode, NEW_FAOST_CODE = manualFAOcode)
dat1l <- merge(dat1l, manual.df, by = "Country", all.x = TRUE)
rm(list = c("missFAOcode", "manualFAOcode", "manual.df"))
dat1l$FAOST_CODE <- ifelse(is.na(dat1l$FAOST_CODE), dat1l$NEW_FAOST_CODE, dat1l$FAOST_CODE)
tae.df <- dat1l[, c("FAOST_CODE", "year", "value")]
names(tae.df) <- c("FAOST_CODE", "Year", "AQ.WAT.EQIRR.HA.NO")

# AQ.WAT.SHIRR.HA.SH ---------------------------------------------------------

xls1 <- read.xls("./input_data/raw/AquastatManualData/SYB_AQUASTAT_Water_Tables_20150304.xls", sheet = 1)
peaai.df <- xls1[-1:-3,c(1,12:13)]
names(peaai.df) <- c("Country","Year","AQ.WAT.SHIRR.HA.SH")
peaai.df$Country <- as.character(peaai.df$Country)
peaai.df$Year <- as.numeric(levels(peaai.df$Year))[peaai.df$Year]
peaai.df$AQ.WAT.SHIRR.HA.SH <- as.numeric(levels(peaai.df$AQ.WAT.SHIRR.HA.SH))[peaai.df$AQ.WAT.SHIRR.HA.SH]
peaai.df <- fillCountryCode(country = "Country", data = peaai.df)
missFAOcode <- unique(peaai.df[is.na(peaai.df$FAOST_CODE), "Country"])
# Cape Verde, China, Laos and Sudan
manualFAOcode <- c(35, 357, 120, 276,231)
manual.df <- data.frame(Country = missFAOcode, NEW_FAOST_CODE = manualFAOcode)
# Also lets remove Cape Verde from the water data
peaai.df <- merge(peaai.df, manual.df, by = "Country", all.x = TRUE)
rm(list = c("missFAOcode", "manualFAOcode", "manual.df"))
peaai.df[is.na(peaai.df$FAOST_CODE), "FAOST_CODE"] <- 
  peaai.df[is.na(peaai.df$FAOST_CODE), "NEW_FAOST_CODE"]
peaai.df <- peaai.df[, c("FAOST_CODE", "Year", "AQ.WAT.SHIRR.HA.SH")]
peaai.df <- subset(peaai.df, subset = !is.na(Year))

# AQ.WAT.WW ------------------------------------------------------------------

xls1 <- read.xls("./input_data/raw/AquastatManualData/SYB_AQUASTAT_Water_Tables_20150304.xls", sheet = 2)
wwr.df <- xls1[-1:-2,c(1:5,7:8,10:11)]
names(wwr.df) <- c("Country","Year","var1","var2","var3","var4","var5","var6","var7")
wwr.df.l <- gather(wwr.df, key = "var", value = "value",3:9)
wwr.df.l$value <- as.factor(str_replace_all(wwr.df.l$value, "\\ ", ""))
wwr.df.l$value <- as.numeric(levels(wwr.df.l$value))[wwr.df.l$value]
wwr.df.l$Year <- as.numeric(wwr.df.l$Year)
wwr.df.l$var <- as.character(wwr.df.l$var)
wwr.df.l$var[wwr.df.l$var == "var1"] <- "AQ.WAT.WWAGR.MC.SH"
wwr.df.l$var[wwr.df.l$var == "var2"] <- "AQ.WAT.WWIND.MC.SH"
wwr.df.l$var[wwr.df.l$var == "var3"] <- "AQ.WAT.WWMUN.MC.SH"
wwr.df.l$var[wwr.df.l$var == "var4"] <- "AQ.WAT.WWTOT.MC.NO"
wwr.df.l$var[wwr.df.l$var == "var5"] <- "AQ.WAT.WWTOT.MC.SH"
wwr.df.l$var[wwr.df.l$var == "var6"] <- "AQ.WAT.RFRWTOT.MC.SH"
wwr.df.l$var[wwr.df.l$var == "var7"] <- "AQ.WAT.RFRWAGR.MC.SH"
wwr.df <- spread(wwr.df.l, key = "var", value = "value")
wwr.df <- fillCountryCode(country = "Country", data = wwr.df)
missFAOcode <- unique(wwr.df[is.na(wwr.df$FAOST_CODE), "Country"])
# Cape Verde, China, C\xf4te d'Ivoire, Laos and Sudan
manualFAOcode <- c(35, 357, 107, 120, 276,231)
manual.df <- data.frame(Country = missFAOcode, NEW_FAOST_CODE = manualFAOcode)
wwr.df <- merge(wwr.df, manual.df, by = "Country", all.x = TRUE)
rm(list = c("missFAOcode", "manualFAOcode", "manual.df"))
wwr.df$FAOST_CODE <- ifelse(is.na(wwr.df$FAOST_CODE), wwr.df$NEW_FAOST_CODE, wwr.df$FAOST_CODE)
# now we have three less variables, the absolute values of water withdrawals by sector
# wwrVar <- c("AQ.WAT.WWAGR.MC.NO", "AQ.WAT.WWAGR.MC.SH", "AQ.WAT.WWIND.MC.NO",
#            "AQ.WAT.WWIND.MC.SH", "AQ.WAT.WWMUN.MC.NO", "AQ.WAT.WWMUN.MC.SH", 
#            "AQ.WAT.WWTOT.MC.NO", "AQ.WAT.WWTOT.MC.SH", "AQ.WAT.RFRWTOT.MC.SH",
#            "AQ.WAT.RFRWAGR.MC.SH")
#"wwr.df <- wwr.df[, c("FAOST_CODE", "Year", wwrVar)]
wwr.df$NEW_FAOST_CODE <- NULL
wwr.df$Country <- NULL
wwr.df$FAOST_CODE <- as.integer(wwr.df$FAOST_CODE)
wwr.df <- subset(wwr.df, subset = !is.na(Year))

# For finding the equivalent FAOST_CODE for a country name
# FAOcountryProfile[grep(pattern = "Ivoire",x = FAOcountryProfile$FAO_TABLE_NAME) ,c("FAOST_CODE","FAO_TABLE_NAME","OFFICIAL_FAO_NAME")]


# Merge -------------------------------------------------------------------

AquastatManualData.df <- data.frame()
AquastatManualData.df <- 
  Reduce(function(x, y) merge(x, y, all = TRUE),
         x = list(wrpc.df, ip.df, tae.df, peaai.df, wwr.df))
rm(list = c("wrpc.df", "ip.df", "tae.df", "peaai.df", "wwr.df"))
str(AquastatManualData.df)
AquastatManualData.df$Year <- as.integer(AquastatManualData.df$Year)
save(x = AquastatManualData.df, file = "./input_data/processed/AquastatManualData.RData")
