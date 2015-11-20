###########################################################################
## This script processes those variables that cannot be automatically 
## downloaded from WDI
###########################################################################

# CP.D.ATLAS.GNP ----------------------------------------------------------

CP.D.ATLAS.GNP.df <- 
  read.csv(file = "./input_data/raw/WBManualData/CP.D.ATLAS.GNP.csv", 
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
CP.D.ATLAS.GNP.df <- 
  data.frame(Country.Code = rep(CP.D.ATLAS.GNP.df$Country_Code, 16),
             Year = rep(2000:2015, each = 210),
             CP.D.ATLAS.GNP = rep(CP.D.ATLAS.GNP.df$Weights, 16))
CP.D.ATLAS.GNP.df <- 
  translateCountryCode(data = CP.D.ATLAS.GNP.df, from = "ISO3_WB_CODE",
                       to = "FAOST_CODE", oldCode = "Country.Code")
CP.D.ATLAS.GNP.df <- 
  CP.D.ATLAS.GNP.df[, c("FAOST_CODE", "Year", "CP.D.ATLAS.GNP")]

# Employment --------------------------------------------------------------

em.df <- 
  read.csv(file = "./input_data/raw/WBManualData/Employment.csv", header = TRUE, 
           na.strings = "", stringsAsFactors = FALSE)
colnames(em.df) <- 
  c("Country.Code", "Country.Name", "Series.Code", "Series.Name",
    paste("X", 1960:2011, sep = ""))
em.df <- 
  translateCountryCode(data = em.df, from = "ISO3_WB_CODE",
                       to = "FAOST_CODE", oldCode = "Country.Code")
em.df <- subset(em.df, !is.na(FAOST_CODE))
SL.EMP.TOTL.FE.df <- subset(em.df, subset = Series.Code == "SL.EMP.TOTL.FE")
SL.EMP.TOTL.MA.df <- subset(em.df, subset = Series.Code == "SL.EMP.TOTL.MA")
SL.EMP.TOTL.df <- subset(em.df, subset = Series.Code == "SL.EMP.TOTL")
rm(em.df)
SL.EMP.TOTL.FE.df <- SL.EMP.TOTL.FE.df[, -c(1,3,4,5)]
SL.EMP.TOTL.MA.df <- SL.EMP.TOTL.MA.df[, -c(1,3,4,5)]
SL.EMP.TOTL.df <- SL.EMP.TOTL.df[, -c(1,3,4,5)]
## remove ".." and coherce to numeric
indexes = which(SL.EMP.TOTL.FE.df == "..", arr.ind = TRUE)
for (i in 1:nrow(indexes)) {
  SL.EMP.TOTL.FE.df[indexes[i,1], indexes[i,2]] = ""
}
for (i in 1:ncol(SL.EMP.TOTL.FE.df)) {
  SL.EMP.TOTL.FE.df[, i] = as.numeric(SL.EMP.TOTL.FE.df[, i])
}
indexes = which(SL.EMP.TOTL.MA.df == "..", arr.ind = TRUE)
for (i in 1:nrow(indexes)) {
  SL.EMP.TOTL.MA.df[indexes[i,1], indexes[i,2]] = ""
}
for (i in 1:ncol(SL.EMP.TOTL.MA.df)) {
  SL.EMP.TOTL.MA.df[, i] = as.numeric(SL.EMP.TOTL.MA.df[, i])
}
indexes = which(SL.EMP.TOTL.df == "..", arr.ind = TRUE)
for (i in 1:nrow(indexes)) {
  SL.EMP.TOTL.df[indexes[i,1], indexes[i,2]] = ""
}
for (i in 1:ncol(SL.EMP.TOTL.df)) {
  SL.EMP.TOTL.df[, i] = as.numeric(SL.EMP.TOTL.df[, i])
}
rm(list = c("i", "indexes"))
## carry forward the 2010 values
SL.EMP.TOTL.FE.df[, c("X2011", "X2012", "X2013")] = SL.EMP.TOTL.FE.df[, c("X2010")]
SL.EMP.TOTL.MA.df[, c("X2011", "X2012", "X2013")] = SL.EMP.TOTL.MA.df[, c("X2010")]
SL.EMP.TOTL.df[, c("X2011", "X2012", "X2013")] = SL.EMP.TOTL.df[, c("X2010")]
## melt
SL.EMP.TOTL.FE.df <- melt(SL.EMP.TOTL.FE.df, id = "FAOST_CODE")
SL.EMP.TOTL.MA.df <- melt(SL.EMP.TOTL.MA.df, id = "FAOST_CODE")
SL.EMP.TOTL.df <- melt(SL.EMP.TOTL.df, id = "FAOST_CODE")

colnames(SL.EMP.TOTL.FE.df) <- c("FAOST_CODE", "Year", "SL.EMP.TOTL.FE")
colnames(SL.EMP.TOTL.MA.df) <- c("FAOST_CODE", "Year", "SL.EMP.TOTL.MA")
colnames(SL.EMP.TOTL.df) <- c("FAOST_CODE", "Year", "SL.EMP.TOTL")
## convert X to empty and coerce to numeric.
SL.EMP.TOTL.FE.df[, "Year"] <- as.numeric(gsub("X", "", SL.EMP.TOTL.FE.df[, "Year"]))
SL.EMP.TOTL.MA.df[, "Year"] <- as.numeric(gsub("X", "", SL.EMP.TOTL.MA.df[, "Year"]))
SL.EMP.TOTL.df[, "Year"] <- as.numeric(gsub("X", "", SL.EMP.TOTL.df[, "Year"]))

# MUV ---------------------------------------------------------------------

## data http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTDECPROSPECTS/0,,contentMDK:20587651~menuPK:3279864~pagePK:64165401~piPK:64165026~theSitePK:476883,00.html

MUV.df <- read.csv("./input_data/raw/WBManualData/MUV06-01-2014.csv", na.strings = "", 
                   header = TRUE, stringsAsFactors = FALSE)
codes <- subset(FAOcountryProfile, subset = M49 %in% "YES")[, "FAOST_CODE"]
MUV.df <- data.frame(FAOST_CODE = rep(codes, times = length(unique(MUV.df$Year))),
                    Year = rep(unique(MUV.df$Year), each = length(codes)),
                    MUV = rep(MUV.df$MUV, each = length(codes)))
MUV.df$MUV <- MUV.df$MUV/100

# Merge -------------------------------------------------------------------

WBManualData.df <- data.frame()
WBManualData.df <- 
  Reduce(function(x, y) merge(x, y, all = TRUE),
         x = list(CP.D.ATLAS.GNP.df, SL.EMP.TOTL.FE.df, SL.EMP.TOTL.MA.df, 
                  SL.EMP.TOTL.df, MUV.df))
rm(list = c("CP.D.ATLAS.GNP.df", "SL.EMP.TOTL.FE.df", "SL.EMP.TOTL.MA.df", 
            "SL.EMP.TOTL.df", "MUV.df"))
save(x = WBManualData.df, file = "./input_data/processed/wbManualData.RData")
