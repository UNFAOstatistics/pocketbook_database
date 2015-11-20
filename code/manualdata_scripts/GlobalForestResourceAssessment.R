###########################################################################
## Title: This script process the Global Forest Resources Assessment data
## Created by: Filippo Gheri
## Created: 07/07/2014
## Updated: 07/07/2014
###########################################################################

gfra.df <- 
  read.csv(file = "./Data/Raw/ForestCharacteristics.csv", 
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
gfra.df <- gfra.df[, c("FRA.categories", "Year", "Country", "Value")]
gfra.df <- fillCountryCode(country = "Country", data = gfra.df)
missFAOcode <- unique(gfra.df[is.na(gfra.df$FAOST_CODE), "Country"])
manualFAOcode <- c(19, 35, 351, 120, 206, 231)
manual.df <- data.frame(Country = missFAOcode, NEW_FAOST_CODE = manualFAOcode)
tmp <- merge(gfra.df, manual.df, by = "Country", all.x = TRUE)
tmp[is.na(tmp$FAOST_CODE), "FAOST_CODE"] <- 
  tmp[is.na(tmp$FAOST_CODE), "NEW_FAOST_CODE"]
onrf.df <- subset(tmp, 
                 select = c("FAOST_CODE", "Year", "Value"),
                 subset = FRA.categories == "Other nataturally regenerated forest")
colnames(onrf.df)[3] <- "GFRA.TOT.ONRF.HA.NO"
prfor.df <- subset(tmp, 
                  select = c("FAOST_CODE", "Year", "Value"),
                  subset = FRA.categories == "Primary forest")
colnames(prfor.df)[3] <- "GFRA.TOT.PF.HA.NO"
plfor.df <- subset(tmp, 
                  select = c("FAOST_CODE", "Year", "Value"),
                  subset = FRA.categories == "Planted forest")
colnames(plfor.df)[3] <- "GFRA.TOT.PLF.HA.NO"
totfor.df <- ddply(tmp, .(FAOST_CODE, Year), numcolwise(sum), na.rm = TRUE)[, c(1:3)]
colnames(totfor.df)[3] <- "GFRA.TOT.TOT.HA.NO"

# Merge -------------------------------------------------------------------

gfra.df <- data.frame()
gfra.df <- 
  Reduce(function(x, y) merge(x, y, all = TRUE),
         x = list(onrf.df, prfor.df, plfor.df, totfor.df))
rm(list = c("onrf.df", "prfor.df", "plfor.df", "totfor.df", "missFAOcode",
            "manualFAOcode", "manual.df", "tmp"))
save(x = gfra.df, file = "./Data/Processed/GlobalForestResourceAssessment.RData")
