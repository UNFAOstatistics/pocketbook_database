###########################################################################
## Title: Script to process the biofuel production variables
## Updated: 14/07/2014
## Note: This data can be downloaded from www.data.un.org. See the mail
## sent by Amy on 11/07/2014
###########################################################################

# BP.AP.GP.MT.NO ----------------------------------------------------------

gap.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/AlcoholProduction.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
gap.df <-
  gap.df[, c("Country.or.Area.Code", "Year", "Quantity")]
gap.df <-
  translateCountryCode(gap.df, from = "UN_CODE", to = "FAOST_CODE",
                               oldCode = "Country.or.Area.Code")
gap.df <- gap.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(gap.df) <- c("FAOST_CODE", "Year", "BP.AP.GP.MT.NO")

# BP.AP.CNEU.MT.NO --------------------------------------------------------

acneu.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/AlcoholConsumptionForNonEnergyUses.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
acneu.df <-
  acneu.df[, c("Country.or.Area.Code", "Year", "Quantity")]
acneu.df <-
  translateCountryCode(acneu.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
acneu.df <- acneu.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(acneu.df) <- c("FAOST_CODE", "Year", "BP.AP.CNEU.MT.NO")

# BP.AP.GP.MT.NO - BP.AP.CNEU.MT.NO ---------------------------------------

gap.df <-
  merge(gap.df, acneu.df, by = c("FAOST_CODE", "Year"), all = TRUE)
gap.df[is.na(gap.df[, "BP.AP.CNEU.MT.NO"]), "BP.AP.CNEU.MT.NO"] <- 0
gap.df[, "BP.AP.GP.MT.NO"] <-
  gap.df[, "BP.AP.GP.MT.NO"] - gap.df[, "BP.AP.CNEU.MT.NO"]
gap.df <- gap.df[, c("FAOST_CODE", "Year", "BP.AP.GP.MT.NO")]
rm(acneu.df)
## convert to terajoules
gap.df[, "BP.AP.GP.TJ.NO"] <- gap.df[, "BP.AP.GP.MT.NO"] * 27
gap.df <- gap.df[, c("FAOST_CODE", "Year", "BP.AP.GP.TJ.NO")]

# BP.BP.GP.MT.NO ----------------------------------------------------------

gbp.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/BagasseProduction.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
gbp.df <-
  gbp.df[, c("Country.or.Area.Code", "Year", "Quantity")]
gbp.df <-
  translateCountryCode(gbp.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
gbp.df <- gbp.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(gbp.df) <- c("FAOST_CODE", "Year", "BP.BP.GP.MT.NO")

# BP.BP.CNEU.MT.NO --------------------------------------------------------

bcneu.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/BagasseConsumptionForNonEnergyUses.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
bcneu.df <-
  bcneu.df[, c("Country.or.Area.Code", "Year", "Quantity")]
bcneu.df <-
  translateCountryCode(bcneu.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
bcneu.df <- bcneu.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(bcneu.df) <- c("FAOST_CODE", "Year", "BP.BP.CNEU.MT.NO")

# BP.BP.GP.MT.NO - BP.BP.CNEU.MT.NO ---------------------------------------

gbp.df <-
  merge(gbp.df, bcneu.df, by = c("FAOST_CODE", "Year"), all = TRUE)
gbp.df[is.na(gbp.df[, "BP.BP.CNEU.MT.NO"]), "BP.BP.CNEU.MT.NO"] <- 0
gbp.df[, "BP.BP.GP.MT.NO"] <-
  gbp.df[, "BP.BP.GP.MT.NO"] - gbp.df[, "BP.BP.CNEU.MT.NO"]
gbp.df <- gbp.df[, c("FAOST_CODE", "Year", "BP.BP.GP.MT.NO")]
rm(bcneu.df)
## convert to terajoules
gbp.df[, "BP.BP.GP.TJ.NO"] <- gbp.df[, "BP.BP.GP.MT.NO"] * 7.7221
gbp.df <- gbp.df[, c("FAOST_CODE", "Year", "BP.BP.GP.TJ.NO")]

# BP.BDP.GP.MT.NO ---------------------------------------------------------

gbdp.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/BiodiselGrossProduction.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
gbdp.df <-
  gbdp.df[, c("Country.or.Area.Code", "Year", "Quantity")]
gbdp.df <-
  translateCountryCode(gbdp.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
gbdp.df <- gbdp.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(gbdp.df) <- c("FAOST_CODE", "Year", "BP.BDP.GP.MT.NO")
## convert to terajoules
gbdp.df[, "BP.BDP.GP.TJ.NO"] <- gbdp.df[, "BP.BDP.GP.MT.NO"] * 27
gbdp.df <- gbdp.df[, c("FAOST_CODE", "Year", "BP.BDP.GP.TJ.NO")]

# BP.BGP.GP.TJ.NO ---------------------------------------------------------

gbgp.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/BiogasGrossProduction.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
gbgp.df <-
  gbgp.df[, c("Country.or.Area.Code", "Year", "Quantity")]
gbgp.df <-
  translateCountryCode(gbgp.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
gbgp.df <- gbgp.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(gbgp.df) <- c("FAOST_CODE", "Year", "BP.BGP.GP.TJ.NO")

# BP.VWP.GP.MT.NO ---------------------------------------------------------

gvp.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/VegetalProduction.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
gvp.df <-
  gvp.df[, c("Country.or.Area.Code", "Year", "Quantity")]
gvp.df <-
  translateCountryCode(gvp.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
gvp.df <- gvp.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(gvp.df) <- c("FAOST_CODE", "Year", "BP.VWP.GP.MT.NO")

# BP.VWP.CNEU.MT.NO -------------------------------------------------------

vwcneu.df <-
  read.csv(file = "./input_data/raw/BiodiselProduction/VegetalConsumptionForNonEnergyUses.csv",
           header = TRUE, na.strings = "", stringsAsFactors = FALSE)
vwcneu.df <-
  vwcneu.df[, c("Country.or.Area.Code", "Year", "Quantity")]
vwcneu.df <-
  translateCountryCode(vwcneu.df, from = "UN_CODE", to = "FAOST_CODE",
                       oldCode = "Country.or.Area.Code")
vwcneu.df <- vwcneu.df[, c("FAOST_CODE", "Year", "Quantity")]
colnames(vwcneu.df) <- c("FAOST_CODE", "Year", "BP.VWP.CNEU.MT.NO")

# BP.VWP.GP.MT.NO - BP.VWP.CNEU.MT.NO -------------------------------------

gvp.df <-
  merge(gvp.df, vwcneu.df, by = c("FAOST_CODE", "Year"), all = TRUE)
gvp.df[is.na(gvp.df[, "BP.VWP.CNEU.MT.NO"]), "BP.VWP.CNEU.MT.NO"] <- 0
gvp.df[, "BP.VWP.GP.MT.NO"] <-
  gvp.df[, "BP.VWP.GP.MT.NO"] - gvp.df[, "BP.VWP.CNEU.MT.NO"]
gvp.df <- gvp.df[, c("FAOST_CODE", "Year", "BP.VWP.GP.MT.NO")]
rm(vwcneu.df)
## convert to terajoules
gvp.df[, "BP.VWP.GP.TJ.NO"] <- gvp.df[, "BP.VWP.GP.MT.NO"] * 12.5
gvp.df <- gvp.df[, c("FAOST_CODE", "Year", "BP.VWP.GP.TJ.NO")]

# merge -------------------------------------------------------------------

BiofuelProduction.df <- data.frame()
BiofuelProduction.df <-
  Reduce(function(x, y) merge(x, y, all = TRUE),
         x = list(gap.df, gbp.df, gbdp.df, gbgp.df, gvp.df))
BiofuelProduction.df[, "BP.TP.GP.TJ.NO"] <-
  rowSums(BiofuelProduction.df[, c("BP.AP.GP.TJ.NO", "BP.BP.GP.TJ.NO",
                                   "BP.BDP.GP.TJ.NO", "BP.BGP.GP.TJ.NO",
                                   "BP.VWP.GP.TJ.NO")], na.rm = TRUE)
rm(list = c("gap.df", "gbp.df", "gbdp.df", "gbgp.df", "gvp.df"))
save(x = BiofuelProduction.df, file = paste0("./input_data/processed/BiofuelProduction.RData"))
