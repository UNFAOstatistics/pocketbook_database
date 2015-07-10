##' A function to read in the meta data input file
##'
##' The function reads in the file containing information about the
##' Space_Time series used and the download path. The file has to be structured
##' in the following way: 
##' a) STS_ID, the sequence of characters identifying the Space-Time Series 
##' with which it is associated.
##' b) TITLE_STS, textual label used as identification of the Space-Time Series.
##' c) TITLE_STS_SHORT, the abbreviated textual label.
##' d) UNIT_MULT, exponent in base 10 specified so that multiplying the
##' observation numeric values by 10^UNIT_MULT gives a value expressed in the unit
##' of measure. This specifically refers to the multiplier of the downloaded data.
##' e) UNIT_MEASURE, the unit in which the data values are measured.
##' f) DEFINITION, the definition of the Space-Time Series.
##' g) OWNER, the owner or provider of the data.
##' h) SOURCE, the original source of the data.
##' i) DATA_TYPE, Whether the data is download as raw or constructed
##' based on the construction file.
##' j) SQL_DOMAIN_CODE, the FAOSTAT domain code used to specify the
##' download path for FAO data.
##' k) SQL_ELEMENT_CODE, the FAOSTAT element code used to specify the
##' download path for FAO data.
##' l) SQL_ITEM_CODE, the FAOSTAT domain code used to specify the
##' download path for FAO data.
##' m) WDINAME, the indicator name used to specify the download path for
##' the World Bank Development Index data.
##' n) COMMENT, additional comments (text).
##' o) TOPIC
##'
##' @param file The name of the file
##' @param ... Additional arguments, see read.csv.
##' @return The function reads the input file and returns a list
##' containing 4 data frames.
##' \itemize{
##'   \item The FULL file.
##'   \item The WDI subset used for downloading World Bank data.
##'   \item The FAOSTAT subset used for downloading FAOSTAT data.
##'   \item The UNIT subset which is used to scale the data to base unit.
##' }
##' @export
ReadMetadata = function(file, ...){
  meta = read.csv(file = file, stringsAsFactors = FALSE,
    na.string = "", header = TRUE, ...)
  rawMeta = subset(meta, DATA_TYPE == "raw")

  WDI = subset(rawMeta, select = c("STS_ID", "WDINAME"),
    subset = SOURCE == "World Bank (WDI)")
  FAOSTAT = subset(rawMeta, select = c("STS_ID", "SQL_DOMAIN_CODE",
                           "SQL_ELEMENT_CODE", "SQL_ITEM_CODE"),
    subset = SOURCE == "FAO, Statistics Division (FAOSTAT)" & DATA_TYPE == "raw")
  OTHER = subset(rawMeta, !(SOURCE %in%
    c("World Bank (WDI)", "FAO, Statistics Division (FAOSTAT)")))
  UNIT_MULT= subset(meta, select = c("STS_ID", "UNIT_MULT"))
  list(FULL = meta, WDI = WDI, FAOSTAT = FAOSTAT, OTHER = OTHER,
       UNIT_MULT = UNIT_MULT)
}
