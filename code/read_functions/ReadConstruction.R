##' A function to read in the construction input file.
##'
##' The function reads in the file containing information concerning 
##' aggregation and construction rules.
##' The file has to be structured in the following way:
##' a) STS_ID, the sequence of characters identifying the Space-Time Series 
##' with which it is associated.
##' b) STS_ID_CONSTR1, the first indicator used for construction.
##' c) STS_ID_CONSTR2, the second indicator used for construction.
##' d) STS_ID_WEIGHT, the indicator used as weight in the aggregation.
##' e) CONSTRUCTION_TYPE, the type of construction (share, growth, change, index).
##' f) GROWTH_RATE_FREQ, the frequency of the growth rate (e.g. 1 for annual, 10
##' for 10 year growth rate)
##' g) GROWTH_TYPE, method used to compute the growth rate 
##' (geo = geometric, ls = least squares)
##' h) AGGREGATION, aggregation method.
##' i) NAtoZERO, whether or not NA values correspond to true zeros.
##' j) SCALING, adjustment in scaling (means variable times SCALING).
##' k) COMMENT, additional comments.
##' 
##' @param file The name of the construction rule file \code{file}.
##' @param ... Additional arguments, see read.csv.
##' 
##' @export

ReadConstruction = function(file = "construct.csv", ...){
  construct = read.csv(file = file, stringsAsFactors = FALSE,
    na.string = "", header = TRUE, ...)
  construct
}
