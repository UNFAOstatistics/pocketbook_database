##' Sum function with predefined arguments 
##'
##' Function to be used in some other functions (e.g. apply).
##'
##' @param x The vector to be summed.

sum2 = function(x) {sum(x, na.rm = any(!is.na(x)))}