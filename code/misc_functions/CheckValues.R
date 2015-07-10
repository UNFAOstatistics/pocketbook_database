##' Function to check for true values.
##'
##' This function simply replaces NaN, -Inf, and Inf values with NA.
##'
##' @param dataset The data frame containing the data \code{dataset}.
##' @param columns The columns in which we need to check for infinite 
##' values \code{columns}.
##' 
##' @return The dataset with NA instead of NaN, -Inf, Inf values.
##' 
##' @export

CheckValues = function(dataset, columns) {
  for (i in columns) {
    ind = which(is.nan(dataset[, i]), arr.ind = TRUE)
    dataset[ind, i] = NA
    ind = which(is.infinite(dataset[, i]), arr.ind = TRUE)
    dataset[ind, i] = NA
  }
  dataset
}
