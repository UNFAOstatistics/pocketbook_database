#' Check logical.
#' 
#' This function checks whether the parameter is logical or not. 
#'
#' @param x The parameter \code{x}.

CheckLogical = function(x){
  if (!is.logical(x)) 
    stop("The parameter ", x, " has to be logical.")
}