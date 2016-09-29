#' Check Class Balance
#' 
#' Calculate the ratio of the most common class to the least common class
#' 
#' @param y A vector of class labels.
#' @param na.rm Logical indicating whether or not to remove \code{NA} values
#'   before computation proceeds.
#' @param ... Additional optional arguments to be passed onto \code{table}.
#' @return Returns the ratio of the frequencies for the largest class and the 
#'   smallest class.
checkClassBalance <- function(y, na.rm = FALSE, ...) {
  tab <- table(y)
  max(tab, na.rm = na.rm) / min(tab, na.rm = na.rm)
}
