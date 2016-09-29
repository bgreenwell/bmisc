#' Number of Missing Values
#' 
#' \code{num_nas} returns the number of \code{NA}s in \code{x}.
#' 
#' @param x A vector.
#' @export
nmissing <- function(x) {
  sum(is.na(x))
}


#' Number of Unique Values
#' 
#' \code{num_unique} returns the number of unique values in \code{x}.
#' 
#' @param x A vector.
#' @export
nunique <- function(x) {
  length(unique(x))
}
