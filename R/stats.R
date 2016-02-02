#' Most Frequent Value
#' 
#' \code{most_freq} returns the most frequent value in \code{x}.
#' 
#' @param x A vector.
#' @export
most_freq <- function(x) {
  names(which.max(table(x, useNA = "always")))
}


#' Coefficient of Variation
#' 
#' Calculate the sample coefficient of variation for a vector of data.
#' 
#' @param x A numeric vector.
#' @param na.rm Logical indicating whether or not \code{NA} values should be 
#'   stripped before the computation proceeds.
cv <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm, trim = trim)
}