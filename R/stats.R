#' Most Frequent Value
#' 
#' \code{most_freq} returns the most frequent value in \code{x}.
#' 
#' @param x A vector.
#' 
#' @export
#' 
#' @examples
#' most_freq(sample(c("a", "b", "c"), size = 100, replace = TRUE, 
#'                  prob = c(0.1, 0.3, 0.6)))
most_freq <- function(x) {
  names(which.max(table(x, useNA = "always")))
}


#' Coefficient of Variation
#' 
#' Calculate the sample coefficient of variation for a vector of data.
#' 
#' @param x A numeric vector.
#' 
#' @param na.rm Logical indicating whether or not \code{NA} values should be 
#' stripped before the computation proceeds.
#' 
#' @export
#' 
#' @examples 
#' cv(rnorm(100))
cv <- function(x, na.rm = FALSE) {
  stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}