#' Within Range
#' 
#' Check whether a set of values are within a given range.
#' 
#' @param x A number, or vector thereof.
#' @param lower Lower bound of range.
#' @param upper Upper bound of range.
#' @param strict Logical indicating whether or not to use strict inequalities
#'   when checking bounds. Default is \code{TRUE}.
#' @export
#' @examples
#' inrange(3, 0, 1)
#' mean(inrange(rnorm(1e+06), -3, 3))
inrange <- function(x, lower, upper, strict = TRUE) {
  stopifnot(is.numeric(x) || is.numeric(lower) || is.numeric(upper) || 
              is.logical(strict))
  if (strict) {
    ifelse(x > lower & x < upper, TRUE, FALSE)
  } else {
    ifelse(x >= lower & x <= upper, TRUE, FALSE)
  }
}