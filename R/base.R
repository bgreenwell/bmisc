#' Hypotenuse length
#'
#' Calculate the length of the hypotenuse of a right-angled triangle.
#' 
#' @param x A numeric value representing one leg of the triangle.
#' 
#' @param y A numeric value representing the other leg of the triangle.
#' 
#' @return The length of the hypotenuse.
#' @export
hypot <- function(x, y) {
  x <- abs(x)
  y <- abs(y)
  x <- max(x, y)
  x * sqrt(1 + (min(x, y)/x)^2)
}


#' Radians to degrees
#' 
#' Convert angles from radians to degrees.
#' 
#' @param x A numeric vector.
#' 
#' @return A numeric vector.
#' 
#' @export
rad2deg <- function(x) {
  180 * x / pi
}


#' Angles to radians
#' 
#' Convert angles from degrees to radians.
#' 
#' @param x A numeric vector.
#' 
#' @return A numeric vector.
#' 
#' @export
deg2rad <- function(x) {
  x * pi / 180
}


#' Within range
#' 
#' Check whether a set of values are within a given range.
#' 
#' @param x A number, or vector thereof.
#' 
#' @param lower Lower bound of range.
#' 
#' @param upper Upper bound of range.
#' 
#' @param strict Logical indicating whether or not to use strict inequalities
#' when checking bounds. Default is \code{TRUE}.
#' 
#' @export
#' 
#' @examples
#' inrange(3, 0, 1)
#' mean(inrange(rnorm(1e+06), -3, 3))
inrange <- function(x, lower, upper, strict = TRUE) {
  stopifnot(is.numeric(x) || is.numeric(lower) || is.numeric(upper) || 
              is.logical(strict))
  if (strict) {
    x > lower & x < upper
  } else {
    x >= lower & x <= upper
  }
}
