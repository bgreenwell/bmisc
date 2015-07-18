#' Miscelaneous math functions
#'
#' Calculate the length of the hypotenuse of a right-angled triangle.
#' 
#' @param x A numeric value representing one leg of the triangle.
#' @param y A numeric value representing the other leg of the triangle.
#' @return The length of the hypotenuse.
#' @export
hypot <- function(x, y) {
  x <- abs(x)
  y <- abs(y)
  x <- max(x, y)
  x * sqrt(1 + (min(x, y)/x)^2)
}

#' Miscelaneous math functions
#' 
#' Convert angles from radians to degrees.
#' 
#' @param x A numeric vector.
#' @return A numeric vector.
#' @export
rad2deg <- function(x) {
  180 * x / pi
}

#' Miscelaneous math functions
#' 
#' Convert angles from degrees to radians.
#' 
#' @param x A numeric vector.
#' @return A numeric vector.
#' @export
deg2rad <- function(x) {
  x * pi / 180
}