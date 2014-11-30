##' Miscelaneous math functions
##'
##' Calculate the length of the hypotenuse of a right-angle triangle.
##' 
##' @param x
##' @param y
##' 
##' @return The length of the hypotenuse.
hypot <- function(x, y) {
  x <- abs(x)
  y <- abs(y)
  x <- max(x, y)
  x * sqrt(1 + (min(x, y)/x)^2)
}

##' Miscelaneous math functions
##' 
##' Convert angles from radians to degrees.
##' 
##' @param x A numeric vector.
##' 
##' @return A numeric vector.
rad2deg <- function(x) {
  180 * x / pi
}

##' Miscelaneous math functions
##' 
##' Convert angles from degrees to radians.
##' 
##' @param x A numeric vector.
##' 
##' @return A numeric vector.
deg2rad <- function(x) {
  x * pi / 180
}