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
    x > lower & x < upper
  } else {
    x >= lower & x <= upper
  }
}


#' The Number of Unique Elements
#' 
#' Return the number of unique values which its argument has.
#' 
#' @param x A vector, matrix, or data frame.
#' @export
nunique <- function(x) {
  UseMethod("nunique")
}


#' @rdname nunique
#' @method nunique default
#' @export
nunique.default <- function(x) {
  length(unique(x))
}


#' @rdname nunique
#' @method nunique matrix
#' @export
nunique.matrix <- function(x) {
  nrow(unique(x))
}


#' @rdname nunique
#' @method nunique data.frame
#' @export
nunique.data.frame <- function(x) {
  nrow(unique(x))
}


#' Convert Characters to Integers
#'
#' Convert one-dimensional character objects to integers.
#' 
#' @param string A character vector.
#' @return An integer (not necessarily unique).
#' @export
#' @examples
#' # Should all produce the same integer (currently results in 124)
#' str2int("Hello World")
#' str2int("hello world")
#' str2int("hello   world")
#' str2int("hello.world")
#' str2int("hello...world")
#' str2int("hello-world")
#' str2int("hello - world")
#' str2int("hello---world")
#' str2int("helloworld")
#' str2int("HelloWorld")
#' str2int("H._.e L. - lo -.  W__or l- .. d")
#' 
#' set.seed(str2int("Some random numbers"))
#' rnorm(3)  # should give: 1.7500983 -0.1093635 -0.9958618
#' set.seed(str2int("Some more random numbers")) 
#' rnorm(3)  # should give: 0.007765185 -1.138536203  0.091017129
#' 
#' # Potential issues
#' str2int("abc")  # does not provide unique integers
#' str2int("cba")
str2int <- function(string) {
  string <- tolower(string)
  all_chars <- c(letters, " ", "\\.", "-", "_")
  all_numbers <- c(seq_len(length(all_chars) - 4), 0, 0, 0, 0)
  chars <- unlist(strsplit(x = string, split = ""))
  chars[chars == "."] <- "\\."
  chars[chars == "-"] <- "\\-" 
  chars[chars == "_"] <- "\\_" 
  id <- sapply(chars, grep, x = all_chars)
  nums <- all_numbers[id]
  seed <- sum(nums)
  min(c(seed, .Machine$integer.max))
}


#' Specify Seeds
#'
#' Specify seeds using character vector.
#' 
#' @param string A character vector.
#' @return Returns NULL, invisibly.
#' @export
#' @examples
#' set_seed("Some random numbers")
#' rnorm(3)  # should give: 1.7500983 -0.1093635 -0.9958618
set_seed <- function(string) {
  set.seed(str2int(string))
}