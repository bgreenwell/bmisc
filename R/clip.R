#' Clip Values
#'
#' Clip (i.e., limit) the values in a vector, matrix, or array.
#' 
#' @param x A vector, matrix, or multi-way array.
#' @param .min .minimum value.
#' @param .max .maximum value.
#' @param ... Additional optional arguments.
#' @export
clip <- function(x, .min, .max, ...) {
  UseMethod("clip")
}


#' @export
clip.integer <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < interval[1L]] <- interval[1L]
  if (!missing(.max)) x[x > interval[2L]] <- interval[2L]
  x
}


#' @export
clip.numeric <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < interval[1L]] <- interval[1L]
  if (!missing(.max)) x[x > interval[2L]] <- interval[2L]
  x
}


#' @export
clip.matrix <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < interval[1L]] <- interval[1L]
  if (!missing(.max)) x[x > interval[2L]] <- interval[2L]
  x
}


#' @export
clip.array <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < interval[1L]] <- interval[1L]
  if (!missing(.max)) x[x > interval[2L]] <- interval[2L]
  x
}