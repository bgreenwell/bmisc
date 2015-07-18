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


#' @rdname clip
#' @method clip default
#' @export
clip.default <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < .min] <- .min
  if (!missing(.max)) x[x > .max] <- .max
  x
}


#' @rdname clip
#' @method clip matrix
#' @export
clip.matrix <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < .min] <- .min
  if (!missing(.max)) x[x > .max] <- .max
  x
}


#' @rdname clip
#' @method clip array
#' @export
clip.array <- function(x, .min, .max, ...) {
  if (!missing(.min)) x[x < .min] <- .min
  if (!missing(.max)) x[x > .max] <- .max
  x
}