#' Statistical Mode
#' 
#' Compute the mode (i.e., most frequent element).
#' 
#' @param x A vector of numbers or characters.
#' @param na.rm Logical indicating whether \code{NA} values should be stripped
#'   before the computation prceeds.
#' @export
statmode <- function(x, na.rm = TRUE) {
  # FIXME: How to handle ties?
  if (na.rm) x <- na.omit(x)
  x[which.max(table(x))]
}