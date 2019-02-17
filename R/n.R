#' Number of unique elements
#' 
#' Return the number of unique values which its argument has.
#' 
#' @param x A vector, matrix, or data frame.
#' 
#' @rdname nunique
#' 
#' @export
#' 
#' @examples 
#' nunique(c(1, 1, 2))
#' nunique(iris$Species)
nunique <- function(x) {
  UseMethod("nunique")
}


#' @rdname nunique
#' 
#' @export
nunique.default <- function(x) {
  length(unique(x))
}


#' @rdname nunique
#'
#' @export
nunique.matrix <- function(x) {
  nrow(unique(x))
}


#' @rdname nunique
#' 
#' @export
nunique.data.frame <- function(x) {
  nrow(unique(x))
}


#' Number of Missing Values
#' 
#' \code{num_nas} returns the number of \code{NA}s in \code{x}.
#' 
#' @param x A vector.
#' 
#' @export
nmissing <- function(x) {
  sum(is.na(x))
}
