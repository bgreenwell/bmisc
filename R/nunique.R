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
#' @S3method nunique default
#' @export
nunique.default <- function(x) {
  length(unique(x))
}


#' @rdname nunique
#' @S3method nunique matrix
#' @export
nunique.matrix <- function(x) {
  nrow(unique(x))
}


#' @rdname nunique
#' @S3method nunique data.frame
#' @export
nunique.data.frame <- function(x) {
  nrow(unique(x))
}
