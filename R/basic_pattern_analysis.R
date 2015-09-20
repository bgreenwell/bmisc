#' @keywords internal
get_pattern <- function(x) {
  if (is.na(x)) {
    return(x)
  } else {
    if (!is.character(x)) {
      x <- as.character(x)
    }
    x <- unlist(strsplit(x, split = ""))
    x[grep("[a-z]", x)] <- "a"
    x[grep("[A-Z]", x)] <- "A"
    x[grep("[0-9]", x)] <- "9"
  }
  paste(x, collapse = "")
}


#' Basic Pattern Analysis
#' 
#' Perform a basic pattern analysis
#' 
#' @param x A data frame or character vector.
#' @param unique_only Logical indicating wether or not to only show the unique
#'   patterns. Default is \code{FALSE}.
#' @param ... Additional optional arguments. (Currently ignored).
#' @export
#' @examples 
#' basic_pattern_analysis(iris)
#' basic_pattern_analysis(iris, unique_only = TRUE)
basic_pattern_analysis <- function(x, unique_only = FALSE, ...) {
  UseMethod("basic_pattern_analysis")
}


#' @export
basic_pattern_analysis.default <- function(x, unique_only = FALSE, ...) {
  if (unique_only) {
    # unique(sapply(x, get_pattern, ...))
    table(sapply(x, get_pattern, ...))
  } else {
    sapply(x, get_pattern, ...)
  }
}


#' @export
basic_pattern_analysis.data.frame <- function(x, unique_only = FALSE, ...) {
  z <- lapply(x, basic_pattern_analysis.default, unique_only = unique_only, ...)
  if (unique_only) {
    z
  } else {
    data.frame(z)
  }
}