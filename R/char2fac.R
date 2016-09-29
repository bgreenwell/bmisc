#' Convert Non-numeric Columns to Factors
#' 
#' Converts all non-numeric columns in \code{x} to factors.
#' 
#' @param x A vector.
#' @export
#' @examples 
#' d <- data.frame(x = 1:3, 
#'                 y = letters[1:3], 
#'                 z = c(TRUE, TRUE, FALSE), 
#'                 stringsAsFactors = FALSE)
#' columnTypes(d)
#' columnTypes(char2fac(d))
char2fac <- function(x) {
  factors <- names(which(sapply(x, function(y) !is.numeric(y))))
  for (fac in factors) {
    x[[fac]] <- as.factor(x[[fac]])
  }
  x
} 