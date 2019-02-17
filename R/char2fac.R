#' Convert non-numeric columns to factors
#' 
#' Converts all non-numeric columns in a data frame to factors.
#' 
#' @param x A data frame.
#' 
#' @export
#' 
#' @examples 
#' # Example data frame
#' d <- data.frame(
#'   x = 1:3, 
#'   y = letters[1:3], 
#'   z = c(TRUE, TRUE, FALSE), 
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Check column types with `col_types()`
#' col_types(d)
#' col_types(char2fac(d))
char2fac <- function(x) {
  if (!inherits(x, what = "data.frame")) {
    stop("This function only applies to data frames.", call. = FALSE)
  }
  factors <- names(which(sapply(x, function(y) !is.numeric(y))))
  for (fac in factors) {
    x[[fac]] <- as.factor(x[[fac]])
  }
  x
}
