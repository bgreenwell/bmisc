#' Most Frequent Value
#' 
#' \code{most_freq} returns the most frequent value in \code{x}.
#' 
#' @param x A vector.
#' @export
most_freq <- function(x) {
  names(which.max(table(x, useNA = "always")))
}


#' 
cv <- function(x, na.rm = FALSE, trim = 0) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm, trim = trim)
}