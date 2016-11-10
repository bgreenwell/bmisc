#' Simplify Column Types
#'
#' Force the class of each column in a data frame to be either 
#' \code{"numeric"} or \code{"factor"}.
#'
#' @param x A data frame.
#' @param fac Logical indicating whether or not to force columns with only a few 
#'   unique values to be a factor. Default is \code{TRUE}.
#' @param k Integer cutoff for deciding the maximum number of unique values a column can have 
#'   before coercing to a factor. Default is \code{2}.
#' @param ... Additional optional arguments. (Currently ignored.)
simplifyColumnTypes <- function(x, fac = TRUE, k = 2, ...) {
  
  # Loop through each column
  for (name in names(x)) {
    
    # Convert integer to numeric
    if (is.integer(x[[name]])) {
      x[[name]] <- as.numeric(x[[name]])
    }
    
    # Convert character to factor
    if (is.character(x[[name]])) {
      x[[name]] <- as.factor(x[[name]])
    }
    
    # Convert logical to factor
    if (is.logical(x[[name]])) {
      x[[name]] <- as.factor(x[[name]])
    }
    
    # Convert to factor if fewer than k unique values
    if (fac) {
      if (length(unique(x[[name]])) <= k) {
        x[[name]] <- as.factor(x[[name]])
      }      
    }

  }
  
  # Return data frame
  x
  
}
