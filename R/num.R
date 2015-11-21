#' Number of Missing Values
#' 
#' \code{num_nas} returns the number of \code{NA}s in \code{x}.
#' 
#' @param x A vector.
#' @export
num_nas <- function(x) {
  sum(is.na(x))
}


#' Number of Unique Values
#' 
#' \code{num_unique} returns the number of unique values in \code{x}.
#' 
#' @param x A vector.
#' @export
num_unique <- function(x) {
  length(unique(x))
}


#' Column Types
#' 
#' \code{column_types} returns a vector indicating the type (i.e., class) of 
#' each column of \code{x}.
#' 
#' @param x A vector.
#' @export
column_types <- function(x) {
  unlist(lapply(x, class))
}


#' Extract Numeric Columns
#' 
#' \code{numeric_columns} returns a vector of names corresponding to the numeric
#' columns of \code{x}.
#' 
#' @param x A vector.
#' @export
numeric_columns <- function(x) {
  names(which(unlist(lapply(x, is.numeric))))
}


#' Convert Non-numeric Columns
#' 
#' \code{factorize} converts all non-numeric columns in \code{x} to factors.
#' 
#' @param x A vector.
#' @export
#' @examples 
#' d <- data.frame(x = 1:3, 
#'                 y = letters[1:3], 
#'                 z = c(T, T, F), 
#'                 stringsAsFactors = FALSE)
#' column_types(d)
#' column_types(factorize(d))
#' str(d)
#' str(factorize(d))
factorize <- function(x) {
  factors <- names(which(sapply(x, function(y) !is.numeric(y))))
  for (fac in factors) {
    x[[fac]] <- as.factor(x[[fac]])
  }
  x
} 


#' Summarize Columns
#' 
#' \code{summarize_columns} produces a data frame summarizing the columns of 
#' \code{x}.
#' 
#' @param x A data frame or an R object that can be coerced into one.
#' @export
#' @examples 
#' d <- data.frame(w = c(NA, NA, exp(1:2), NA),
#'                 x = 1:5, 
#'                 y = c("a", "b", "a", "a", NA), 
#'                 z = c(T, T, F, NA, NA), 
#'                 stringsAsFactors = FALSE)
#' summarize_columns(d)
summarize_columns <- function(x) {
  if (class(x) != "data.frame") {
    x <- as.data.frame(x)
  }
  res <- data.frame("column" = names(x),
                    "type" = column_types(x),
                    "# unique" = sapply(x, num_unique),
                    "# missing" = sapply(x, num_nas),
                    "% missing" = sapply(x, num_nas) / nrow(x),
                    "most freq" = sapply(x, most_freq),
                    check.names = FALSE)
  rownames(res) <- NULL
  res
}
