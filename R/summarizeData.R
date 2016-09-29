#' Columns Types
#' 
#' Extract the data type of each column of a data frame.
#' 
#' @param x A data frame or an R object that can be coerced into one.
#' @export
columnTypes <- function(x, ...) {
  sapply(x, class)
}

#' Summarize Data
#' 
#' Summarize a data frame.
#' 
#' @param x A data frame or an R object that can be coerced into one.
#' @param ... Additional optional arguments. 
summarizeData <- function(x, ...) {
  
  # If x is not a data frame, then try to coerce it into one
  if (class(x) != "data.frame") {
    x <- as.data.frame(x)
  }
  
  # Column names
  col.names <- names(x)
  
  # Column types
  col.types <- columnTypes(x)
  
  # Number of unique values
  num.unique <- unlist(lapply(x, function(y) length(unique(y))))
                       
  # Number of missing values
  num.missing <- unlist(lapply(x, function(y) sum(is.na(y))))
  
  # Proportion missing
  prop.missing <- num.missing / nrow(x)
  
  # Store results in a data frame
  res <- data.frame("column" = col.names,
                    "type" = col.types,
                    "number.unique" = num.unique,
                    "number.missing" = num.missing,
                    "fraction.missing" = prop.missing)
  rownames(res) <- NULL
  res
  
}