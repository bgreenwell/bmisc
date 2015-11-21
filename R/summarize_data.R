#' Columns Types
#' 
#' Extract the data type of each column of a data frame.
#' 
#' @param x A data frame or an R object that can be coerced into one.
column_types <- function(x, ...) {
  unlist(lapply(x, class))
}

#' Summarize Data
#' 
#' Summarize a data frame.
#' 
#' @param x A data frame or an R object that can be coerced into one.
#' @param ... Additional optional arguments. 
summarize_data <- function(x, ...) {
  
  # If x is not a data frame, then try to coerce it into one
  if (class(x) != "data.frame") {
    x <- as.data.frame(x)
  }
  
  # Column names
  col_names <- names(x)
  
  # Column types
  col_types <- column_types(x)
  
  # Number of unique values
  num_unique <- unlist(lapply(x, function(y) length(unique(y))))
                       
  # Number of missing values
  num_missing <- unlist(lapply(x, function(y) sum(is.na(y))))
  
  # Proportion missing
  prop_missing <- num_missing / nrow(x)
  
  # Store results in a data frame
  res <- data.frame("column" = col_names,
                    "type" = col_types,
                    "number_unique" = num_unique,
                    "number_missing" = num_missing,
                    "fraction_issing" = prop_missing)
  rownames(res) <- NULL
  res
  
}