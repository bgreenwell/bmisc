#' Insert missing values
#' 
#' Insert missing values into a data frame or matrix.
#'
#' @param x A data frame or matrix.
#'
#' @param p Proportion of values to replace at random with \code{NA}.
#' 
#' @param cols Either a character vector of columns names or an integer vector 
#' of column positions specifying which columns to insert mssing values into.
#' Default is \code{NULL} which will use all avaialble columns.
#'
#' @export
#' 
#' @examples 
#' insert_missing(iris, 0.5)
#' insert_missing(iris, 0.5, cols = 1:4)
insert_missing <- function(x, p = 0.1, cols = NULL) {
  dim_x <- dim(x)
  if (!is.null(cols)) {
    if (is.character(x)) {
      # Convert to colum positions
      cols <- match(cols, colnames(x))
    }
    col_seq <- cols
  } else {
    col_seq <- seq_len(dim_x[2L])
  }
  ij_grid <- expand.grid(i = seq_len(dim_x[1L]), j = col_seq)
  ij_missing <- ij_grid[sample(nrow(ij_grid), replace = FALSE, 
                               size = ceiling(prod(p, dim_x))), ]
  for (k in seq_len(nrow(ij_missing))) {
    x[ij_missing[k, "i"], ij_missing[k, "j"]] <- NA
  }
  x
}
