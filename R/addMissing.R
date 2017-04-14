#' Add Missing Values
#' 
#' Add missing values to a data frame or matrix.
#'
#' @param x A data frame or matrix.
#'
#' @param p Proportion of values to replace at random with \code{NA}.
#'
#' @export
addMissing <- function(x, p = 0.1) {
  dim.x <- dim(x)
  ij.grid <- expand.grid(i = seq_len(dim.x[1L]), j = seq_len(dim.x[2L]))
  ij.missing <- ij.grid[sample(nrow(ij.grid), replace = FALSE, 
                               size = ceiling(prod(p, dim.x))), ]
  for (k in seq_len(nrow(ij.missing))) {
    x[ij.missing[k, "i"], ij.missing[k, "j"]] <- NA
  }
  x
}
