#' Inspect a Data Frame
#'
#' Inspect a data frame.
#'
#' @param x A data frame.
#' @param n Integer specifying the number of rows to inspect at random.
#' @importFrom dplyr tbl_df
#' @export
inspect <- function(x, n = 8) {
  tbl_df(x[sample(nrow(x), size = n), ])
}
