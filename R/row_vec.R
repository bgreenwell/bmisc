#' Row Vectors
#' 
#' Constructs a row vector.
#' 
#' @param ... The elements of the row vector.
#' 
#' @return A row vector of dimension \code{1 x length(x)}.
#' 
#' @param use.names Logical indicating whether or not names should be preserved.
#' 
#' @export
#' 
#' @examples
#' row_vec(1:5)
#' row_vec(1, 2, 3, 4, 5)
#' row_vec(t(row_vec(1:5)))
#' row_vec(col_vec(1:5))
#' row_vec(pi, exp(1), sin(0))
#' row_vec(rnorm(3))
#' row_vec("a" = 1, "b" = 2)  # names are preserved by default
row_vec <- function(..., use.names = TRUE) {
  UseMethod("row_vec")
}


#' @rdname row_vec
#' 
#' @export
row_vec.default <- function(..., use.names = TRUE) {
  dots <- list(...)
  x <- unlist(dots, recursive = FALSE, use.names = use.names)
  t(x)
}

#' @rdname row_vec
#' 
#' @export
row_vec.matrix <- function(..., use.names = TRUE) {
  x <- list(...)[[1L]]
  if (!use.names) {
    dimnames(x) <- NULL
  }
  dims <- dim(x)
  if (!(1 %in% dims)) {
    stop("x should should be a row or column vector.")
  }
  if (dims[1L] == 1) {
    x
  } else {
    t(x)
  }
}


#' @rdname row_vec
#' 
#' @export
row_vec.data.frame <- function(..., use.names = TRUE) {
  x <- list(...)[[1L]]
  row_vec(data.matrix(x))
}


#' Column Vectors
#' 
#' Constructs a column vector.
#' 
#' @param ... The elements of the columns vector.
#' 
#' @param use.names Logical indicating whether or not names should be preserved.
#' 
#' @return A row vector of dimension \code{length(x) x 1}.
#' 
#' @export
#' 
#' @examples
#' col_vec(1:5)
#' col_vec(row_vec(1:5))
#' 
#' X <- matrix(rnorm(20), ncol = 2)
#' b <- col_vec(1.5, 3)
#' (yhat <- X %*% b)
col_vec <- function(..., use.names = TRUE) {
  UseMethod("col_vec")
}


#' @rdname col_vec
#' 
#' @export
col_vec.default <- function(..., use.names = TRUE) {
  dots <- list(...)
  x <- unlist(dots, recursive = FALSE, use.names = use.names)
  t(t(x))
}


#' @rdname col_vec
#' 
#' @export
col_vec.matrix <- function(..., use.names = TRUE) {
  x <- list(...)[[1L]]
  if (!use.names) {
    dimnames(x) <- NULL
  }
  dims <- dim(x)
  if (!(1 %in% dims)) {
    stop("x should should be a row or column vector.")
  }
  if (dims[2L] == 1) {
    x
  } else {
    t(x)
  }
}


#' @rdname col_vec
#' 
#' @export
col_vec.data.frame <- function(..., use.names = TRUE) {
  x <- list(...)[[1L]]
  col_vec(data.matrix(x))
}
