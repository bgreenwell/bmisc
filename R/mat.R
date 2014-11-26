##' NumPy style matrix function
##'
##' Returns a matrix from a character vector or list of vectors.
##' 
##' @examples
##' ## Using character vectors
##' mat('1, 2, 3; 4, 5, 6')  # similar to numpy.mat from Python extension NumPy
##' (m <- mat(paste('exp(', 1:9, ')')))
##' resize(m, 3, 3)
##' resize(m, 3, 3, byrow = FALSE)
##' matrix(exp(1:9), 3, 3)
##' 
##' ## Using a list
##' mat(list(1:3, 4:6, 7:9))
mat <- function(x, ...) {
  UseMethod("mat")
}

##' @rdname mat
##' @export
##' @method mat character
mat.character <- function(x) {
  
  ## Gather rows and individual values
  char_rows <- unlist(strsplit(x, split = ";"))
  values <- unname(unlist(sapply(char_rows, strsplit, split = ",")))
  
  ## Form matrix from parsed values by calling R's built-in matrix function
  matrix(sapply(values, function(x) eval(parse(text = x))), 
         nrow = length(char_rows), byrow = TRUE)
  
}

##' @rdname mat
##' @export
##' @method mat list
mat.list <- function(x, rows = TRUE) {
  
  ## Check element types
  if (!all(sapply(x, class) %in% c("numeric", "integer"))) {
    stop("Each element must be of type 'numeric' or 'integer'.", call. = FALSE)
  }
  
  ## Check length of each element
  if (!all(sapply(x, length) >= 1) && length(unique(sapply(x, length))) != 1) {
    stop("Each element must contain at least one value.", call. = FALSE)
  }
  
  ## Form matrix by combining elements
  if (rows) do.call(rbind, x) else do.call(cbind, x)
  
}

##' Resize a given matrix
##' 
##' Returns a new matrix of dimension nrows by ncols using the elements of x.
resize <- function(x, nrows, ncols, byrow = TRUE) {
  
  ## Check dimensions (if supplied)
  if (missing(nrows) && missing(ncols)) {
    nrows <- nrow(x)
    ncols <- ncol(x)
  }
  if (missing(nrows) && !missing(ncols)) {
    if (length(x) %% ncols != 0) {
      stop("dimension mismatch.", call. = FALSE)
    }
    nrows <- length(x) / ncols
  }
  if (!missing(nrows) && missing(ncols)) {
    if (length(x) %% nrows != 0) {
      stop("dimension mismatch.", call. = FALSE)
    }
    ncols <- length(x) / nrows
  }
  if (nrows * ncols != length(x)) {
    stop("dimension mismatch.", call. = FALSE)
  }
  if (!inherits(x, "matrix")) {
    stop("x must be of class 'matrix'.", call. = FALSE)
  }

  ## Return matrix with new dimensions
  if (byrow) {
    attr(x, "dim") <- c(ncols, nrows) 
    t(x)
  } else {
    attr(x, "dim") <- c(nrows, ncols) 
    x
  }
  
}