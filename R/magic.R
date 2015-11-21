#' Odd Magic Square
#' 
#' Construct a magic square of odd order using de La Loubere’s method.
#' 
#' @param n An odd integer
#' @examples
#' oms <- odd_magic_square(9)
#' rowSums(oms)
#' colSums(oms)
#' sum(diag(oms))
#' @export
odd_magic_square <- function(n) {
  x <- matrix(0L, nrow = n, ncol = n)
  i <- 1
  j <- (n + 1) / 2
  for (k in 1:(n^2)) {
    is <- i
    js <- j
    x[i, j] <- k
    i <- n - ((n + 1 - i) %% n)
    j <- (j %% n) + 1
    if (x[i, j] != 0) {
      i = (is %% n) + 1
      j = js
    }
  }
  x
}


# Check combinatorics book for algorithms!



