#' @keywords internal
createRaterMatrix <- function(x) {
  
  # Split each element into individual labels
  tokens <- lapply(strsplit(x, split = ","), trimws)
  
  # Vector of unique labels
  unique.tokens <- sort(unique(unlist(tokens)))
  
  # Initialize rater matrix
  rater.matrix <- matrix(NA, nrow = length(x), ncol = length(unique.tokens), 
                         dimnames = list(NULL, unique.tokens))
  
  # Assign each element of the rater matrix a 1 or 0
  for (i in seq_len(nrow(rater.matrix))) {
    for (j in seq_len(ncol(rater.matrix))) {
      if (unique.tokens[j] %in% tokens[[i]]) {
        rater.matrix[i, j] <- 1
      } else {
        rater.matrix[i, j] <- 0
      }
    }
  }
  
  # Return value
  rater.matrix
  
}


#' Phi Coefficient
#' 
#' Computes the phi coefficient measure of association for 2-by-2 contigency 
#' tables.
#' 
#' @param x A 2-by-2 contigency table.
#' @return The phi coefficient for the table.
#' @export
phi.coefficient <- function(x) {
  (x[1, 1] * x[2, 2] - x[1, 2] * x[2, 1]) / sqrt(prod(rowSums(x), colSums(x)))
}


#' Phi Coefficient for Multi Label Data
#' 
#' Computes an averaged phi coefficient measure of association for multi label
#' data.
#' 
#' @param x Rater 1.
#' @param y Rater 2.
#' @return A list with the following components: \code{avg.phi} and \code{phi}.
#' @export
phi.multi <- function(x, y) {
  xrm <- createRaterMatrix(x)
  yrm <- createRaterMatrix(y)
  stopifnot(identical(colnames(xrm), colnames(yrm)))
  phi <- numeric(ncol(xrm))
  tables <- vector("list", length = ncol(xrm))
  for (i in seq_len(length(phi))) {
    tables[[i]] <- table(xrm[, i], yrm[, i])
    phi[i] <- phi.coefficient(tables[[i]])
  }
  names(phi) <- names(tables) <- colnames(xrm)
  res <- list("avg.phi" = mean(phi),
              "phi" = phi,
              "xrm" = xrm,
              "yrm" = yrm,
              "tables" = tables)
  class(res) <- "phi.multi"
  res
}

#' @keywords internal
print.phi.multi <- function(x, ...) {
  for (i in seq_len(length(x$tables))) {
    cat("\n", 
        "---------------", "\n",
        "Agreement for", names(x$tables)[i], "\n",
        "---------------", "\n",
        "Phi coefficient:", phi.coefficient(x$tables[[i]]))
    cat("\n", "Agreement matrix:", "\n")
    print(x$tables[[i]])
    # cat(" ---------------", "\n")
  }
  cat("\n", "Average phi coefficient:", x$avg.phi, "\n")
}
