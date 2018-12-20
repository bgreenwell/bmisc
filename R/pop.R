#' Remove Items
#' 
#' \code{pop} is a generic function that will remove the item at the given 
#' position.
#' 
#' @param x An appropriate R object.
#' 
#' @param i Integer position of the element to be removed in \code{x}. Defaults
#' to removing the last element of \code{x}.
#' 
#' @return Returns the object \code{x} with the i-th element removed.
#' 
#' @rdname pop
#' 
#' @export
#' 
#' @examples 
#' x <- 1:3
#' y <- letters[1:3]
#' z <- as.list(x)
#' names(z) <- y
#' pop(x)
#' pop(y, 2)  # remove the second element
#' pop(y, -2)  # remove everythng except the second element
#' for (i in 1:3) print(z <- pop(z))
pop <- function(x, i = length(x)) {
  UseMethod("pop")
}


#' @rdname pop
#' 
#' @export
pop.default <- function(x, i = length(x)) {
  x[-i]
}


#' @rdname pop
#' 
#' @export
pop.list <- function(x, i = length(x)) {
  x[[i]] <- NULL
  x
}