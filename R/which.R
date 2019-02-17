#' Where are certain types of columns?
#' 
#' Determines the names or locations (i.e., index) of particular types of 
#' columns.
#' 
#' @param x A data frame (ideally), but list-like objects should work too.
#' 
#' @param names Logical indicating whether to return names (\code{TRUE}) or 
#' indexes \code{FALSE}. Default is \code{TRUE}.
#' 
#' @rdname which-type
#' 
#' @export
#' 
#' @examples 
#' which_numeric(iris)
#' which_factor(iris)
#' d <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
#' which_integer(d)
#' which_numeric(d)
#' which_character(d)
which_numeric <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "numeric")  # FIXME: Use `is.numeric()`?
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}


#' @rdname which-type
#' 
#' @export
which_factor <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "factor")  # FIXME: Use `is.factor()`?
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}


#' @rdname which-type
#' 
#' @export
which_character <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "character")  # FIXME: Use `is.character()`?
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}


#' @rdname which-type
#' 
#' @export
which_integer <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "integer")  # FIXME: Use `is.integer()`?
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}
