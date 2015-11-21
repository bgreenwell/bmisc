#' Additional Operators
#' 
#' Returns a logical vector indicating if there is not a match for its left 
#' operand.
#' 
#' @param x The values to be matched.
#' @param y The values to be matched against.
#' @rdname operators
#' @export
`%notin%` <- function(x, y) {
  !(match(x, y, nomatch = 0) > 0)
}


#' @rdname operators
`%+=%` <- function(x, y){
  UseMethod("%+=%")
}


#' @rdname operators
`%+=%.default` <- function(x, y){
  xchar <- deparse(substitute(x))
  ychar <- deparse(substitute(y))
  command <- paste(xchar, "<-", xchar, "+", ychar)
  invisible(eval(parse(text = command), envir = parent.frame(1)))
}


#' @rdname operators
`%-=%` <- function(x, y){
  UseMethod("%-=%")
}


#' @rdname operators
`%-=%.default` <- function(x, y){
  xchar <- deparse(substitute(x))
  ychar <- deparse(substitute(y))
  command <- paste(xchar, "<-", xchar, "-", ychar)
  invisible(eval(parse(text = command), envir = parent.frame(1)))
}


#' @rdname operators
`%*=%` <- function(x, y){
  UseMethod("%*=%")
}


#' @rdname operators
`%*=%.default` <- function(x, y){
  xchar <- deparse(substitute(x))
  ychar <- deparse(substitute(y))
  command <- paste(xchar, "<-", xchar, "*", ychar)
  invisible(eval(parse(text = command), envir = parent.frame(1)))
}


#' @rdname operators
`%/=%` <- function(x, y){
  UseMethod("%/=%")
}


#' @rdname operators
`%/=%.default` <- function(x, y){
  xchar <- deparse(substitute(x))
  ychar <- deparse(substitute(y))
  command <- paste(xchar, "<-", xchar, "/", ychar)
  invisible(eval(parse(text = command), envir = parent.frame(1)))
}


#' @rdname operators
`%+%` <- function(x, y) {
  UseMethod("%+%")
}


#' @rdname operators
`%+%.default` <- function(x, y) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  if (!is.character(y)) {
    y <- as.character(y)
  }
  paste(x, y, sep = "")
}


#' @rdname operators
`%+%.character` <- function(x, y) {
  paste(x, y, sep = "")
}


#' @rdname operators
`%-%` <-function(x, y) {
  UseMethod("%-%")
}


#' @rdname operators
`%-%.character` <-function(x, y) {
  xtokens <- strsplit(x, "")[[1]]
  ytokens <- strsplit(y, "")[[1]]
  if (any(ytokens %in% xtokens)) {
    xtokens[!xtokens %in% ytokens]
  } else {
    x
  }
}