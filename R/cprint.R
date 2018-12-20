#' Copy print
#'
#' The \code{cprint} function provides a capability to print common R data 
#' structures in a form which can be used as input to the interpreter. 
#'
#' @param x An appropriate R object.
#' 
#' @param ... Additional optional arguments. (Currently ignored.)
#' 
#' @rdname cprint
#' 
#' @export
#' 
#' @examples 
#' cprint(iris)  # output can easily be copied into an RStudio script file
cprint <- function(x, ...) {
  UseMethod("cprint")
}


#' @rdname cprint
#' 
#' @export
cprint.numeric <- function(x, ...) {
  string <- paste0("c(", paste0(x, collapse = ", "), ")")
  cat(string, "\n")
}


#' @rdname cprint
#' 
#' @export
cprint.character <- function(x, ...) {
  string <- paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
  cat(string, "\n")
}


#' @rdname cprint
#' 
#' @export
cprint.data.frame <- function(x, ...) {
  cnames <- names(x)
  cat("data.frame(", "\n", ...)
  for (i in seq_len(length(cnames) - 1)) {
    if (is.numeric(x[[i]])) {
      cat(" ", cnames[i], "=", 
          paste0("c(", paste0(x[[i]], 
                              collapse = ", "), "),"), "\n", ...)
    } else {
      cat(" ", cnames[i], "=", 
          paste0("c(", paste0("\"", x[[i]], "\"", 
                              collapse = ", "), "),"), "\n", ...)
    }
  }
  if (is.numeric(x[[length(cnames)]])) {
    cat(" ", cnames[length(cnames)], "=", 
        paste0("c(", paste0(x[[length(cnames)]], 
                            collapse = ", "), ")"), "\n", ...)    
  } else {
    cat(" ", cnames[length(cnames)], "=", 
        paste0("c(", paste0("\"", x[[length(cnames)]], "\"", 
                            collapse = ", "), ")"), "\n", ...)    
  }
  cat(")", "\n", ...)
}
