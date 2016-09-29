#' String Methods
#' 
#' Extract the first alphabetic character (if available) from the characters of 
#' a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns the first alphabetic character (A-Z or a-z) or \code{NA} if 
#'   no alphabetic characters are found.
#' @export
first.alpha <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    grep("[A-Za-z]", tokens, value = TRUE)[1]
  })
}


#' String Methods
#' 
#' Extract the first digit (if available) from the characters of a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns the first digit (0-9) or \code{NA} if no digits are found.
#' @export
first.digit <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    grep("[0-9]", tokens, value = TRUE)[1]
  })
}


#' String Methods
#' 
#' General test if characters in a string are alphanumeric.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns \code{TRUE} if all characters in the string are alphanumeric 
#'   and there is at least one character, FALSE otherwise.
#' @export
is.alnum <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    all(grepl("[A-Za-z0-9]", tokens))
  })
}


#' String Methods
#' 
#' General test if characters in a string are digits.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns \code{TRUE} if all characters in the string are digits and 
#'   there is at least one character, FALSE otherwise.
#' @export
is.digit <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    all(grepl("[0-9]", tokens))
  })
}


#' String Methods
#' 
#' Extract the last alphabetic character (if available) from the characters of 
#' a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns the last alphabetic character (A-Z or a-z) or \code{NA} if 
#'   no alphabetic characters are found.
#' @export
last.alpha <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[A-Za-z]", tokens, value = TRUE)
    out[length(out)]
  })
}


#' String Methods
#' 
#' Extract the last digit (if available) from the characters of a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns the last digit (0-9) or \code{NA} if no digits are found.
#' @export
last.digit <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[0-9]", tokens, value = TRUE)
    out[length(out)]
  })
}


#' String Methods
#' 
#' Extract the k-th alphabetic character (if available) from the characters of 
#' a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @param k Integer specifying which alphabetic character to extract.
#' @return Returns the k-th alphabetic character (A-Z or a-z) or \code{NA} if 
#'   at least \code{k} alphabetic characters are not found.
#' @export
kth.alpha <- function(string, k = 1L) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[A-Za-z]", tokens, value = TRUE)
    out[k]
  })
}


#' String Methods
#' 
#' Extract the k-th digit (if available) from the characters of a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' @param k Integer specifying which digit to extract.
#' @return Returns the k-th digit (0-9) or \code{NA} if at least \code{k} digits 
#'   are not found.
#' @export
kth.digit <- function(string, k = 1L) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[0-9]", tokens, value = TRUE)
    out[k]
  })
}
