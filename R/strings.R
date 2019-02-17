#' String methods
#' 
#' Extract the first alphabetic character (if available) from the characters of 
#' a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @return Returns the first alphabetic character (A-Z or a-z) or \code{NA} if 
#' no alphabetic characters are found.
#' 
#' @export
#' 
#' @example
#' first_alpha("12@3Zba589@")
first_alpha <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    grep("[A-Za-z]", tokens, value = TRUE)[1]
  })
}


#' String methods
#' 
#' Extract the first digit (if available) from the characters of a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @return Returns the first digit (0-9) or \code{NA} if no digits are found.
#' 
#' @export
#' 
#' @examples 
#' first_digit("aBc-def84hj")
first_digit <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    grep("[0-9]", tokens, value = TRUE)[1]
  })
}


#' String methods
#' 
#' General test if characters in a string are alphanumeric.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @return Returns \code{TRUE} if all characters in the string are alphanumeric 
#' and there is at least one character, FALSE otherwise.
#' 
#' @export
#' 
#' @examples 
#' is_alnum("abc")
#' is_alnum("abc1")
#' is_alnum("1")
is_alnum <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    all(grepl("[A-Za-z0-9]", tokens))
  })
}


#' String methods
#' 
#' General test if characters in a string are digits.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @return Returns \code{TRUE} if all characters in the string are digits and 
#' there is at least one character, FALSE otherwise.
#' 
#' @export
#' 
#' @examples 
#' is_digit("abc")
#' is_digit("abc1")
#' is_digit("1")
#' is_digit("1.3")
is_digit <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    all(grepl("[0-9]", tokens))
  })
}


#' String methods
#' 
#' Extract the last alphabetic character (if available) from the characters of 
#' a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @return Returns the last alphabetic character (A-Z or a-z) or \code{NA} if 
#' no alphabetic characters are found.
#' 
#' @export
#' 
#' @examples 
#' last_alpha("123abc")
#' last_alpha("123abc456")
#' last_alpha("123")
#' last_alpha("abc")
last_alpha <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[A-Za-z]", tokens, value = TRUE)
    out[length(out)]
  })
}


#' String methods
#' 
#' Extract the last digit (if available) from the characters of a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @return Returns the last digit (0-9) or \code{NA} if no digits are found.
#' 
#' @export
#' 
#' @examples 
#' last_digit("123abc")
#' last_digit("123abc456")
#' last_digit("123")
#' last_digit("abc")
last_digit <- function(string) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[0-9]", tokens, value = TRUE)
    out[length(out)]
  })
}


#' String methods
#' 
#' Extract the k-th alphabetic character (if available) from the characters of 
#' a string.
#' 
#' @param string An object that inherits from class \code{"character"}.
#' 
#' @param k Integer specifying which alphabetic character to extract.
#' 
#' @return Returns the k-th alphabetic character (A-Z or a-z) or \code{NA} if 
#' at least \code{k} alphabetic characters are not found.
#' 
#' @export
#' 
#' @examples
#' kth_digit("abc0def7ghi5")
#' kth_digit("abc0def7ghi5", k = 2)
kth_alpha <- function(string, k = 1L) {
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
#' 
#' @param k Integer specifying which digit to extract.
#' 
#' @return Returns the k-th digit (0-9) or \code{NA} if at least \code{k} digits 
#' are not found.
#' 
#' @export
#' 
#' @examples 
#' kth_digit("abc0def7ghi5")
#' kth_digit("abc0def7ghi5", k = 2)
kth_digit <- function(string, k = 1L) {
  stopifnot(is.character(string))
  sapply(string, FUN = function(x) {
    tokens <- strsplit(x, "")[[1L]]
    out <- grep("[0-9]", tokens, value = TRUE)
    out[k]
  })
}


#' Convert characters to integers
#'
#' Convert one-dimensional character objects to integers.
#' 
#' @param string A character vector.
#' 
#' @return An integer (not necessarily unique).
#' 
#' @export
#' 
#' @examples
#' # Should all produce the same integer (currently results in 124)
#' str2int("Simulation 1")
#' str2int("simulation 1")
#' str2int("SIMULATION 1")
#' str2int("simulation.1")
#' str2int("simulation..1")
#' str2int("simulation-1")
#' str2int("simulation - 1")
#' str2int("simulation---1")
#' str2int("simulation1")
#' 
#' # Potential issues
#' str2int("abc")  # does not provide unique integers
#' str2int("cba")
str2int <- function(string) {
  string <- tolower(string)
  all_chars <- c(letters, " ", "\\.", "-", "_", paste(0L:9L))
  all_numbers <- c(seq_len(length(all_chars) - 14), rep_len(0, length.out = 14))
  chars <- unlist(strsplit(x = string, split = ""))
  chars[chars == "."] <- "\\."
  chars[chars == "-"] <- "\\-" 
  chars[chars == "_"] <- "\\_" 
  id <- sapply(chars, grep, x = all_chars)
  nums <- all_numbers[id]
  seed <- sum(nums)
  min(c(seed, .Machine$integer.max))
}


#' Specify seeds
#'
#' Specify seeds using character starins.
#' 
#' @param string A character string.
#' 
#' @return Returns NULL, invisibly.
#' 
#' @export
#' 
#' @examples
#' set_seed("Some reproducible random numbers")
#' rnorm(3)  # should give: 1.4243634 -0.8759381 -1.6210951
set_seed <- function(string) {
  set.seed(str2int(string))
}