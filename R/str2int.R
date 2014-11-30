##' Convert Characters to Integers
##'
##' Convert one-dimensional character objects to integers.
##' 
##' @param string A character vector.
##' 
##' @return An integer (not necessarily unique).
##' 
##' @examples
##' ## Should all produce the same integer (currently results in 124)
##' str2int("Hello World")
##' str2int("hello world")
##' str2int("hello   world")
##' str2int("hello.world")
##' str2int("hello...world")
##' str2int("hello-world")
##' str2int("hello - world")
##' str2int("hello---world")
##' str2int("helloworld")
##' str2int("HelloWorld")
##' str2int("H._.e L. - lo -.  W__or l- .. d")
##' 
##' set.seed(str2int("Some random numbers"))
##' rnorm(3)  # should give: 1.7500983 -0.1093635 -0.9958618
##' set.seed(str2int("Some more random numbers")) 
##' rnorm(3)  # should give: 0.007765185 -1.138536203  0.091017129
##' 
##' ## Potential issues
##' str2int("abc")  # does not provide unique integers
##' str2int("cba")
str2int <- function(string) {
  string <- tolower(string)
  all_chars <- c(letters, " ", "\\.", "-", "_")
  all_numbers <- c(seq_len(length(all_chars) - 4), 0, 0, 0, 0)
  chars <- unlist(strsplit(x = string, split = ""))
  chars[chars == "."] <- "\\."
  chars[chars == "-"] <- "\\-" 
  chars[chars == "_"] <- "\\_" 
  id <- sapply(chars, grep, x = all_chars)
  nums <- all_numbers[id]
  seed <- sum(nums)
  min(c(seed, .Machine$integer.max))
}

##' Specify Seeds
##'
##' Specify seeds using character vector.
##' 
##' @param string A character vector.
##' 
##' @return Returns NULL, invisibly.
##' 
##' @examples
##' setSeed("Some random numbers")
##' rnorm(3)  # should give: 1.7500983 -0.1093635 -0.9958618
setSeed <- function(string) {
  set.seed(str2int(string))
}