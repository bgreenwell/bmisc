which.numeric <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "numeric")
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}


which.factor <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "factor")
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}


which.character <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "character")
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}


which.integer <- function(x, names = TRUE) {
  pos <- which(sapply(x, class) == "integer")
  if (names) {
    names(x)[pos]
  } else {
    pos
  }
}
