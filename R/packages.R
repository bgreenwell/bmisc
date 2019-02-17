#' Import specific objects from packages
#'
#' Import specific objects (e.g., functions) from a packages namespace.
#'
#' @param pkg Character string giving the name of the specific R package from
#' which the objects will be imported.
#' 
#' @param import Character vector specifying the particular objects to import
#' from \code{pkg}.
#' 
#' @param as Optional character vector specifying the names for which
#' the imported objects will be assigned to. If missing, then the original
#' objects (i.e., \code{objects}) names will be used.
#' 
#' @param envir The environment for which the objects will be loaded into. The
#' default is \code{.GlobalEnv} meaning load the objects into the global
#' environment.
#'   
#' @details
#' Normally, one could implement this via \code{object <- pkg::object}, but that
#' would require a new assignment for each object imported. This method allows
#' one to import many objects at once from a particular package. It is also less
#' error-prone then using \code{pkg::object} each time \code{object} is used.
#'
#' @export
#' 
#' @examples
#' # Importing package plyr's entire namespace can cause issues when package
#' # dplyr is loaded. Using importr can help mitigate these types of issues. The 
#' # following example assumes that both dplyr and plyr are installed.
#' library(dplyr)
#' from("plyr", import = c("ddply", "ldply"))
#' from("plyr", import = c("ddply", "ldply"), as = c(".ddply", ".ldply"))
#' identical(ddply, .ddply)
from <- function(pkg, import, as, envir = .GlobalEnv) {
  if (!(pkg %in% utils::installed.packages()[, "Package"])) {
    stop(paste("package", pkg, "not found"))
  }
  object.names <- if (missing(as)) import else as
  for (i in seq_len(length(import))) {
    assign(object.names[i],
           eval(parse(text = paste(pkg, import[i], sep = "::"))),
           envir = envir)
  }
}


#' Load/unload multiple packages
#'
#' Load or unload multiple packages at once, rather than having to call
#' \code{library} or \code{detach} separately for each.
#'
#' @rdname load-remove
#' 
#' @param pkgs Character vector of package names.
#' 
#' @export
load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
}


#' @rdname load-remove
#' 
#' @export
unload_packages <- function(pkgs) {
  pkgs <- paste("package", pkgs, sep = ":")
  for (pkg in pkgs) {
    detach(pkg, character.only = TRUE, unload = TRUE)
  }
}
