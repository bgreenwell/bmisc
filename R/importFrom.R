#' Import Specific Objects from Packages
#' 
#' Import specific objects (e.g., functions) from a packages namespace.
#' 
#' @param pkg An R package.
#' @param objects Character vector specifying the particular objects to import
#'   from \code{pkg}.
#' @param envir The environment for which the objects will be loaded into. The
#'   default is \code{.GlobalEnv} meaning load the objects into the global
#'   environment.
#' @param names Optional character vector speccifying the name assigned to the
#'   imported objects. Currently not implemented.
#'   
#' @details 
#' Normally, one could implement this via \code{object <- pkg::object}, but that
#' would require a new assignment for each object imported. This method allows
#' one to import many objects at once from a particular package. 
importFrom <- function(pkg, objects, envir = .GlobalEnv) {
  imports <- sapply(objects, function(x) {
    assign(x, 
           eval(parse(text = paste(pkg, x, sep = "::"))), 
           envir = envir)
  })
  invisible(imports)
}
