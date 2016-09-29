#' Color Wheel
#' 
#' Plot a color wheel for a given color.
#' 
#' @param str A Character string representing the color of interest.
#' @param ... Additional optional arguments to be passed on to \code{pie}.
#' @export
#' @examples 
#' color_wheel("purple")
#' color_wheel("springgreen")
#' color_wheel("blue", cex = 0.75)
color_wheel <- function(str, ...) {
  cols <- grDevices::colors()[grep(str, colors())]
  graphics::pie(rep(1, length(cols)), labels = cols, col = cols, ...)
  cols
}