#' Color Wheel
#' 
#' Plot a color wheel for a given color.
#' 
#' @param str A Character string representing the color of interest.
#' @param ... Additional optional arguments to be passed on to \code{pie}.
#' @importFrom graphics pie
#' @importFrom grDevices colors
#' @export
#' @examples 
#' color_wheel("purple")
#' color_wheel("springgreen")
#' color_wheel("blue", cex = 0.75)
color_wheel <- function(str, ...) {
  cols <- colors()[grep(str, colors())]
  pie(rep(1, length(cols)), labels = cols, col = cols, ...)
  cols
}


#' Multiple Plots
#' 
#' Multiple \code{"ggplot"} plots on a single page.
#' 
#' @param ... Plots of class \code{"ggplot"}.
#' @param cols Desired number of coluns in final layout of plot.
#' @param layout Desired layout of plots.
#' @importFrom grid grid.layout grid.newpage pushViewport viewport
#' @export
multiplot <- function(..., cols = 1, layout = NULL) {
  
  # Make a list from the ... arguments
  plots <- list(...)
  
  num_plots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(num_plots / cols)),
                     ncol = cols, nrow = ceiling(num_plots / cols))
  }
  
  if (num_plots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in seq_len(num_plots)) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


