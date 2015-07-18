#' Plot of Random Forest OOB Error
#' 
#' ggplot2-based lineplot of oob-based error for a random forest object.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param include.legend Logical indicating whether to include a legend. Default
#'   is \code{TRUE}.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param ... Additional optional arguments.
#' @importFrom ggplot2 aes coord_flip element_blank element_text geom_bar
#' @importFrom ggplot2 geom_hline geom_line geom_point geom_tile ggplot ggtitle
#' @importFrom ggplot2 scale_fill_gradient scale_x_discrete scale_y_discrete
#' @importFrom ggplot2 stat_smooth theme theme_light xlab ylab
#' @export
ggErrorPlot <-function(object, include.legend = TRUE,                           
                       xlab = "Number of trees",
                       ylab = "OOB error",
                       main = "Training Error Plot\n", ...) {
  
  # Get data from random forest object for plotting
  errs <- fortify(object, values = "error")
  
  # Initialize ggplot object
  if (object$type == "classification") {
    p <- ggplot(errs, aes(x = ntree, y = error, colour = class))
  } else {
    p <- ggplot(errs, aes(x = ntree, y = mse))
  }

  # Add layers
  p <- p + geom_line()
  p <- p + theme_light(base_size = 20)
  p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main)  # annotate plot
  if (include.legend) {
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.title = element_blank())
  } else {
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.position = "none")
  }
  
  # Return ggplot object
  return(p)
  
}


#' Margin Plot
#' 
#' @param object A \code{randomForest} object.
#' @param include.legend Logical indicating whether a legend should be drawn
#'   on the plot.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param main Plot title.
#' @param ... Additional optional arguments.
#' @export
ggMarginPlot <- function(object, 
                         include.legend = TRUE,                           
                         xlab = "Index",
                         ylab = "Margin",
                         main = "Margin Plot\n", ...) {
  
  # Get data from random forest object for plotting
  md <- fortify(object, values = "margin")
  
  # Initialize ggplot object
  p <- ggplot(md, aes(x = as.numeric(reorder(index, margin)), 
                      y = margin, colour = class))
  
  # Add layers
  p <- p + geom_point()
  p <- p + theme_light(base_size = 20)
  p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main)  # annotate plot
  if (include.legend) {
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.title = element_blank())
  } else {
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.position = "none")
  }
  
  # Return ggplot object
  return(p)
  
}



#' Random Forest Parallel Coordinates Plot
#' 
#' Parallel coordinates plot of random forest feature importance scores for each
#' class.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param include.legend Logical indicating whether to include a legend. Default
#'   is \code{TRUE}.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param ... Additional optional arguments.
#' @importFrom randomForest randomForest
#' @export
ggParallelCoordinatePlot <- function(object, 
                                     include.legend = FALSE,
                                     xlab = "Variable",
                                     ylab = "Importance score",
                                     main = "Parallel Coordinate Plot\n", ...) {
  
  # Data frame containing feature importance scores by class
  pcd <- fortify(object, values = "parallel")
  
  # Initialize ggplot object
  p <- ggplot(pcd, aes(x = feature, y = score))
  
  # Add layers
  p <- p + geom_line(aes(group = as.factor(class),
                         colour = as.factor(class)))
  p <- p + theme_light(base_size = 20)
  p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main)
  if (!include.legend) {
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.position = "none")
  } else {
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.title = element_blank())
  }
  
  # Return ggplot object
  return(p)
  
}



#' Random Forest Partial Dependence Plot
#' 
#' ggplot2 version of \code{randomForest::partialPlot}.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param smooth Logical indicating whether to add a scatterplot smoother to the
#'   plot. Default is \code{TRUE}.
#' @param ... Additional arguments required by \code{partialPlot}. AT a minimum,
#'   \code{x.var} and \code{pred.data} should be given, althouh here, 
#'   \code{x.data} should be a character vector giving the name of the predictor
#'   variable of interest.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @importFrom randomForest randomForest
#' @export
ggPartialPlot <- function(object, smooth = TRUE, ...,
                          main = "Partial Dependence Plot\n",
                          xlab = list(...)$x.var,
                          ylab = "Partial dependence") {
  
  # Data frame containing partial dependence data
  pdd <- fortify(object, values = "partial", ...)
  
  # Initialize ggplot object
  p <- ggplot(pdd, aes(x = x, y = y))
  
  # Add layers
  if (smooth) p <- p + stat_smooth(size = 2, alpha = 0.5)
  p <- p + geom_line() 
  p <- p + theme_light(base_size = 20)
  p <- p + ylab(ylab)
  p <- p + xlab(xlab)
  p <- p + ggtitle(main)
  p <- p + theme(plot.title = element_text(size = 18))
  
  # Return ggplot object
  return(p)
  
}



#' Random Forest Proximity Matrix Plot
#' 
#' ggplot2 version of \code{randomForest::MDSplot} and more.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param plot.type Type of plot to produce. Current options are 
#'   \code{"heat"} for a heat map of the proximity matrix or \code{"MDS"} for a
#'   multi-dimensional scaling plot of the proximity matrix.
#' @param fac Abc.
#' @param size Abc.
#' @param alpha Abc.
#' @param include.legend Logical indicating whether to include a legend.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param ... Additional optional arguments. This option is currently ignored.
#' @importFrom randomForest randomForest
#' @export
ggProximityPlot <- function(object, plot.type = c("heat", "MDS", "outlier"), 
                            fac, size = 3, alpha = 1, include.legend = TRUE, ...,
                            xlab = if (plot.type == "MDS") "Dimension 1" else "",
                            ylab = if (plot.type == "MDS") "Dimension 2" else "",
                            main = if (plot.type == "MDS") "MDS Plot of Proximity Matrix\n" else "Heat Map of Proximity Matrix\n") {
  
  plot.type <- match.arg(plot.type)
  
  # Heat map of proximity matrix
  if (plot.type == "heat") {
    
    # Extract data for plot
    .data <- fortify(object, values = "proximity-heat")
    
    # Initialize ggplot object
    p <- ggplot(.data, aes(x = obs1, y = obs2))
    
    # Add layers
    p <- p + geom_tile(aes(fill = proximity), colour = "white")
    p <- p + scale_fill_gradient(low = "white", high = "steelblue")
    p <- p + theme_light(base_size = base_size)
    p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main)
    p <- p + scale_x_discrete(expand = c(0, 0))
    p <- p + scale_y_discrete(expand = c(0, 0))
    p <- p + theme(plot.title = element_text(size = 18),
                   legend.position = "none",
                   axis.ticks = element_blank(), 
                   axis.text.x = element_text(size = 7, 
                                              hjust = 0, 
                                              colour = "grey50"))
    
  }
  
  # Multi-dimensional scaling plot of proximity matrix
  if (plot.type == "MDS") {
    
    # Extract data for plot
    .data <- fortify(object, values = "proximity-mds")
    
    # Initialize ggplot object
    p <- ggplot(.data, aes(x = dim1, y = dim2, colour = class))
    
    # Add layers
    p <- p + geom_point(aes(shape = class), size = size, alpha = alpha)
    p <- p + theme_light(base_size = 20)
    p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main)
    if (!include.legend) {
      p <- p + theme(plot.title = element_text(size = 18),
                     legend.position = "none")
    } else {
      p <- p + theme(plot.title = element_text(size = 18),
                     legend.title = element_blank())
    }
    
  }
  
  # Proximity-based outlier plot
  if (plot.type == "outlier") {
    
    # Extract proximity matrix from ranfom forest (if available)
    out <- fortify(object, values = "proximity-outlier", ...)
    
    # Initialize ggplot object
    p <- ggplot(out, aes(x = obs, y = outlyingness, colour = fac))
    
    # Add layers
    p <- p + geom_bar(aes(fill = fac), stat = "identity", width = 0.25)
    if (refline) p <- p + geom_hline(yintercept = 10)
    p <- p + theme_light(base_size = 20)
    p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main)
    if (include.legend) {
      p <- p + theme(plot.title = element_text(size = 18),
                     legend.title = element_blank())
    } else {
      p <- p + theme(plot.title = element_text(size = 18),
                     legend.position = "none")
    }
    
  }
  
  
  # Return ggplot object
  return(p)
  
}



#' Random Forest Variable Importance Plot
#' 
#' ggplot2 version of \code{randomForest::varImpPlot}.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param type Either 1 or 2, specifying the type of importance measure 
#'   (1=mean decrease in accuracy, 2=mean decrease in node impurity).
#' @param class For classification problem, which class-specific measure to 
#'   return.
#' @param scale For permutation based measures, should the measures be divided 
#'   their "standard errors"?
#' ... Additional optional arguments. This option is currently ignored.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param ... Additional optional arguments.
#' @importFrom randomForest randomForest
#' @export
ggVarImpPlot <- function(object, type = NULL, class = NULL, scale = TRUE, ...,
                         main = "Feature Importance Plot\n",
                         xlab = "",
                         ylab = "Importance score") {
  
  # Data frame containing feature importance scores
  feature_importance <- fortify(object, values = "importance", type = type,
                                class = class, scale = scale)
  
  # ggplot2 version of randomForest::varImpPlot
  p <- ggplot(feature_importance, 
              aes(x = reorder(feature, importance), y = importance))
  p <- p + geom_bar(stat = "identity", fill = "#53cfff")
  p <- p + coord_flip()
  p <- p + theme_light(base_size = 20)
  p <- p + ylab(ylab)
  p <- p + xlab(xlab)
  p <- p + ggtitle(main)
  # Finally, add theme to plot
  p <- p + theme(plot.title = element_text(size = 18))
  
  # Return ggplot object
  return(p)
  
}