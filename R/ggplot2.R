#' Random Forest Summary Data
#' 
#' Extract information from a \code{"randomForest"} object into a data frame.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param values The type of data to collect.
#' @param fac A factor that was used as the response to train the random forest.
#'   Only used if \code{values = "proximity-mds"} or 
#'   \code{values = "proximity-outlier"}.
#' @param ... Additional arguments.
#' 
#' @return A data frame.
#' 
#' @importFrom ggplot2 fortify
#' @importFrom randomForest importance margin partialPlot
#' @importFrom reshape2 melt
#' 
#' @export
fortify.randomForest <- function (object, 
                                  values = c("importance",
                                             "oob-error",
                                             "oob-rsquared",
                                             "margin",
                                             "parallel",
                                             "partial", 
                                             "proximity-heat",
                                             "proximity-mds",
                                             "proximity-outlier"), 
                                  fac,
                                  ...) {
  
  # Object should have type "regression" or "classification"
  rf_type <- object$type
  if (rf_type == "unsupervised") {
    stop(paste("ggRandomForest does not currently support unsupervised random", 
               "forests."), call. = FALSE)
  }
  
  values <- match.arg(values)
  
  # Data frame containing variable importance scores
  if (values == "importance") {
    imp <- importance(object, ...)  # FIXME: Guaranteed to return one or the other?
    imp <- data.frame(feature = row.names(imp), importance = imp[, 1])
    return(imp)
  }
  
  # Data frame containing OOB error estimates
  if (values == "oob-error") {
    if (rf_type == "classification") {
      oob_errors <- as.data.frame(object$err.rate)
    } else {
      oob_errors <- as.data.frame(object$mse)
    }
    oob_errors$ntrees <- seq_len(nrow(oob_errors))
    oob_errors <- melt(oob_errors, id = "ntrees")
    names(oob_errors) <- c("ntrees", "class", "error")
    oob_errors$class <- as.factor(oob_errors$class)
    return(oob_errors)
  }
  
  # Data frame containing OOB R-squared estimates
  if (values == "oob-rsquared") {
    if (rf_type == "regression") {
      oob_rsq <- as.data.frame(object$rsq)
    } else {
      stop("OOB estimtes of R-squared only available for regression.")
    }
    oob_rsq$ntrees <- seq_len(nrow(oob_rsq))
    names(oob_rsq) <- c("rsquared", "ntrees")
    return(oob_rsq)
  }
  
  # Data frame containing proximity values for a heat map
  if (values == "proximity-heat") {
    if (is.null(object$proximity)) {
      stop("No proximity matrix detected.")
    }
    prox_heat <- melt(as.data.frame(object$proximity))
    names(prox_heat) <- c("obs1", "obs2", "proximity")
    return(prox_heat)
  }
  
  # Data frame containing MDS coordinates of proximity matrix
  if (values == "proximity-mds") {
    if (object$type != "classification") {
      stop(paste("Plot of multi-dimensional scaling coordinates only",
                 "available for classification."))
    }
    if (is.null(object$proximity)) {
      stop("No proximity matrix detected.")
    }
    prox_mds <- as.data.frame(cmdscale(1 - object$proximity, eig = TRUE, 
                                       k = 2)[[1]])
    prox_mds$fac <- if (missing(fac)) as.factor(object$y) else as.factor(fac)
    names(prox_mds) <- c("dim1", "dim2", "class")
    return(prox_mds)
  }
  
  # Data frame containing proximity-based outlier values
  if (values == "proximity-outlier") {
    if (object$type != "classification") {
      stop("Outlier plot only available for classification.")
    }
    if (is.null(object$proximity)) {
      stop("No proximity matrix detected.")
    }
    prox_out <- data.frame("outlyingness" = outlier(object))
    prox_out$obs <- seq_len(nrow(prox_out))
    prox_out$fac <- if (missing(fac)) as.factor(object$y) else as.factor(fac)
    return(prox_out)
  }
  
  # Data frame containing values for parallel coordinates plot
  if (values == "parallel") {
    if (object$type != "classification") {
      stop(paste("Parallel coordinates plot of variable importance scores is",
                 "only available for classification."))
    }
    imp <- importance(object)
    if (ncol(imp) == 1) {
      warning("Rerunning random forest with \"importance = TRUE\".")
      newobj <- update(object, importance = TRUE)
      imp <- importance(newobj)
    }
    pcd <- melt(subset(imp, select = -c(MeanDecreaseAccuracy, 
                                        MeanDecreaseGini)))
    names(pcd) <- c("feature", "class", "score")
    return(pcd)
  }
  
  # Data frame containing values for partial dependence plot
  if (values == "partial") {
    pdd <- as.data.frame(partialPlot(object, ..., plot = FALSE))
    return(pdd)
  }
  
  # Data frame containing OOB error for various values of mtry
  if (values == "margin") {
    if (object$type != "classification") {
      stop(paste("Parallel coordinates plot of variable importance scores is",
                 "only available for classification."))
    }
    m <- margin(object)
    md <- data.frame(margin = as.numeric(m), class = as.factor(names(m)))
    md$index <- seq_len(nrow(md))
    return(md)
  }
  
}


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