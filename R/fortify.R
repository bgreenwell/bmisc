#' Random Forest Summary Data
#' 
#' Extract information from a \code{"randomForest"} object into a data frame.
#' 
#' @param object An object of class \code{"randomForest"}.
#' @param type The type of data to collect.
#' @param ... Additional arguments.
#' @param fac A factor that was used as the response to train the random forest.
#'   Only used if \code{values = "proximity-mds"} or 
#'   \code{values = "proximity-outlier"}.
#' @return A data frame.
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
    stop(paste("ggRandomForest does not currently support", 
               "unsupervised random forests"))
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
