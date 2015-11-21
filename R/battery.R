#' Battery Target
#' 
#' Battery Target gives you powerful means to automatically explore and extract 
#' all mutual dependencies among predictors. By the word "dependencies," we mean 
#' a potentially nonlinear multivariate relationship that goes way beyond the 
#' simplicity of conventional correlations. Furthermore, as a powerful side 
#' effect, this Battery provides general means for missing value imputation, 
#' which is extremely useful to support those modeling engines that do not 
#' directly handle missing values. Helps identify clusters of related variables.
#' 
#' @param X A data frame containing the variables of interest.
#' @importFrom rpart rpart
#' @importFrom caret varImp
#' @importFrom Matrix forceSymmetric
#' @importFrom plyr ldply
#' @export
battery_target <- function(X, surrogates = FALSE, competes = TRUE, 
                           scale.importance = TRUE, symmetric = TRUE,
                           do.trace = FALSE, ...) {
  
  # Coerce X into a dsta frame if it is not already so
  if (class(X) != "data.frame") {
    X <- as.data.frame(X)
  }
  
  # Number of variables/trees
  nvars <- ncol(X)
  if (nvars < 2) {
    stop("Battery target requires at least two variables.")
  }
  
  # Initialize list to store trees
  trees <- vector("list", length = nvars)
  names(trees) <- names(X)
  
  # Initialize matrix to hold pairwise variable importance scores
  vimat <- matrix(nrow = nvars, ncol = nvars)
  rownames(vimat) <- colnames(vimat) <- names(X)
  
  # Loop through each variable while using it as the current target
  for (i in seq_len(nvars)) {
    
    # Assign variable roles and model formula
    target <- names(X)[i]
    predictors <- names(X)[-i]
    form <- formula(paste(target, paste(predictors, collapse = "+"), sep = "~"))
    
    # Print trace information (if requested)
    if (do.trace) {
      cat("Battery", i, "of", nvars, 
          paste0("(current target: ", target, ")"), "\n")
    }
    
    # Fit a CART-like decision tree to the data. Here, we wrap the model fitting
    # code in tryCatch() inorder to capture any errors or warnings. For example,
    # if one of the variables is a constant, then rpart() will throw an error 
    # when that variable is being used as the response.
    .tree <- tryCatch(rpart(form, data = X, x = TRUE, y = TRUE, ...),
                      error = function(e) "error",
                      warning = function(w) "warning")
    
    if (class(.tree) == "rpart") {
      
      # FIXME: Should vimat be non-symmetric? For example, shouldn't vimat[i, j]
      # represent the variable importance when variable i is the target and 
      # variable j is a predictor while vimat[j, i] is the variable importance 
      # when variable j is the target and variable i is a predcitor. Hence,
      # vimat[i, j] != vimat[j, i].
      
      # Variable importance
      if (is_root(.tree)) {
        # vimat[i, ] <- vimat[, i] <- 0
        vimat[i, ] <- 0
      } else {
        # FIXME: It is suggested by Salford Systems that these be scaled by 
        #        overall model accuracy. How can this be done?
        vimp <- varImp(.tree, surrogates = surrogates, competes = competes)
        if (scale.importance) {
          # FIXME: These should be scaled by overall model accuracy!
          vimp <- vimp / sum(vimp) * accuracy(.tree)
        }
        # vimat[i, ] <- vimat[, i] <- vimp[names(X), ]
        vimat[i, ] <- vimp[names(X), ]
      }
      
    } else {
      
      # Variable importance
      # vimat[i, ] <- vimat[, i] <- NA
      vimat[i, ] <- NA
      
    }
    
    # FIXME: What to do with diagonal elements and missing values? Perhaps it's
    # best to scale the variable importance scores to lie in [0, 1]. Then
    # diagonal elements could be set to 1 while NAs couls be set to 0?
    diag(vimat) <- 1  # what about diagonal elements that are NA?
    vimat[is.na(vimat)] <- 0
    
    # Symmetrize matrix of pairwise variable importance scores
    if (symmetric) {
      vimat <- forceSymmetric(vimat) 
    }
    
    # Assign tree
    trees[[i]] <- .tree
    
  }
  
  # Data frame of performance measures, etc. for each tree
  trees_summary <- ldply(trees, summarize_tree, .id = "Target")
  
  # Return list of results
  res <- list("trees" = trees,
              "summary" = trees_summary,
              "vimat" = vimat)
  class(res) <- "battery_target"
  res
  
  
}


#' Overall Model Accuracy
#' 
#' \code{accuracy} returns a single number between zero and one describing the 
#' overall accuracy of the decision tree.
#' 
#' @param x An object that inherits from class \code{"rpart"}.
#' @importFrom caret R2
#' @importFrom e1071 classAgreement
#' @importFrom stats predict
#' @keywords internal
accuracy <- function(object, formula = "corr", na.rm = FALSE) {
  if (object$method == "anova") {
    # For regression trees, we simply report the value of R-squared
    pred <- predict(object, type = "vector")
    obs <- object$y
    acc <- R2(pred, obs, formula = formula, na.rm = na.rm)
  } else if (object$method == "class") {
    # For classification trees we report the prediction accuracy. However, it
    # would be nice if this could take Kappa into account. Perhaps just average
    # the two together?
    pred <- predict(object, type = "class")
    obs <- object$y
    acc <- classAgreement(table(obs, pred))$diag
  } else {
    stop("This function only works for classification and regression trees.")
  }
  acc  # overall model accuracy
}


#' Summarize Decision Trees
#' 
#' \code{summarize_tree} returns a basic summary of a decision tree.
#' 
#' @param object An object that inherits from class \code{"rpart"}.
#' @param na.rm Logical indicating whether or not \code{NA}s should be removed.
#'   Default is \code{FALSE}.
#' @param formula Character string specifying which formula to use in 
#'   calculating R-squared. Default is \code{"corr"}, meaning use the squared
#'   correlation between the observed and predicted values.  The only other 
#'   option is \code{"traditional"}. See Kvalseth (1985) for a summary of the 
#'   different approaches.
#' @keywords internal
summarize_tree <- function(object, na.rm = FALSE, formula = "corr") {
  if (class(object) == "rpart") {
    if (object$method == "anova") {
      summarize_regression_tree(object, na.rm = na.rm, formula = formula)
    } else if (object$method == "class") {
      summarize_classification_tree(object)
    } else {
      c("RMSE" = NA, 
        "R2" = NA, 
        "Accuracy" = NA, 
        "Kappa" = NA,
        "Splits" = NA,
        "Splitters" = NA)
    }
  } else {
    c("RMSE" = NA, 
      "R2" = NA, 
      "Accuracy" = NA, 
      "Kappa" = NA,
      "Splits" = NA,
      "Splitters" = NA)
  }
}


#' Is the Tree a Root?
#' 
#' \code{is_root} tests if an \code{"rpart"} object is just a root (i.e., no
#' splits).
#' 
#' @param object An object that inherits from class \code{"rpart"}.
#' @keywords internal
is_root <- function(object) {
  if (class(object) != "rpart") {
    stop('is_root only works for "rpart" objects.')
  }
  is.null(object$splits)  # no splits implies root
}


#' Summarize Regression Trees
#' 
#' \code{summarize_regression_tree} returns a basic summary of a regression 
#' tree.
#' @importFrom caret R2 RMSE
#' @importFrom stats predict
#' @keywords internal
summarize_regression_tree <- function(tree, na.rm = FALSE, formula = "corr") {
  pred <- predict(tree, type = "vector")
  obs <- tree$y
  splits <- splitters(tree)
  c("RMSE" = RMSE(pred, obs, na.rm = na.rm), 
    "R2" = R2(pred, obs, formula = formula, na.rm = na.rm), 
    "Accuracy" = NA, 
    "Kappa" = NA,
    "Splits" = splits$Splits,
    "Splitters" = splits$Splitters)
}


#' Summarize Classification Trees
#' 
#' \code{summarize_classification_tree} returns a basic summary of a 
#' classification tree.
#' 
#' @importFrom e1071 classAgreement
#' @importFrom stats predict
#' @keywords internal
summarize_classification_tree <- function(tree) {
  pred <- predict(tree, type = "class")
  obs <- tree$y
  ca <- classAgreement(table(obs, pred))
  splits <- splitters(tree)
  c("RMSE" = NA, 
    "R2" = NA, 
    "Accuracy" = ca$diag, 
    "Kappa" = ca$kappa,
    "Splits" = splits$Splits,
    "Splitters" = splits$Splitters)
}


#' Extract Splitting Variables
#' 
#' \code{splitters} returns a vector of variable names used to partition
#' the data.
#'
#' @param x An object that inherits from class \code{"rpart"}.
#' @keywords internal
splitters <- function(x) {
  if (class(x) == "rpart") {
    if (is_root(x)) {
      list("Splits" = 0, 
           "Splitters" = NA)
    } else {
      vars <- as.character(x$frame$var)
      vars <- vars[vars != "<leaf>"]
      list("Splits" = length(vars), 
           "Splitters" = paste(unique(vars), collapse = ", "))
    }
  } else {
    list("Splits" = NA, 
         "Splitters" = NA)
  }

}


#' Variable Importance Heatmap
#' 
#' Produces a heatmap of all the pairwise variable importance scores.
#' 
#' @param x An object that inherits from class \code{"battery_target"}.
#' @param ... Additional optional arguments to be passed onto \code{d3heatmap}.
#' @importFrom d3heatmap d3heatmap
#' @export
plot.battery_target <- function(x, ...) {
  d3heatmap(x$vimat, symm = TRUE, ...)
}


#' @keywords internal
print.battery_target <- function(x, ...) {
  print(x$summary, ...)
  invisible(x)
}
