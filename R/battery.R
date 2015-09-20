#' Battery Target
#'
#' Automatic extraction of mutual dependencies among predictors. Here, the word
#' "dependencies," refers to a potentially nonlinear multivariate relationship.
#'
#' @param X A data frame of predictor variables.
#' @param ... Additional optional arguments to be passed on to \code{rpart}.
#'
#' @importFrom caret postResample R2 RMSE
#' @importFrom rpart rpart
#' @importFrom stats predict
#' @examples
#' data(mushroom, package = "misc")
#' bt <- battery_target(Mushroom)
#' bt
#' plot(bt)
battery_target<- function(X, cv = FALSE, ...) {

  nvars <- ncol(X)

  # FIXME: Should we use the cv_tree function that's based on caret::train?
  
  # Allocate space
  trees <- vector("list", length = nvars)  # to store trees
  vi.mat <- diag(rep(NA, nvars))  # to store variable importance
  rownames(vi.mat) <- colnames(vi.mat) <- names(X)
  num_vars <- num_nodes <- r2 <- rmse <- accuracy <- kappa <- numeric(nvars)

  # Create a model using each predictor as the response
  for (i in seq_len(nvars)) {
    form <- formula(paste(names(X)[i], "~",
                          paste(names(X)[-i], collapse = "+")))
    if (cv) {
      .tree <- tryCatch(rpart(form, data = X, 
                                 control = rpart.control(cp = 0), ...),
                        error = function(e) NULL)
      if (!is.null(.tree)) {
        opt <- .tree$cptable[which.min(.tree$cptable[, "xerror"]), "CP"]
        .tree <- prune(.tree, cp = opt)
      }
      trees[[i]] <- .tree
    } else {
      trees[[i]] <- tryCatch(rpart(form, data = X, ...),
                             error = function(e) NULL)
    }
    if (is.null(trees[[i]])) {
      r2[i] <- rmse[i] <- accuracy[i] <- kappa[i] <- NA
    } else {
      vi <- trees[[i]]$variable.importance  # variable importance
      vi <- (vi/sum(vi)*100)[names(vi.mat[i, -i])]  # scale to sum to 100
      vi[is.na(vi)] <- 0  # vi for variables not used as splits or surrogates is 0
      vi.mat[i, -i] <- if (is.null(trees[[i]]$splits)) 0 else vi
      split.vars <- levels(trees[[i]]$frame$var)
      num_vars[i] <- length(split.vars[split.vars != "<leaf>"])  # number of variables used in constructing tree
      num_nodes[i] <- length(unique(trees[[i]]$where))  # number of terminal nodes
      if (trees[[i]]$method == "anova") {
        pred <- predict(trees[[i]], type = "vector")
        r2[i] <- R2(pred, X[[i]], formula = "traditional")
        rmse[i] <- RMSE(pred, X[[i]])
        accuracy[i] <- kappa[i] <- NA
      } else if (trees[[i]]$method == "class") {
        pred <- predict(trees[[i]], type = "class")
        performance <- postResample(pred, X[[i]])
        accuracy[i] <- performance[1]
        kappa[i] <- performance[2]
        r2[i] <- rmse[i] <- NA
      } else {
        r2[i] <- rmse[i] <- accuracy[i] <- kappa[i] <- NA
      }
    }
  }
  out <- list("results" = data.frame("Target" = names(X),
                                     "R2" = r2,
                                     "RMSE" = rmse,
                                     "Accuracy" = accuracy,
                                     "Kappa" = kappa,
                                     "Terminal.nodes" = num_nodes,
                                     "N.vars" = num_vars),
              "vi.mat" = round(vi.mat, digits = 0),
              "trees" = trees)
  class(out) <- "battery_target"
  out

}


#' @keywords internal
print.battery_target <- function(x, ...) {
  print(x$results)
  invisible(x)
}


#' Plot the Output of a Battery Target Run
#'
#' This visualizes the output of a battery target run using a heatmap of the
#' variable importance matrix.
#'
#' @importFrom stats heatmap
#' @export
#' @method plot battery_target
#'
#' @param x An object that inherits from class \code{"battery_target"}.
#' @param ... Additional optional arguments to be passed on to \code{heatmap}.
plot.battery_target <- function(x,...) {
  heatmap(x$vi.mat, symm = TRUE, ...)
  invisible(x)
}
