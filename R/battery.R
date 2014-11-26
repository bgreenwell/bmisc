##' Batteries of Runs
##' 
##' Tree-based algorithms are characterized by a substantial number of control 
##' settings. Often the optimal values for many parameters cannot be determined 
##' beforehand and require a trial and error experimental approach. In other 
##' cases, it is desirable to try various settings to study their impact on the 
##' resulting models. CART batteries were designed to automate the most 
##' frequently-occurring modeling situations that require multiple collections 
##' of CART runs.
##' 
##' @rdname battery
##' @export
##' 
##' @param object An object that inherits from class \code{rpart} or 
##'               \code{randomForest}.
##' @param battery The type of battery to perform. For the entire list of 
##'                battery methods use \code{batteryMethods("rpart")}.
##'                
##' @param ... Additional optional arguments to be passed onto \code{rpart} or 
##'            \code{randomForest}.
##'            
##' @return A data frame.
##'
##' @references
##' Salford Systems
##' 
##' @examples
##' ## Spam example 
##'
##' ## Load packages
##' library(caret)
##' library(rpart)
##' 
##' ## Load and partition data
##' data(spam, package = "kernlab")
##' set.seed(101)
##' trainID <- createDataPartition(spam$type, p = 0.7, list = FALSE, times = 1)
##' trainSample <- spam[trainID, ]
##' testSample <- spam[-trainID, ]

##' ## CART
##' cartFit <- rpart(type ~ ., data = trainSample)

##' ## Battery priors
##' table(trainSample$type)/nrow(trainSample)  # default priors
##' res <- battery(cartFit, battery = "priors")
##' res[order(res$error), ]  # order the results
battery <- function(object, ...) {
  UseMethod("battery")
}

##' @rdname battery
##' @export
##' @method battery rpart
battery.rpart <- function(object, 
                          type = c("priors", "shaving", "target", "depth"),
                          newdata, .progress = TRUE, 
                          prior.grid, prior.min = 0.05, prior.max = 0.95, 
                          prior.step = 0.05,
                          ...) {
  
  data <- if (missing(newdata)) eval(object$call$data) else newdata
  xname <- intersect(all.vars(formula(object)[[3]]), colnames(data)) 
  yname <- all.vars(formula(object)[[2]])
  
  ## Determine battery type
  type <- match.arg(type)
  
  ## Battery priors ------------------------------------------------------------
  if (type == "priors") {
    
    ## This battery is only available for classification trees
    if (object$method != "class") {
      stop("battery priors is only available with classification trees")
    }
    
    ## Response values
    y <- data[, yname]
    if (!is.factor(y)) y <- as.factor(y)
    
    ## Grid of prior values
    priors <- priorGrid(n = length(unique(y)), .min = prior.min, 
                        .max = prior.max, .step = prior.step)
    
    ## Allocate space
    batteryFits <- vector("list", length = nrow(priors))  # to store trees
#     auc <- numeric(nrow(priors))  # to store area under ROC curve
    accuracy <- matrix(0, nrow = nrow(priors), ncol = nlevels(y)+1)  
    
    ## Create a model for each set of priors
    for (i in seq_len(nrow(priors))) {
      batteryFits[[i]] <- update(object, parms = list(prior = priors[i, ]))
      pred_vals <- predict(batteryFits[[i]], newdata = data, type = "class")
      conf_tab <- table(pred_vals, y)
      accuracy[i, nlevels(y)+1] <- sum(diag(conf_tab)) / length(y)
      ## FIXME: need to add individual class accuracy
    }

    ## Return results
    names(priors) <- paste("prior", levels(y), sep = ".")
    colnames(accuracy) <- c(paste("accuracy", levels(y), sep = "."), "accuracy")
    return(cbind(priors, accuracy))
    
  }
  
#   ## Battery oneoff ------------------------------------------------------------
#   if (type == "oneoff") {
#     
#     num_batteries <- length(xname)
#     obj_data <- eval(object$call$data)
#     
#     ## Allocate space
#     batteryFits <- vector("list", length = num_batteries)  # to store trees
#     
#     ## Create a model using each predictor as the response
#     for (i in seq_len(num_batteries)) {
#       tmp_data <- subset(obj_data, select = c(xname[i], yname))
#       batteryFits[[i]] <- update(object, data = tmp_data)
#     }
#     
#   }
# 
#   ## Battery shaving -----------------------------------------------------------
#   if (battery == "shaving") {
#     stop("battery shaving not yet implemented for rpart objects")
#   }
# 
  ## Battery target ----------------------------------------------------------
  if (type == "target") {
    
    x <- data[, xname]
    x <- x[, unlist(lapply(x, is.numeric))]  # only consider numeric variables
    nvars <- length(xname)
    
    ## Allocate space
    batteryFits <- vector("list", length = nvars)  # to store trees
    vi_matrix <- diag(rep(NA, nvars))  # to store variable importance 
    colnames(vi_matrix) <- xname  
    num_vars <- num_nodes <- r_squared <- rmse <- numeric(nvars)
    
    ## Create a model using each predictor as the response
    for (i in seq_len(nvars)) {
      ## Battery fit
      tmp_y <- x[, i]
      tmp_data <- data.frame(x[, -i], "tmp_y" = tmp_y)
      batteryFits[[i]] <- rpart(tmp_y ~ ., data = tmp_data, ...)
      vi <- batteryFits[[i]]$variable.importance  # variable importance
      vi <- (vi/sum(vi)*100)[names(vi_matrix[i, -i])]  # scale to sum to 100
      vi[is.na(vi)] <- 0  # vi for variables not used as splits or surrogates is 0
      vi_matrix[i, -i] <- if (is.null(batteryFits[[i]]$splits)) 0 else vi
      num_vars[i] <- length(vi[vi > 0])  # number of variables used in constructing tree
      num_nodes[i] <- length(unique(batteryFits[[i]]$where))  # number of terminal nodes
      pred <- predict(batteryFits[[i]], type = "vector")
      r_squared[i] <- R2(pred, tmp_y, formula = "traditional")
      rmse[i] <- RMSE(pred, tmp_y)
    }
    out <- data.frame("Target" = xname, 
                      "R.squared" = r_squared,
                      "RMSE" = rmse,
                      "Terminal.nodes" = num_nodes,
                      "N.vars" = num_vars,  # wrong (includes surrogates)
                      round(vi_matrix, digits = 0))
    return(list("results" = out, "fits" = batteryFits))
    
  }
  
}

# ## Spam example ----------------------------------------------------------------
# 
# ## Load packages
# library(caret)
# library(rpart)
# 
# ## Load and partition data
# data(spam, package = "kernlab")
# set.seed(101)
# trainID <- createDataPartition(spam$type, p = 0.7, list = FALSE, times = 1)
# trainSample <- spam[trainID, ]
# testSample <- spam[-trainID, ]
# 
# ## CART
# cartFit <- rpart(type ~ ., data = trainSample)
# 
# ## Battery priors
# table(trainSample$type)/nrow(trainSample)  # default priors
# res <- battery(cartFit, newdata = trainSample, battery = "priors")
# res[order(res[, "accuracy"], decreasing = TRUE), ]
# 
# ## Battery target
# spam_target <- battery(cartFit, battery = "target", 
#                        control = list(maxsurrogate = 0))
# spam_target$results[order(spam_target$results$R.squared, decreasing = TRUE), 1:5]
# tree34 <- spam_target$fits[[34]]
# prp(tree34)
# library(plotmo)
# plotmo(tree34, data = spamTrain)
# 
# ## Using confusionMatrix from caret package
# fit <- rpart(Species ~ ., data = iris)
# pred <- predict(fit, type = "class")
# ctab <- table(pred = pred, truth = iris$Species)
# cmat <- confusionMatrix(ctab)
# 
# ## Bouston housing example -----------------------------------------------------
# data(Boston, package = "MASS")
# Boston2 <- Boston
# Boston2$x <- log(Boston2$nox) + sin(Boston2$rm)
# fit <- rpart(medv ~ ., data = Boston2)
# battery(fit, battery = "target", control = list(maxsurrogate = 0))
# 
# ## Wine example ----------------------------------------------------------------
# 
# ## Import data
# wine <- read.csv("/home/w108bmg/Downloads/winequality-red.csv", sep = ";",
#                  header = TRUE)
# set.seed(101)
# trainID <- createDataPartition(wine$quality, p = 0.8, list = FALSE, times = 1)
# wineTrain <- wine[trainID, ]
# wineTest <- wine[-trainID, ]
# wineCART <- rpart(quality ~ ., data = wineTrain)
# wineTarget <- battery(wineCART, battery = "target",
#                       control = list(maxsurrogate = 0))
# wineTarget
# densityCART <- wineTarget$batteries[[8]]