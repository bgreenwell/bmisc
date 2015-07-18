#' Terminal Node Assignment
#'
#' Assign each observation of the original data (or newdata) to a terminal
#' node. Based on a nice hack from stackoverflow.
#'
#' @rdname assignNode
#' @export
#'
#' @param object An object that inherits from class \code{rpart}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param na.action a function which indicates what should happen when the
#'   data contain \code{NA}s.
#' @param ... Additional optional arguments. At present, no optional arguments
#'   are used.
#'
#' @return A numeric vector containing the terminal node each observation
#'   belongs to.
assignNode <- function(object, newdata, na.action = na.pass, ...) {

  # Extract data if none are specified
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Replace fitted values with the corresponding node number
  object$frame$yval <- rownames(object$frame)# as.numeric(rownames(object$frame))

  # Return node predictions
  unname(predict(object, newdata = .data, type = "vector",
                 na.action = na.action, ...))

}

#' Create CART-like Dummy Variables
#'
#' Augments the supplied data with an additional factor variable describing
#' terminal node assignment.
#'
#' @rdname nodeFactor
#' @export
#'
#' @param object An object that inherits from class \code{rpart}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param important Logical indicating whether to include only important
#'   variables. Default is \code{FALSE}.
#' @param ... Additional optional arguments. At present, no optional arguments
#'   are used.
#'
#' @return A data frame containing the original data with an additional factor
#'   variable containing terminal node assignments.
#'
# #
# # Boston housing data
# #
# 
# # Load packages and data
# library(rpart)  # for CART-like decision trees
# library(earth)  # for MARS-like regression models
# data(Boston, package = "MASS")  # Boston housing data
# 
# # CART model
# boston_cart <- rpart(medv ~ ., data = Boston, cp = 0.005)
# Boston2 <- addNodeFactor(boston_cart)
# 
# # MARS model
# earth(medv ~ ., data = Boston, degree = 2, linpreds = TRUE)
# 
# # Hybrid model
# earth(medv ~ ., data = Boston2, degree = 2, linpreds = TRUE)
addNodeFactor <- function(object, newdata, important = FALSE, ...) {
  .data <- if (missing(newdata)) eval(object$call$data) else newdata
  if (important) {  # only keep "important" variables
    .data <- .data[, c(names(object$variable.importance),
                       all.vars(formula(object)[[2]]))]
  }
  #data$node <- as.factor(object$where)  # terminal node indicators
  within(.data, { node <- as.factor(assignNode(object, newdata = .data)) })
}
