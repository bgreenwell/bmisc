##' Terminal Node Assignment
##' 
##' Assign each observation of the original data (or newdata) to a terminal 
##' node. Based on a nice hack from stackoverflow.
##' 
##' @param object An object that inherits from class \code{rpart}.
##' @param newdata An optional data frame in which to look for variables with 
##'   which to predict. If omitted, the fitted values are used.
##' @param na.action a function which indicates what should happen when the 
##'   data contain \code{NA}s. 
##' @param ... Additional optional arguments. At present, no optional arguments 
##'   are used.
##' 
##' @references
##' https://www.salford-systems.com/resources/whitepapers
assignNode <- function(object, newdata, na.action = na.pass, ...) {
  data <- if (missing(newdata)) eval(object$call$data) else newdata
  object$frame$yval <- as.numeric(rownames(object$frame))
  unname(predict(object, newdata = data, type = "vector", 
                 na.action = na.action, ...))
}

##' Create CART-like Dummy Variables
##' 
##' Returns the supplied data with an additional factor variable describing
##' node assignment.
##' 
##' @param object An object that inherits from class \code{rpart}.
##' @param newdata An optional data frame in which to look for variables with 
##'   which to predict. If omitted, the fitted values are used.
##' @param important Logical indicating whether to include only important 
##'   variables. Default is \code{FALSE}.
##' @param ... Additional optional arguments. At present, no optional arguments 
##'   are used.
##' 
##' @examples
##' ## Boston housing data
##' library(rpart)
##' library(rpart.plot)
##' data(Boston, package = "MASS")
##' boston_cart <- rpart(medv ~ ., data = Boston, cp = 0.005)
##' prp(boston_cart, extra = 101, nn = TRUE, nn.col = "red3")
##' Boston2 <- nodeFactor(boston_cart)  # compare to previous plot
##' 
##' ## CART/MARS hybrid model for Boston housing data
##' library(earth)
##' 
##' ## Original model
##' earth(medv ~ ., data = Boston, degree = 2, linpreds = T)
##' 
##' ## Hybrid model
##' earth(medv ~ ., data = Boston2, degree = 2, linpreds = T)
##' 
##' @references
##' https://www.salford-systems.com/resources/whitepapers
nodeFactor <- function(object, newdata, important = FALSE, ...) {
  .data <- if (missing(newdata)) eval(object$call$data) else newdata
  if (important) {  # only keep "important" variables
    .data <- .data[, c(names(object$variable.importance), 
                       all.vars(formula(object)[[2]]))]
  }
#     data$node <- as.factor(object$where)  # terminal node indicators
  within(.data, { node <- as.factor(assignNode(object, newdata = .data)) })
}
