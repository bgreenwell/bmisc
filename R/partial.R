#' Partial Dependence Values
#'
#' Compute partial dependence values from an appropriate R object.
#'
#' @param object An appropriate R object.
#' @param xname Character string specifying the predictor variable of interest.
#' @param xvalues Vector of values of \code{xname} for which partial dependence
#'   values are sought.
#' @param newdata Data frame containing data for all variables of interest in
#'   the model.
#' @param ... Additional optional arguments to be passed onto \code{predict}.
#'
#' @note Only used for regression (i.e.,continuous response values). This may
#'   change in the future. Also, this function only calculates partial 
#'   dependence for one independent variabe at a time. Bivariate partial
#'   dependence will be available in a future release.
#' 
#' @importFrom plyr ddply
#' @importFrom stats predict
#'
#' @export
partial_values <- function(object, xname, xvalues, newdata, ...) {

  # Data for computing partial dependence values
  if (missing(newdata)) {
    newdata <- eval(object$call$data)
    if (is.null(newdata)) {
      stop("No data avaialble.")
    }
  }

  # Predictor values
  if (missing(xvalues)) {
    xvalues <- sort(unique(newdata[[xname]]))
  }

  # It may be better to use plyr::ddply here!
  
  # Partial dependence values
#   pd <- sapply(xvalues, function(x) {
#     temp <- newdata
#     temp[[xname]] <- x
#     mean(predict(object, newdata = temp, ...), na.rm = TRUE)
#   })
  pd <- ddply(xvalues, function(x) {
    temp <- newdata
    temp[[xname]] <- x
    mean(predict(object, newdata = temp, ...), na.rm = TRUE)
  })

  # Return data frame of results
  ret <- data.frame(xvalues, pd)
  names(ret) <- c(xname, "y")
  return(ret)

}


# # Example: regression tree -----------------------------------------------------
# library(randomForest)
# data(airquality)
#
#
# set.seed(101)
# rf <- randomForest(Ozone ~ ., data = airquality, na.action = na.omit)
# partialPlot(rf, airquality, "Temp", n.pt = 100)
# pd <- partial_values(rf, xname = "Temp", xvalues = seq(from = 56, to = 97, length = 100))
# plot(pd, type = "l")
