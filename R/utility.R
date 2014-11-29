##' List of battery methods
##' 
##' Prints a list of the available battery methods.
##' 
##' @rdname battery
##' @export
##' 
##' @return A list with two components:
##' \itemize{
##'   \item \code{rpart} 
##'   \item \code{randomForest} 
##' }
listBatteryMethods <- function() {
  list("rpart" = c("priors", "shaving", "target"),
       "randomForest" = c("classwt", "shaving", "target", "mtry", "cutoff"))
}

##' Model performance measures
##' 
RMSE <- function (pred, obs, na.rm = FALSE) {
  sqrt(mean((pred - obs)^2, na.rm = na.rm))
}

R2 <- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))  # number of complete cases
  switch(formula, 
         corr = cor(obs, pred, 
                    use = ifelse(na.rm, "complete.obs", "everything"))^2, 
         traditional = 1 - (sum((obs - pred)^2, na.rm = na.rm)/
                              ((n - 1) * var(obs, na.rm = na.rm)))
  )
}