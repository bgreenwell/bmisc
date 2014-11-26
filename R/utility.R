##' Make a grid of prior values
##' 
##' Creates a grid of possible prior values.
##' 
##' @keywords internal
makePriorGrid <- function(n = 2, .min = 0.05, .max = 0.95, .step = 0.05) {
  
  ## Allocate space
  prior_list <- vector("list", length = n-1)
  
  ## Sequence of possible prior values
  prior_vals <- seq(from = .min, to = .max, by = .step)
  
  ## Create the grid of prior values
  for (i in seq_len(n - 1)) {
    prior_list[[i]] <- prior_vals
  }
  prior_grid <- expand.grid(prior_list)
  prior_grid$last <- apply(prior_grid, 1, function(x) 1 - sum(x))
  keep <- which(prior_grid$last >= .min & prior_grid$last <= .max)
  prior_grid <- as.data.frame(prior_grid[keep, ])
  #   stopifnot(all(apply(prior_grid, 1, sum) == 1))
  setNames(prior_grid, paste("prior", seq_len(n), sep = ""))
  
}

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