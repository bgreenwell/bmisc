#' K-Fold Cross-Validation Tree
#' 
#' Build a decision tree based on k-fold cross-validation.
#' 
#' @param x A matrix or predictor variables.
#' @param y A vector of response values.
#' @param cv.folds In teger representing the number of folds to use for cross-
#'   validation.
#' @importFrom caret train
#' @importFrom rpart rpart
#' @export
#' @examples
#' data(mushroom, package = "misc")
#' tree <- cv_tree(x = subset(mushroom, select = -Edibility),
#'                 y = mushroom$Edibility)
#' plot(tree)               
cv_tree <- function(x, y, cv.folds = 10, tune.length = 10) {
  
  # Train an rpart model
  rpart.tune <- train(x = x, y = y, 
                      method = "rpart", 
                      metric = ifelse(is.factor(y), "Accuracy", "R2"),
                      trControl = trainControl(method = "cv", 
                                               number = cv.folds),
                      tuneLength = tune.length)
  rpart.tune
  
}