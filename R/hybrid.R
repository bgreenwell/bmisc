## CART hybrid example


## Assign observations to terminal node (nice hack from stackoverflow)
assignNode <- function(object, newdata,
#                        type = c("vector", "prob", "class", "matrix"),
                       na.action = na.pass, ...) {
  data <- if (missing(newdata)) eval(object$call$data) else newdata
  object$frame$yval <- as.numeric(rownames(object$frame))
  unname(predict(object, newdata = data, type = "vector", 
                 na.action = na.action, ...))
}
# fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
# prp(fit, nn = TRUE, nn.col = "red3")
# assignNode(fit)
# unique(fit$where)

################################################################################
##
## Create categorical variable from terminal nodes
##
################################################################################
nodeFactor <- function(object, newdata, important = FALSE) {
  data <- if (missing(newdata)) eval(object$call$data) else newdata
  if (important) {  # only keep "important" variables
    data <- data[, c(names(object$variable.importance), 
                     all.vars(formula(object)[[2]]))]
  }
#     data$node <- as.factor(object$where)  # terminal node indicators
  data$node <- as.factor(assignNode(object, newdata = data))
  data
}

# ## Spam example
# library(caret)
# data(spam, package = "kernlab")
# set.seed(101)
# trainID <- createDataPartition(spam$type, p = 0.7, list = FALSE, times = 1)
# trainData <- spam[trainID, ]
# testData <- spam[-trainID, ]
# cartTrain <- rpart(type ~ ., data = train)
# trainData2 <- nodeFactor(rpart(type ~ ., data = trainData))
# testData2 <- nodeFactor(rpart(type ~ ., data = testData))

# ## Boston housing example with 10-fold cross-validation
# data(Boston, package = "MASS")
# ctrl <- trainControl(method = "repeatedcv", number = 10)
# set.seed(101)
# cartTrain<- train(x = subset(Boston, select = -medv),
#                   y = Boston$medv,
#                   method = "rpart",
#                   control = list(minsplit = 70, minbucket = 30),
#                   trControl = ctrl,
#                   tuneLength = 30)
# cartFit <- cartTrain$finalModel
# prp(cartFit, extra = 101, nn = TRUE, nn.col = "red3")
# Boston2 <- nodeFactor(cartFit)
# names(Boston2)[which(names(Boston2) == ".outcome")] <- "medv"
# 
# ## CART/MARS hybrid model for Boston housing data
# library(earth)
# earthFit <- earth(medv ~ ., data = Boston, degree = 2, linpreds = FALSE)
# earthFit2 <- earth(medv ~ ., data = Boston2, degree = 2, linpreds = FALSE)
# earthFit$grsq
# earthFit2$grsq
