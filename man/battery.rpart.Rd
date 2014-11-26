\name{battery.rpart}
\alias{battery.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
battery.rpart(object, type = c("priors", "shaving", "target", "depth"), newdata, .progress = TRUE, prior.grid, prior.min = 0.05, prior.max = 0.95, prior.step = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{.progress}{
%%     ~~Describe \code{.progress} here~~
}
  \item{prior.grid}{
%%     ~~Describe \code{prior.grid} here~~
}
  \item{prior.min}{
%%     ~~Describe \code{prior.min} here~~
}
  \item{prior.max}{
%%     ~~Describe \code{prior.max} here~~
}
  \item{prior.step}{
%%     ~~Describe \code{prior.step} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, type = c("priors", "shaving", "target", "depth"), 
    newdata, .progress = TRUE, prior.grid, prior.min = 0.05, 
    prior.max = 0.95, prior.step = 0.05, ...) 
{
    data <- if (missing(newdata)) 
        eval(object$call$data)
    else newdata
    xname <- intersect(all.vars(formula(object)[[3]]), colnames(data))
    yname <- all.vars(formula(object)[[2]])
    type <- match.arg(type)
    if (type == "priors") {
        if (object$method != "class") {
            stop("battery priors is only available with classification trees")
        }
        y <- data[, yname]
        if (!is.factor(y)) 
            y <- as.factor(y)
        priors <- priorGrid(n = length(unique(y)), .min = prior.min, 
            .max = prior.max, .step = prior.step)
        batteryFits <- vector("list", length = nrow(priors))
        accuracy <- matrix(0, nrow = nrow(priors), ncol = nlevels(y) + 
            1)
        for (i in seq_len(nrow(priors))) {
            batteryFits[[i]] <- update(object, parms = list(prior = priors[i, 
                ]))
            pred_vals <- predict(batteryFits[[i]], newdata = data, 
                type = "class")
            conf_tab <- table(pred_vals, y)
            accuracy[i, nlevels(y) + 1] <- sum(diag(conf_tab))/length(y)
        }
        names(priors) <- paste("prior", levels(y), sep = ".")
        colnames(accuracy) <- c(paste("accuracy", levels(y), 
            sep = "."), "accuracy")
        return(cbind(priors, accuracy))
    }
    if (type == "target") {
        x <- data[, xname]
        x <- x[, unlist(lapply(x, is.numeric))]
        nvars <- length(xname)
        batteryFits <- vector("list", length = nvars)
        vi_matrix <- diag(rep(NA, nvars))
        colnames(vi_matrix) <- xname
        num_vars <- num_nodes <- r_squared <- rmse <- numeric(nvars)
        for (i in seq_len(nvars)) {
            tmp_y <- x[, i]
            tmp_data <- data.frame(x[, -i], tmp_y = tmp_y)
            batteryFits[[i]] <- rpart(tmp_y ~ ., data = tmp_data, 
                ...)
            vi <- batteryFits[[i]]$variable.importance
            vi <- (vi/sum(vi) * 100)[names(vi_matrix[i, -i])]
            vi[is.na(vi)] <- 0
            vi_matrix[i, -i] <- if (is.null(batteryFits[[i]]$splits)) 
                0
            else vi
            num_vars[i] <- length(vi[vi > 0])
            num_nodes[i] <- length(unique(batteryFits[[i]]$where))
            pred <- predict(batteryFits[[i]], type = "vector")
            r_squared[i] <- R2(pred, tmp_y, formula = "traditional")
            rmse[i] <- RMSE(pred, tmp_y)
        }
        out <- data.frame(Target = xname, R.squared = r_squared, 
            RMSE = rmse, Terminal.nodes = num_nodes, N.vars = num_vars, 
            round(vi_matrix, digits = 0))
        return(list(results = out, fits = batteryFits))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
