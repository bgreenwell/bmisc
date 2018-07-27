#' Simulate random intercept and slope data
#'
#' Simulate data from a (balanced) random intercept and slope model.
#'
#' @param m Integer specifying the number of subjects.
#' 
#' @param n Integer specifying the number of observations per subject.
#'
#' @param mu Numeric vector of length two giving the means of the random 
#' intercept and slope, respectively.
#'
#' @param sigma Numeric vector of length three giving the standard deviations of 
#' the random intercept, slope, and error term, respectively.
#' 
#' @export
#' 
#' @examples
#' set.seed(101)  # for reproducibility
#' df <- sim_ris_data(m = 100, n = 15)
#' lattice::xyplot(y ~ x, data = df, groups = subject, type = "l", 
#'                 col = "black", alpha = 0.2)
sim_ris_data <- function(m = 30, n = 10, mu = c(4.8, 3), 
                         sigma = c(1.3, 0.7, 0.7)) {
  x <- rep(seq(from = 0, to = 10, length = n), times = m)
  subject <- rep (1:m, each = n)
  b0 <- rnorm(m, mean = mu[1L], sd = sigma[1L])
  b1 <- rnorm(m, mean = mu[2L], sd = sigma[2L])
  y <- rnorm (m * n, mean = b0[subject] + b1[subject] * x, sd = sigma[3L])
  data.frame(x, y, subject)
}
