#' Sample Size Calculations for the Log-Rank Test
#' 
#' Calculates the required sample size for a log-rank test using Freedman's
#' method.
#' 
#' @param S1 ?.
#' @param S2 ?.
#' @param sig.level Significance level (Type I error probability).
#' @param power Power of test (1 minus Type II error probability).
#' @param alternative Character string specifying the form of the alternative 
#'   hypothesis. Must be one of \code{"two.sided"} (default), \code{"greater"} 
#'   or \code{"less"}.
#' @param method Character string specifying the method to use in calculating
#'   the required sample size. (Currently ignored.)
#' 
#' @return An estimate of the required sample size.
#' 
#' @details 
#' PASS Sample Size Software, Chapter 700:
#' The power calculations used here assume an underlying exponential 
#' distribution. However, we are rarely in a position to assume exponential 
#' survival times in an actual clinical trial. How do we justify the exponential 
#' survival time assumption? First, the logrank test and the test derived using 
#' the exponential distribution have nearly the same power when the data are in 
#' fact exponentially distributed. Second, under the proportional hazards model 
#' (which is assumed by the logrank test), the survival distribution can be 
#' transformed to be exponential and the logrank test remains the same under 
#' monotonic transformations.
#' 
#' @note Returns \code{Inf} when \code{S1 == S2}.
#' @export
#' @examples
#' pwr.logRank(0.3, 0.4)
pwr.logRank <- function(S1, S2, sig.level = 0.05, power = 0.8, 
                        alternative = c("two.sided", "less", "greater"),
                        method = c("Freedman")) {
  
  # FIXME: Relabel S1 and S2 as S.ctrl and S.trt
  
  alt <- match.arg(alternative)
  za <- if (alt == "two.sided") qnorm(sig.level / 2) else qnorm(sig.level)
  zb <- qnorm(1 - power)
  haz.ratio <- log(S1) / log(S2)
  cat("Expected number of events:", 4 * (za + zb) ^ 2 / log(1 / haz.ratio) ^ 2)
  cat("\n")
  (((haz.ratio + 1) / (haz.ratio - 1)) ^ 2) * (za + zb) ^ 2 / (2 - S1 - S2)
}
