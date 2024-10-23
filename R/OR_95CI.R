#' Calculate Odds Ratios with 95% Confidence Intervals
#'
#' @param coef A vector of coefficient estimates.
#' @param se A vector of standard errors.
#' @param siglevel Significance level for confidence intervals.
#' @param roundto Number of decimal places to round the result.
#' @return A formatted string with OR and confidence intervals.
#' @examples
#' coef <- c(0.5, -0.2)
#' se <- c(0.1, 0.05)
#' OR_95CI(coef, se, 0.05, 2)
#' @export
OR_95CI <- function(coef, se, siglevel = 0.05, roundto = 2) {
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(
    format(round(OR, roundto), nsmall = roundto),
    " (",
    format(round(ORlcl, roundto), nsmall = roundto),
    ", ",
    format(round(ORucl, roundto), nsmall = roundto),
    ")"
  )
  return(ORresult)
}

library(usethis)

toydata <- read.table("inst/extdata/toydata.txt", header = TRUE)
use_data(toydata, overwrite = TRUE)
