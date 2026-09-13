#' Calibration targets for the Sick-Sicker model
#'
#' A list with calibration targets for the Sick-Sicker model. Each target is a
#' data frame with one row per target time (10, 20 and 30 years) and the
#' following 8 variables: \code{Target}, the target name; \code{Time}, the time
#' in years; \code{Num}, the numerator; \code{Pop}, the denominator;
#' \code{value}, the target value; \code{se}, its standard error; and \code{lb}
#' and \code{ub}, the lower and upper bounds of its 95 percent confidence
#' interval.
#' @format A list with three calibration targets:
#' \describe{
#'   \item{Surv}{Survival target. \code{Num} is the number of people alive and
#'     \code{Pop} the population at risk.}
#'   \item{Prev}{Prevalence target. \code{Num} is the number of sick people
#'     (either Sick or Sicker) and \code{Pop} the population at risk.}
#'   \item{PropSicker}{Proportion of Sicker people target. \code{Num} is the
#'     number of sick people in the Sicker state and \code{Pop} the population
#'     of sick people (either Sick or Sicker).}
#' }
#' @docType data
"SickSicker_targets"
