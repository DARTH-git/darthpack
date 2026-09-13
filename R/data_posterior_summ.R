#' Posterior distribution of calibrated parameters
#'
#' A \code{data.frame} with the summary statistics of the calibrated parameters.
#' @format A \code{data.frame} with three rows, one per calibrated parameter
#' and 6 columns:
#' \describe{
#'   \item{Parameter}{Parameter name.}
#'   \item{Mean}{Posterior mean.}
#'   \item{2.5 percent}{Lower bound of the 95 percent credible interval.}
#'   \item{50 percent}{Posterior median.}
#'   \item{97.5 percent}{Upper bound of the 95 percent credible interval.}
#'   \item{MAP}{Maximum-a-posteriori (MAP) estimate.}
#' }
#' @docType data
"df_posterior_summ"
