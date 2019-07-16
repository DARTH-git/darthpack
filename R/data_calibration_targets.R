#' Calibration targets for the Sick-Sicker model
#'
#' A list with calibration targets for the Sick-Sicker model
#' @format A list with three calibration targets:
#' \describe{
#'   \item{Surv}{Survival target. A data frame with 3 rows and 8 variables:
#'     \itemize{\item Target: Target name
#'              \item Time: Time in years
#'              \item Num: Number of people alive
#'              \item Pop: Population at risk
#'              \item value: Target value
#'              \item se: Standard error
#'              \item lb: 95\% CI lower bound
#'              \item ub: 95\% CI lower bound} }
#'   \item{Prev}{Prevalence target. A data frame with 3 rows and 8 variables:
#'     \itemize{\item Target: Target name
#'              \item Time: Time in years
#'              \item Num: Number of sick people (either Sick or Sicker) 
#'              \item Pop: Population at risk
#'              \item value: Target value
#'              \item se: Standard error
#'              \item lb: 95\% CI lower bound
#'              \item ub: 95\% CI lower bound} }
#'   \item{PropSicker}{Proportion of Sicker people target. A data frame with 3 rows and 8 variables:
#'     \itemize{\item Target: Target name
#'              \item Time: Time in years
#'              \item Num: Number of sick people in the Sicker state
#'              \item Pop: Population of sick people (either Sick or Sicker) 
#'              \item value: Target value
#'              \item se: Standard error
#'              \item lb: 95\% CI lower bound
#'              \item ub: 95\% CI lower bound} }
#' }
#' @md
"SickSicker_targets"