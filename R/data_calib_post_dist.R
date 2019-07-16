#' Posterior distribution of calibrated parameters
#'
#' A dataset with the posterior distribution of the calibrated parameters of 
#' the decision model.
#' @format A matrix with 1000 rows and 3 variables:
#' \describe{
#'   \item{p_S1S2}{Annual transition probability of disease progression (S1 to S2)}
#'   \item{hr_S1}{Hazard rate ratio of death in S1 vs H}
#'   \item{hr_S2}{Hazard rate ratio of death in S2 vs H}
#' }
"m_calib_post"