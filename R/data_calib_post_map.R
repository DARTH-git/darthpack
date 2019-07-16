#' Maximum-a-posteriori (MAP) estimate of calibrated parameters
#'
#' A vector with the maximum-a-posteriori (MAP) estimate of the calibrated 
#' parameters of the decision model.
#' @format A vector with 3 variables:
#' \describe{
#'   \item{p_S1S2}{Annual transition probability of disease progression (S1 to S2)}
#'   \item{hr_S1}{Hazard rate ratio of death in S1 vs H}
#'   \item{hr_S2}{Hazard rate ratio of death in S2 vs H}
#' }
"v_calib_post_map"