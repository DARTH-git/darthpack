#' PSA object of the cost-effectiveness analysis decision model
#'
#' An object of class \code{psa} with PSA inputs and outputs for the Sick-Sicker model.
#' @format A list with 11 rows and 5 variables:
#' \describe{
#'   \item{n_strategies}{Number of strategies.}
#'   \item{strategies}{Strategy names.}
#'   \item{n_sim}{Number of PSA samples.}
#'   \item{cost}{A data frame with \code{n_sim} rows and \code{n_strategies} columns with the cost per strategy for all PSA samples.}
#'   \item{effectiveness}{A data frame with \code{n_sim} rows and \code{n_strategies} columns with the effectiveness per strategy for all PSA samples.}
#'   \item{parameters}{Survival target. A data frame of input parameters with 
#'   \code{n_sim} rows and 15 columns:
#'     \itemize{\item c_H: Annual cost for healthy individuals
#'              \item c_S1: Annual cost for sick individuals in S1
#'              \item c_S2: Annual cost for sick individuals in S2
#'              \item c_D: Annual cost for dead individuals
#'              \item c_Trt: Additional costs of sick individuals treated in S1 or S2
#'              \item u_H: Utility for healthy individuals
#'              \item u_S1: Utility for sick individuals in S1
#'              \item u_S2: Utility for sick individuals in S1
#'              \item u_D: Utility for dead individuals
#'              \item u_Trt: Utility for treated individuals in S1
#'              \item p_HS1: Annual transition probability of disease onset (H to S1)
#'              \item p_S1H: Annual transition probability of recovery (S1 to H)
#'              \item p_S1S2: Annual transition probability of disease progression (S1 to S2)
#'              \item hr_S1: Hazard rate ratio of death in S1 vs H
#'              \item hr_S2: Hazard rate ratio of death in S2 vs H
#'              \item n_age_init: Initial age of the population
#'              \item n_t: Time horizon in years
#'              \item d_c: Annual discount rate for costs
#'              \item d_e: Annual discount rate for effectiveness}}
#'   \item{parnames}{A vector of strings with parameter names.}
#'   \item{currency}{A string with the currency.}
#' }
"l_psa"