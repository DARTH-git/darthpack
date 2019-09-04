#' Generate PSA dataset of CEA parameters
#'
#' \code{generate_psa_params} generates PSA input dataset by sampling decision 
#' model parameters from their distributions. The sample of the calibrated
#' parameters is a draw from their posterior distribution obtained with the
#' IMIS algorithm.
#' @param n_sim Number of PSA samples.
#' @param seed Seed for reproducibility of Monte Carlo sampling.
#' @return 
#' A data frame with \code{n_sim} rows and 15 columns of parameters for PSA. 
#' Each row is a parameter set sampled from distributions that characterize 
#' their uncertainty
#' @examples 
#' generate_psa_params()
#' @export
generate_psa_params <- function(n_sim = 1000, seed = 20190220){ # User defined
  ## Load calibrated parameters
  n_sim <- nrow(m_calib_post)
  set_seed <- seed
  df_psa_params <- data.frame(
    ### Calibrated parameters
    m_calib_post,
    
    ### Transition probabilities (per cycle)
    p_HS1   = rbeta(n_sim, 30, 170),        # probability to become sick when healthy
    p_S1H   = rbeta(n_sim, 60, 60) ,        # probability to become healthy when sick
    
    ### State rewards
    ## Costs
    c_H   = rgamma(n_sim, shape = 100, scale = 20)    , # cost of remaining one cycle in state H
    c_S1  = rgamma(n_sim, shape = 177.8, scale = 22.5), # cost of remaining one cycle in state S1
    c_S2  = rgamma(n_sim, shape = 225, scale = 66.7)  , # cost of remaining one cycle in state S2
    c_Trt = rgamma(n_sim, shape = 73.5, scale = 163.3), # cost of treatment (per cycle)
    c_D   = 0                                         , # cost of being in the death state
    ## Utilities
    u_H   = truncnorm::rtruncnorm(n_sim, mean =    1, sd = 0.01, b = 1), # utility when healthy
    u_S1  = truncnorm::rtruncnorm(n_sim, mean = 0.75, sd = 0.02, b = 1), # utility when sick
    u_S2  = truncnorm::rtruncnorm(n_sim, mean = 0.50, sd = 0.03, b = 1), # utility when sicker
    u_D   = 0                                               , # utility when dead
    u_Trt = truncnorm::rtruncnorm(n_sim, mean = 0.95, sd = 0.02, b = 1)  # utility when being treated
  )
  return(df_psa_params)
}
