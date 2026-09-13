#' Generate PSA dataset of CEA parameters
#'
#' \code{generate_psa_params} generates PSA input dataset by sampling decision
#' model parameters from their distributions. The sample of the calibrated
#' parameters is a draw from their posterior distribution obtained with the
#' IMIS algorithm.
#' @param n_sim Number of PSA samples. Default = 1000, the size of the
#' posterior sample of the calibrated parameters stored in
#' \code{\link{m_calib_post}}.
#' @param seed Seed for reproducibility of Monte Carlo sampling. Use
#' \code{NULL} to sample from the current state of the random number
#' generator, for instance when the seed is set by the calling script.
#' @return
#' A data frame with \code{n_sim} rows and 15 columns of parameters for PSA.
#' Each row is a parameter set sampled from distributions that characterize
#' their uncertainty
#' @examples
#' df_psa_input <- generate_psa_params(n_sim = 10)
#' dim(df_psa_input)
#' @export
generate_psa_params <- function(n_sim = 1000, seed = 20190220){ # User defined
  #### Error checking ####
  if (!is.numeric(n_sim) || length(n_sim) != 1 || is.na(n_sim) || n_sim < 1) {
    stop("'n_sim' must be a single number of at least 1")
  }
  n_sim <- as.integer(n_sim)

  ## Set the seed so that the PSA dataset is reproducible. The seed used to be
  ## assigned to an unused variable, which left the sample dependent on the
  ## state of the random number generator when the function was called.
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ## Load calibrated parameters
  ## The calibrated parameters are not drawn from a parametric distribution:
  ## they are taken from the posterior sample produced by the IMIS calibration,
  ## which preserves the correlation between them.
  n_post <- nrow(m_calib_post)
  if (n_sim == n_post) {
    ## Use every posterior draw exactly once
    m_calib_post_samp <- m_calib_post
  } else {
    if (n_sim > n_post) {
      warning("'n_sim' (", n_sim, ") is larger than the posterior sample of ",
              "the calibrated parameters (", n_post, "), so posterior draws ",
              "are resampled with replacement. Rerun the calibration with a ",
              "larger 'B.re' to obtain n_sim distinct draws.", call. = FALSE)
    }
    v_rows_post <- sample.int(n = n_post, size = n_sim,
                              replace = n_sim > n_post)
    m_calib_post_samp <- m_calib_post[v_rows_post, , drop = FALSE]
  }
  rownames(m_calib_post_samp) <- NULL

  df_psa_params <- data.frame(
    ### Calibrated parameters
    m_calib_post_samp,

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
