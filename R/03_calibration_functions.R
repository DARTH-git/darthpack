#' Generate model outputs for calibration from a parameter set
#'
#' \code{calibration_out} computes model outputs to be used for calibration
#' routines.
#'
#' @param v_params_calib Vector of parameters that need to be calibrated.
#' @param l_params_all List with all parameters of the decision model.
#' @param v_target_times Numeric vector with the model cycles (i.e., the times
#' of the calibration targets) at which model outputs should be returned.
#' Default = \code{c(10, 20, 30)}, matching the times of
#' \code{\link{SickSicker_targets}}.
#' @return
#' A list with Survival (Surv), Prevalence of Sick and Sicker (Prev), and
#' proportion of Sicker (PropSicker) out of all sick (Sick+Sicker) individuals,
#' each evaluated at \code{v_target_times}.
#' @examples
#' l_params_all <- load_all_params()
#' calibration_out(v_params_calib = c(p_S1S2 = 0.105, hr_S1 = 3, hr_S2 = 10),
#'                 l_params_all   = l_params_all)
#' @export
calibration_out <- function(v_params_calib,
                            l_params_all,
                            v_target_times = c(10, 20, 30)){ # User defined
  # Substitute values of calibrated parameters in base-case with
  # calibrated values
  l_params_all <- update_param_list(l_params_all = l_params_all, params_updated = v_params_calib)

  #### Error checking ####
  # The target times must be cycles the model actually simulates, otherwise the
  # outputs below would be silently padded with NA
  v_times_notvalid <- setdiff(v_target_times, 0:l_params_all$n_t)
  if (length(v_times_notvalid) > 0) {
    stop("The calibration target times must be whole model cycles between 0 ",
         "and the model time horizon (n_t = ", l_params_all$n_t, "). ",
         "Not valid: ", paste(v_times_notvalid, collapse = ", "))
  }

  # Run model with updated calibrated parameters
  l_out_stm <- decision_model(l_params_all = l_params_all)

  # Rows of the cohort trace are named after the model cycle (0, 1, ..., n_t),
  # so target times are matched by name rather than by hard-coded position
  v_rows_targets <- as.character(v_target_times)

  ####### Epidemiological Output ###########################################
  #### Overall Survival (OS) ####
  v_os <- 1 - l_out_stm$m_M[, "D"]

  #### Number of sick individuals (Sick + Sicker) ####
  v_sick <- rowSums(l_out_stm$m_M[, c("S1", "S2")])

  #### Disease prevalence #####
  v_prev <- v_sick / v_os

  #### Proportion of sick individuals who are in the S2 (Sicker) state #####
  v_prop_S2 <- l_out_stm$m_M[, "S2"] / v_sick

  ####### Return Output ###########################################
  l_out <- list(Surv       = v_os[v_rows_targets],
                Prev       = v_prev[v_rows_targets],
                PropSicker = v_prop_S2[v_rows_targets])
  return(l_out)
}

#' Sample from prior distributions of calibrated parameters
#'
#' \code{sample_prior} generates a sample of parameter sets from their prior
#' distribution.
#' @param n_samp Number of samples.
#' @param v_param_names Vector with parameter names.
#' @param v_lb Vector with lower bounds for each parameter.
#' @param v_ub Vector with upper bounds for each parameter.
#' @return
#' A matrix with \code{n_samp} rows and one column per calibrated parameter.
#' Each row corresponds to a parameter set sampled from their prior
#' distributions.
#' @examples
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' sample_prior(2)
#' @export
sample_prior <- function(n_samp,
                         v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                         v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                         v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)){
  n_param <- length(v_param_names)
  check_prior_bounds(v_param_names = v_param_names, v_lb = v_lb, v_ub = v_ub)

  m_lhs_unit   <- lhs::randomLHS(n = n_samp, k = n_param)
  m_param_samp <- matrix(nrow = n_samp, ncol = n_param)
  colnames(m_param_samp) <- v_param_names
  for (i in 1:n_param){
    m_param_samp[, i] <- qunif(m_lhs_unit[, i],
                               min = v_lb[i],
                               max = v_ub[i])
    # ALTERNATIVE prior using beta (or other) distributions
    # m_param_samp[, i] <- qbeta(m_lhs_unit[, i],
    #                            shape1 = 1,
    #                            shape2 = 1)
  }
  return(m_param_samp)
}

#' Sample from prior distributions of calibrated parameters
#'
#' \code{sample.prior} is kept for backwards compatibility and because the
#' \code{IMIS} package calls the sampling function by this exact name. It is a
#' thin wrapper around \code{\link{sample_prior}}, which follows the DARTH
#' naming conventions and should be preferred in new code.
#' @inheritParams sample_prior
#' @inherit sample_prior return
#' @seealso \code{\link{sample_prior}}
#' @examples
#' sample.prior(2)
#' @export
sample.prior <- function(n_samp,
                         v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                         v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                         v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)){
  sample_prior(n_samp        = n_samp,
               v_param_names = v_param_names,
               v_lb          = v_lb,
               v_ub          = v_ub)
}

#' Evaluate log-prior of calibrated parameters
#'
#' \code{log_prior} computes a log-prior value for one (or multiple) parameter
#' set(s) based on their prior distributions.
#' @param v_params Vector (or matrix) of model parameters.
#' @param v_param_names Vector with parameter names.
#' @param v_lb Vector with lower bounds for each parameter.
#' @param v_ub Vector with upper bounds for each parameter.
#' @return
#' A scalar (or vector) with log-prior values.
#' @examples
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' log_prior(v_params = sample_prior(n_samp = 5))
#' @export
log_prior <- function(v_params,
                      v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                      v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                      v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)){
  v_params <- as_param_matrix(v_params, v_param_names)
  n_param  <- length(v_param_names)
  n_samp   <- nrow(v_params)
  check_prior_bounds(v_param_names = v_param_names, v_lb = v_lb, v_ub = v_ub)

  lprior <- rep(0, n_samp)
  for (i in 1:n_param){
    lprior <- lprior + dunif(v_params[, i],
                             min = v_lb[i],
                             max = v_ub[i],
                             log = TRUE)
    # ALTERNATIVE prior using beta distributions
    # lprior <- lprior + dbeta(v_params[, i],
    #                          shape1 = 1,
    #                          shape2 = 1,
    #                          log = TRUE)
  }
  return(lprior)
}

#' Evaluate prior of calibrated parameters
#'
#' \code{prior} computes a prior value for one (or multiple) parameter set(s).
#' @inheritParams log_prior
#' @return
#' A scalar (or vector) with prior values.
#' @examples
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' prior(v_params = sample_prior(n_samp = 5))
#' @export
prior <- function(v_params,
                  v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                  v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                  v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)) {
  v_prior <- exp(log_prior(v_params      = v_params,
                           v_param_names = v_param_names,
                           v_lb          = v_lb,
                           v_ub          = v_ub))
  return(v_prior)
}

#' Log-likelihood function for a parameter set
#'
#' \code{log_lik} computes a log-likelihood value for one (or multiple)
#' parameter set(s).
#'
#' @param v_params Vector (or matrix) of model parameters.
#' @param l_params_all List with all parameters of the decision model.
#' @param v_weights Numeric vector with one weight per calibration target, used
#' to compute the overall log-likelihood as a weighted sum of the
#' target-specific log-likelihoods. If \code{NULL} (default), all targets get
#' an equal weight of one.
#' @return
#' A scalar (or vector) with log-likelihood values. Parameter sets for which the
#' decision model cannot be evaluated return \code{-Inf}.
#' @importFrom stats dnorm dunif quantile qunif rbeta rgamma sd
#' @examples
#' \donttest{
#'   v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#'   n_param        <- length(v_param_names)
#'   v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#'   v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#'   v_target_names <- c("Surv", "Prev", "PropSicker")
#'   n_target       <- length(v_target_names)
#'   log_lik(v_params = sample_prior(n_samp = 2))
#' }
#' @export
log_lik <- function(v_params,
                    l_params_all = load_all_params(),
                    v_weights = NULL){ # User defined
  v_params <- as_param_matrix(v_params)

  n_samp <- nrow(v_params)
  v_target_names <- c("Surv", "Prev", "PropSicker")
  n_target       <- length(v_target_names)

  ## can give different targets different weights (user must change this)
  if (is.null(v_weights)) {
    v_weights <- rep(1, n_target)
  }
  if (length(v_weights) != n_target) {
    stop("'v_weights' must have one weight per calibration target (",
         n_target, "): ", paste(v_target_names, collapse = ", "))
  }

  m_llik <- matrix(0, nrow = n_samp, ncol = n_target,
                   dimnames = list(NULL, v_target_names))
  v_llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j <- 1
    ## A parameter set the model cannot be evaluated at gets -Inf, so that it is
    ## given zero posterior weight without affecting the other parameter sets
    v_llik_overall[j] <- tryCatch({
      ###   Run model for parameter set "v_params" ###
      l_model_res <- calibration_out(v_params_calib = v_params[j, ],
                                     l_params_all = l_params_all)

      ###  Calculate log-likelihood of model outputs to targets  ###
      ## TARGET 1: Survival ("Surv")
      ## Normal log-likelihood
      m_llik[j, "Surv"] <- sum(dnorm(x    = SickSicker_targets$Surv$value,
                                     mean = l_model_res$Surv,
                                     sd   = SickSicker_targets$Surv$se,
                                     log  = TRUE))

      ## TARGET 2: Prevalence ("Prev")
      ## Normal log-likelihood
      m_llik[j, "Prev"] <- sum(dnorm(x    = SickSicker_targets$Prev$value,
                                     mean = l_model_res$Prev,
                                     sd   = SickSicker_targets$Prev$se,
                                     log  = TRUE))

      ## TARGET 3: Proportion of Sick+Sicker who are Sicker ("PropSicker")
      ## Normal log-likelihood
      m_llik[j, "PropSicker"] <- sum(dnorm(x    = SickSicker_targets$PropSicker$value,
                                           mean = l_model_res$PropSicker,
                                           sd   = SickSicker_targets$PropSicker$se,
                                           log  = TRUE))

      ## OVERALL
      ## weighted sum of the target-specific log-likelihoods
      as.numeric(m_llik[j, ] %*% v_weights)
    }, error = function(e) -Inf)
  } ## End loop over sampled parameter sets

  ## A non-finite log-likelihood (e.g., NaN from an infeasible model run) is
  ## treated the same way as a model that failed to run
  v_llik_overall[is.na(v_llik_overall)] <- -Inf

  ## return GOF
  return(v_llik_overall)
}

#' Likelihood
#'
#' \code{likelihood} computes a likelihood value for one (or multiple)
#' parameter set(s).
#'
#' @inheritParams log_lik
#' @return
#' A scalar (or vector) with likelihood values.
#' @examples
#' \donttest{
#'   v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#'   n_param        <- length(v_param_names)
#'   v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#'   v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#'   v_target_names <- c("Surv", "Prev", "PropSicker")
#'   n_target       <- length(v_target_names)
#'   likelihood(v_params = sample_prior(n_samp = 2))
#' }
#' @export
likelihood <- function(v_params,
                       l_params_all = load_all_params(),
                       v_weights = NULL){
  v_like <- exp(log_lik(v_params     = v_params,
                        l_params_all = l_params_all,
                        v_weights    = v_weights))
  return(v_like)
}

#' Evaluate log-posterior of calibrated parameters
#'
#' \code{log_post} Computes a log-posterior value for one (or multiple)
#' parameter set(s) based on the simulation model, likelihood functions and
#' prior distributions.
#' @inheritParams log_prior
#' @inheritParams log_lik
#' @return
#' A scalar (or vector) with log-posterior values.
#' @examples
#' \donttest{
#'   v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#'   n_param        <- length(v_param_names)
#'   v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#'   v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#'   v_target_names <- c("Surv", "Prev", "PropSicker")
#'   n_target       <- length(v_target_names)
#'   log_post(v_params = sample_prior(n_samp = 5))
#' }
#' @export
log_post <- function(v_params,
                     l_params_all = load_all_params(),
                     v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                     v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                     v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15),
                     v_weights = NULL) {
  v_params <- as_param_matrix(v_params, v_param_names)

  v_lprior <- log_prior(v_params      = v_params,
                        v_param_names = v_param_names,
                        v_lb          = v_lb,
                        v_ub          = v_ub)

  ## Parameter sets outside the support of the prior have a log-posterior of
  ## -Inf whatever the likelihood is, so the decision model is only run for the
  ## parameter sets the prior gives non-zero density to. This avoids evaluating
  ## the model at infeasible parameter values.
  v_lpost <- rep(-Inf, length(v_lprior))
  v_in_support <- is.finite(v_lprior)
  if (any(v_in_support)) {
    v_lpost[v_in_support] <- v_lprior[v_in_support] +
      log_lik(v_params     = v_params[v_in_support, , drop = FALSE],
              l_params_all = l_params_all,
              v_weights    = v_weights)
  }
  return(v_lpost)
}

#' Evaluate posterior of calibrated parameters
#'
#' \code{posterior} computes a posterior value for one (or multiple) parameter
#' set(s).
#' @inheritParams log_post
#' @return
#' A scalar (or vector) with posterior values.
#' @examples
#' \donttest{
#'   v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#'   n_param        <- length(v_param_names)
#'   v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#'   v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#'   v_target_names <- c("Surv", "Prev", "PropSicker")
#'   n_target       <- length(v_target_names)
#'   posterior(v_params = sample_prior(n_samp = 5))
#' }
#' @export
posterior <- function(v_params,
                      l_params_all = load_all_params(),
                      v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                      v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                      v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15),
                      v_weights = NULL) {
  v_posterior <- exp(log_post(v_params      = v_params,
                              l_params_all  = l_params_all,
                              v_param_names = v_param_names,
                              v_lb          = v_lb,
                              v_ub          = v_ub,
                              v_weights     = v_weights))
  return(v_posterior)
}

#-----------------------------------------------------------------------------#
#### Internal helpers                                                      ####
#-----------------------------------------------------------------------------#

#' Coerce a parameter set to a matrix of parameter sets
#'
#' Internal helper that lets the calibration functions accept either a single
#' parameter set (a vector) or many parameter sets (a matrix), and that fails
#' with an informative message when the number of columns does not match the
#' number of calibrated parameters.
#'
#' @param v_params Vector (or matrix) of model parameters.
#' @param v_param_names Vector with parameter names, or \code{NULL} to leave
#' existing column names untouched.
#' @return A matrix of parameter sets, one row per set.
#' @noRd
as_param_matrix <- function(v_params, v_param_names = NULL) {
  if (is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params)
  }
  v_params <- as.matrix(v_params)

  if (!is.null(v_param_names)) {
    if (ncol(v_params) != length(v_param_names)) {
      stop("'v_params' has ", ncol(v_params), " column(s) but ",
           length(v_param_names), " calibrated parameter(s) were expected: ",
           paste(v_param_names, collapse = ", "))
    }
    colnames(v_params) <- v_param_names
  }
  return(v_params)
}

#' Check the bounds of the prior distributions
#'
#' Internal helper that verifies that the vectors of lower and upper bounds are
#' consistent with the vector of calibrated parameter names.
#'
#' @param v_param_names Vector with parameter names.
#' @param v_lb Vector with lower bounds for each parameter.
#' @param v_ub Vector with upper bounds for each parameter.
#' @return Invisibly \code{TRUE}; called for its side effect of raising an error.
#' @noRd
check_prior_bounds <- function(v_param_names, v_lb, v_ub) {
  n_param <- length(v_param_names)
  if (length(v_lb) != n_param | length(v_ub) != n_param) {
    stop("'v_lb' and 'v_ub' must both have one element per calibrated ",
         "parameter (", n_param, "): ", paste(v_param_names, collapse = ", "))
  }
  if (any(v_lb >= v_ub)) {
    stop("Each lower bound in 'v_lb' must be smaller than the corresponding ",
         "upper bound in 'v_ub'. Check: ",
         paste(v_param_names[v_lb >= v_ub], collapse = ", "))
  }
  invisible(TRUE)
}
