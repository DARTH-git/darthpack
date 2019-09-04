#' Generate model outputs for calibration from a parameter set
#'
#' \code{calibration_out} computes model outputs to be used for calibration 
#' routines.
#'
#' @param v_params_calib Vector of parameters that need to be calibrated.
#' @param l_params_all List with all parameters of the decision model.
#' @return 
#' A list with Survival (Surv), Prevalence of Sick and Sicker (Prev), and 
#' proportion of Sicker (PropSicker) out of all sick (Sick+Sicker) individuals.
#' @export
calibration_out <- function(v_params_calib, l_params_all){ # User defined
  # Substitute values of calibrated parameters in base-case with 
  # calibrated values
  l_params_all <- update_param_list(l_params_all = l_params_all, params_updated = v_params_calib)
  
  # Run model with updated calibrated parameters
  l_out_stm <- decision_model(l_params_all = l_params_all)
  
  ####### Epidemiological Output ###########################################
  #### Overall Survival (OS) ####
  v_os <- 1 - l_out_stm$m_M[, "D"]
  
  #### Disease prevalence #####
  v_prev <- rowSums(l_out_stm$m_M[, c("S1", "S2")])/v_os
  
  #### Proportion of sick in S1 state #####
  v_prop_S2 <- l_out_stm$m_M[, "S2"] / rowSums(l_out_stm$m_M[, c("S1", "S2")])
  
  ####### Return Output ###########################################
  l_out <- list(Surv = v_os[c(11, 21, 31)],
                Prev = v_prev[c(11, 21, 31)],
                PropSicker = v_prop_S2[c(11, 21, 31)])
  return(l_out)
}

#' Sample from prior distributions of calibrated parameters
#'
#' \code{sample.prior} generates a sample of parameter sets from their prior 
#' distribution.
#' @param n_samp Number of samples.
#' @param v_param_names Vector with parameter names.
#' @param v_ub Vector with lower bounds for each parameter.
#' @param v_lb Vector with upper bounds for each parameter.
#' @return 
#' A matrix with 3 rows and \code{n_samp} rows. Each row corresponds to a 
#' parameter set sampled from their prior distributions
#' @examples 
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' sample.prior(2)
#' @export
sample.prior <- function(n_samp,
                         v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                         v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                         v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)){
  n_param <- length(v_param_names)
  m_lhs_unit   <- lhs::randomLHS(n = n_samp, k = n_param)
  m_param_samp <- matrix(nrow = n_samp, ncol = n_param)
  colnames(m_param_samp) <- v_param_names
  for (i in 1:n_param){
    m_param_samp[, i] <- qunif(m_lhs_unit[,i],
                               min = v_lb[i],
                               max = v_ub[i])
    # ALTERNATIVE prior using beta (or other) distributions
    # m_param_samp[, i] <- qbeta(m_lhs_unit[,i],
    #                            min = 1,
    #                            max = 1)
  }
  return(m_param_samp)
}

#' Evaluate log-prior of calibrated parameters
#'
#' \code{log_prior} computes a log-prior value for one (or multiple) parameter 
#' set(s) based on their prior distributions.
#' @param v_params Vector (or matrix) of model parameters.
#' @param v_param_names Vector with parameter names.
#' @param v_ub Vector with lower bounds for each parameter.
#' @param v_lb Vector with upper bounds for each parameter.
#' @return 
#' A scalar (or vector) with log-prior values.
#' @examples 
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' log_prior(v_params = sample.prior(n_samp = 5))
#' @export
log_prior <- function(v_params, 
                      v_param_names = c("p_S1S2", "hr_S1", "hr_S2"),
                      v_lb = c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5),
                      v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)){
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  n_param <- length(v_param_names)
  n_samp <- nrow(v_params)
  colnames(v_params) <- v_param_names
  lprior <- rep(0, n_samp)
  for (i in 1:n_param){
    lprior <- lprior + dunif(v_params[, i],
                             min = v_lb[i],
                             max = v_ub[i], 
                             log = T)
    # ALTERNATIVE prior using beta distributions
    # lprior <- lprior + dbeta(v_params[, i],
    #                          min = 1,
    #                          max = 1, 
    #                          log = T)
  }
  return(lprior)
}

#' Evaluate prior of calibrated parameters
#'
#' \code{prior} computes a prior value for one (or multiple) parameter set(s).
#' @param v_params Vector (or matrix) of model parameters 
#' @return 
#' A scalar (or vector) with prior values.
#' @examples
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' prior(v_params = sample.prior(n_samp = 5))
#' @export
prior <- function(v_params) { 
  v_prior <- exp(log_prior(v_params)) 
  return(v_prior)
}

#' Log-likelihood function for a parameter set
#'
#' \code{log_lik} computes a log-likelihood value for one (or multiple) 
#' parameter set(s).
#'
#' @param v_params Vector (or matrix) of model parameters.
#' @param l_params_all List with all parameters of the decision model. 
#' @return 
#' A scalar (or vector) with log-likelihood values.
#' @importFrom stats dnorm dunif quantile qunif rbeta rgamma sd
#' @examples 
#' \dontrun{
#'   v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#'   n_param        <- length(v_param_names)
#'   v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#'   v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#'   v_target_names <- c("Surv", "Prev", "PropSick")
#'   n_target       <- length(v_target_names)
#'   log_lik(v_params = sample.prior(n_samp = 2))
#' }
#' @export
log_lik <- function(v_params,
                    l_params_all = load_all_params()){ # User defined
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  
  n_samp <- nrow(v_params)
  v_target_names <- c("Surv", "Prev", "PropSick")
  n_target       <- length(v_target_names)
  v_llik <- matrix(0, nrow = n_samp, ncol = n_target) 
  colnames(v_llik) <- v_target_names
  v_llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j=1
    jj <- tryCatch( { 
      ###   Run model for parametr set "v_params" ###
      l_model_res <- calibration_out(v_params_calib = v_params[j, ], 
                                     l_params_all = l_params_all)
      
      ###  Calculate log-likelihood of model outputs to targets  ###
      ## TARGET 1: Survival ("Surv")
      ## Normal log-likelihood  
      v_llik[j, "Surv"] <- sum(dnorm(x = SickSicker_targets$Surv$value,
                                     mean = l_model_res$Surv,
                                     sd = SickSicker_targets$Surv$se,
                                     log = T))
      
      ## TARGET 2: Prevalence ("Prev")
      ## Normal log-likelihood
      v_llik[j, "Prev"] <- sum(dnorm(x = SickSicker_targets$Prev$value,
                                     mean = l_model_res$Prev,
                                     sd = SickSicker_targets$Prev$se,
                                     log = T))
      
      ## TARGET 3: Proportion Sick+Sicker who are Sick ("PropSick")
      ## Normal log-likelihood
      v_llik[j, "PropSick"] <- sum(dnorm(x = SickSicker_targets$PropSick$value,
                                         mean = l_model_res$PropSick,
                                         sd = SickSicker_targets$PropSick$se,
                                         log = T))
      
      ## OVERALL
      ## can give different targets different weights (user must change this)
      v_weights <- rep(1, n_target)
      ## weighted sum
      v_llik_overall[j] <- v_llik[j, ] %*% v_weights
    }, error = function(e) NA) 
    if(is.na(jj)) { v_llik_overall <- -Inf }
  } ## End loop over sampled parameter sets
  
  ## return GOF
  return(v_llik_overall)
}

#' Likelihood
#'
#' \code{likelihood} computes a likelihood value for one (or multiple) 
#' parameter set(s).
#'
#' @param v_params Vector (or matrix) of model parameters. 
#' @return 
#' A scalar (or vector) with likelihood values.
#' @examples
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' v_target_names <- c("Surv", "Prev", "PropSick")
#' n_target       <- length(v_target_names)
#' likelihood(v_params = sample.prior(n_samp = 2))
#' @export
likelihood <- function(v_params){ 
  v_like <- exp(log_lik(v_params)) 
  return(v_like)
}

#' Evaluate log-posterior of calibrated parameters
#'
#' \code{log_post} Computes a log-posterior value for one (or multiple) 
#' parameter set(s) based on the simulation model, likelihood functions and 
#' prior distributions.
#' @param v_params Vector (or matrix) of model parameters 
#' @return 
#' A scalar (or vector) with log-posterior values.
#' @examples 
#' v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#' n_param        <- length(v_param_names)
#' v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#' v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#' v_target_names <- c("Surv", "Prev", "PropSick")
#' n_target       <- length(v_target_names)
#' log_post(v_params = sample.prior(n_samp = 5))
#' @export
log_post <- function(v_params) { 
  v_lpost <- log_prior(v_params) + log_lik(v_params)
  return(v_lpost) 
}

#' Evaluate posterior of calibrated parameters
#'
#' \code{posterior} computes a posterior value for one (or multiple) parameter 
#' set(s).
#' @param v_params Vector (or matrix) of model parameters 
#' @return 
#' A scalar (or vector) with posterior values.
#' @examples
#' \dontrun{
#'  v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
#'  n_param        <- length(v_param_names)
#'  v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)  # lower bound
#'  v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15) # upper bound
#'  v_target_names <- c("Surv", "Prev", "PropSick")
#'  n_target       <- length(v_target_names)
#'  posterior(v_params = sample.prior(n_samp = 5))
#' }
#' @export
posterior <- function(v_params) { 
  v_posterior <- exp(log_post(v_params)) 
  return(v_posterior)
}