#' Calculate cost-effectiveness outcomes
#'
#' \code{calculate_ce_out} calculates costs and effects for a given vector of 
#' parameters using a decision model. This function needs to be modified by the 
#' users to fit their needs
#' @param l_params_all List with all parameters of decision model
#' @param n_wtp Willingness-to-pay threshold to compute net benefits.
#' @return 
#' A data frame with discounted costs, effectiveness and NMB for each strategy.
#' @export
calculate_ce_out <- function(l_params_all = load_all_params(), 
                             n_wtp = 100000){ # User defined
  with(as.list(l_params_all), {
    ## Create discounting vectors
    v_dwc <- 1 / ((1 + d_c) ^ (0:(n_t))) # vector with discount weights for costs
    v_dwe <- 1 / ((1 + d_e) ^ (0:(n_t))) # vector with discount weights for QALYs

    ## Run STM model at a parameter set for each intervention
    ## In the Sick-Sicker model, treatment does not alter the transition
    ## probabilities, only the state rewards, so both strategies share one run of
    ## the decision model. Give each strategy its own call to decision_model()
    ## whenever treatment changes the natural history of the disease.
    l_model_out_no_trt <- decision_model(l_params_all = l_params_all)
    l_model_out_trt    <- l_model_out_no_trt

    ## Cohort trace by treatment
    m_M_no_trt <- l_model_out_no_trt$m_M # No treatment
    m_M_trt    <- l_model_out_trt$m_M    # Treatment
    
    ## Vectors with costs and utilities by treatment
    v_u_no_trt <- c(u_H, u_S1, u_S2, u_D)
    v_u_trt    <- c(u_H, u_Trt, u_S2, u_D)
    
    v_c_no_trt <- c(c_H, c_S1, c_S2, c_D)
    v_c_trt    <- c(c_H, c_S1 + c_Trt, c_S2 + c_Trt, c_D)
    
    ## Mean Costs and QALYs for Treatment and NO Treatment
    v_tu_no_trt <- m_M_no_trt %*% v_u_no_trt
    v_tu_trt    <- m_M_trt %*% v_u_trt
    
    v_tc_no_trt <- m_M_no_trt %*% v_c_no_trt
    v_tc_trt    <- m_M_trt %*% v_c_trt
    
    ## Total discounted mean Costs and QALYs
    tu_d_no_trt <- t(v_tu_no_trt) %*% v_dwe 
    tu_d_trt    <- t(v_tu_trt) %*% v_dwe
    
    tc_d_no_trt <- t(v_tc_no_trt) %*% v_dwc
    tc_d_trt    <- t(v_tc_trt)    %*% v_dwc
    
    ## Vector with total discounted mean Costs and QALYs
    v_tc_d <- c(tc_d_no_trt, tc_d_trt)
    v_tu_d <- c(tu_d_no_trt, tu_d_trt)
    
    ## Vector with discounted net monetary benefits (NMB)
    v_nmb_d <- v_tu_d * n_wtp - v_tc_d
    
    ## Dataframe with discounted costs, effectiveness and NMB
    df_ce <- data.frame(Strategy = v_names_str,
                        Cost     = v_tc_d,
                        Effect   = v_tu_d,
                        NMB      = v_nmb_d)
    
    return(df_ce)
  }
  )
}

#' One-way sensitivity analysis (OWSA)
#'
#' This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#' @param parms Vector with strings with the name of the parameters of interest
#' @param ranges A named list of the form list("parm" = c(0, 1), ...) that gives
#' the ranges for the parameters of interest. The list is matched to
#' \code{parms} by name, so the order of its elements does not matter. The
#' number of samples from each range is determined by \code{nsamps}
#' @param nsamps Number of parameter values to evaluate for each parameter.
#' Default = 100
#' @param params_basecase List with parameters for the base case
#' @param FUN Function that takes \code{params_basecase} and \code{...} and
#' produces \code{outcome} of interest
#' @param outcome String with the name of the outcome of interest produced by
#' \code{FUN}
#' @param strategies vector of strategy names. The default (NULL) will use
#' strategy names in FUN
#' @param ... Further arguments to FUN (not used)
#' @keywords owsa
#' @return A dataframe with the results of the sensitivity analysis. Can be
#' visualized with \code{plot.owsa}, \code{owsa_opt_strat} and
#' \code{owsa_tornado} from \code{dampack}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of the columns must be outcomes.
#' @examples
#' \donttest{
#'   l_params_all <- load_all_params()
#'   owsa_det(parms  = c("c_Trt", "u_S1"),
#'            ranges = list("c_Trt" = c(6000, 13000),
#'                          "u_S1"  = c(0.75, 0.95)),
#'            nsamps = 10,
#'            params_basecase = l_params_all,
#'            FUN     = calculate_ce_out,
#'            outcome = "NMB",
#'            n_wtp   = 150000)
#' }
#' @export
owsa_det <- function(parms, ranges, nsamps = 100, params_basecase, FUN, outcome,
                     strategies = NULL, ...){
  ### Check for errors
  ranges <- check_sa_ranges(parms = parms, ranges = ranges,
                            params_basecase = params_basecase, nsamps = nsamps)

  funtest <- check_sa_fun(FUN = FUN, params_basecase = params_basecase,
                          outcome = outcome, ...)
  if(is.null(strategies)){
    strategies <- funtest[, 1]
  }
  n_str <- length(strategies)
  if(length(strategies) != length(funtest[, 1])){
    stop("Number of strategies not the same as in FUN")
  }

  df_owsa_all <- NULL
  for (i in 1:length(parms)) { # i <- 2
    ### Generate matrix of inputs
    v_owsa_input <- seq(ranges[[i]][1],
                        ranges[[i]][2],
                        length.out = nsamps)
    ### Initialize matrix to store outcomes from a OWSA of the CEA
    m_out_owsa <- matrix(0, 
                         nrow = length(v_owsa_input), 
                         ncol = n_str)
    ### Run model and capture outcome
    l_owsa_input <- params_basecase
    for (j in 1:length(v_owsa_input)){ # j <- 1
      l_owsa_input[names(l_owsa_input) == parms[i]] <- v_owsa_input[j]
      m_out_owsa[j, ] <- FUN(l_owsa_input, ...)[[outcome]]
    }
    
    df_owsa <- data.frame(parameter = parms[i],
                          v_owsa_input,
                          m_out_owsa)
    names(df_owsa)[-1] <- c("param_val", strategies)
    
    df_owsa_all <- rbind(df_owsa_all, df_owsa)
  }
  
  df_owsa_lng <- reshape2::melt(df_owsa_all, 
                                id.vars = c("parameter", "param_val"), 
                                variable.name = "strategy", 
                                value.name = "outcome_val")
  
  class(df_owsa_lng) <- c("owsa", "data.frame")
  
  return(df_owsa_lng)
}

#---------------------------------------------------------------#
#### Function to compute two-way sensitivity analysis (TWSA) ####
#---------------------------------------------------------------#
#' Two-way sensitivity analysis (TWSA)
#'
#' This function runs a deterministic two-way sensitivity analysis (TWSA) on a
#' given function that produces outcomes.
#' @param parm1 String with the name of the first parameter of interest
#' @param parm2 String with the name of the second parameter of interest
#' @param ranges A named list of the form list("parm1" = c(0, 1), ...) that gives
#' the ranges for the parameters of interest. The list is matched to
#' \code{parm1} and \code{parm2} by name, so the order of its elements does not
#' matter. The number of samples from each range is determined by \code{nsamps}
#' @param nsamps Number of parameter values to evaluate for each parameter, so
#' that \code{FUN} is evaluated \code{nsamps^2} times. Default = 40
#' @param params_basecase List with parameters for the base case
#' @param FUN Function that takes \code{params_basecase} and \code{...} and
#' produces \code{outcome} of interest
#' @param outcome String with the name of the outcome of interest produced by
#' \code{FUN}
#' @param strategies vector of strategy names. The default (NULL) will use
#' strategy names in FUN
#' @param progress Logical variable to display the simulation progress.
#' Default = TRUE
#' @param ... Further arguments to FUN (not used)
#' @keywords twsa
#' @return
#' A dataframe with the results of the sensitivity analysis. Can be
#' visualized with \code{plot.twsa} from \code{dampack}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of the columns must be outcomes.
#' @examples
#' \donttest{
#'   l_params_all <- load_all_params()
#'   twsa_det(parm1  = "u_S1",
#'            parm2  = "u_Trt",
#'            ranges = list("u_S1"  = c(0.70, 0.80),
#'                          "u_Trt" = c(0.90, 1.00)),
#'            nsamps = 5,
#'            params_basecase = l_params_all,
#'            FUN     = calculate_ce_out,
#'            outcome = "NMB",
#'            n_wtp   = 150000)
#' }
#' @export
twsa_det <- function(parm1, parm2, ranges, nsamps = 40, params_basecase, FUN, outcome,
                     strategies = NULL, progress = TRUE, ...){
  ### Check for errors
  parms  <- c(parm1, parm2)
  ranges <- check_sa_ranges(parms = parms, ranges = ranges,
                            params_basecase = params_basecase, nsamps = nsamps)

  funtest <- check_sa_fun(FUN = FUN, params_basecase = params_basecase,
                          outcome = outcome, ...)
  if(is.null(strategies)){
    strategies <- funtest[, 1]
  }
  n_str <- length(strategies)
  if(length(strategies) != length(funtest[, 1])){
    stop("Number of strategies not the same as in FUN")
  }

  ### Generate matrix of inputs
  df_twsa_params <- expand.grid(placeholder_name1 = seq(ranges[[1]][1],
                                                        ranges[[1]][2],
                                                        length.out = nsamps),
                                placeholder_name2 = seq(ranges[[2]][1],
                                                        ranges[[2]][2],
                                                        length.out = nsamps))
  names(df_twsa_params) <- c(parm1, parm2)
  n_rows <- nrow(df_twsa_params)

  ### Initialize matrix to store outcomes from a TWSA of the CEA
  m_out_twsa <- matrix(0,
                       nrow = n_rows,
                       ncol = n_str)

  ### Run model and capture outcome
  l_twsa_input <- params_basecase
  ## Cycles at which to report progress, computed once so that the report does
  ## not depend on an exact equality between floating point numbers
  v_cycles_progress <- unique(round(seq(n_rows / 10, n_rows, length.out = 10)))
  for (i in 1:n_rows){ # i <- 1
    l_twsa_input[names(l_twsa_input) == parm1] <- df_twsa_params[i, 1]
    l_twsa_input[names(l_twsa_input) == parm2] <- df_twsa_params[i, 2]
    m_out_twsa[i, ] <- FUN(l_twsa_input, ...)[[outcome]]

    ## Display simulation progress
    if(progress && i %in% v_cycles_progress) {
      cat('\r', paste(round(i / n_rows * 100), "% done", sep = " "))
    }
  }
  if(progress) cat('\n')
  
  df_twsa <- data.frame(df_twsa_params,
                        m_out_twsa)
  names(df_twsa)[-c(1:2)] <- strategies
  
  
  df_twsa_lng <- reshape2::melt(df_twsa, id.vars = c(parm1, parm2), 
                                variable.name = "strategy", 
                                value.name = "outcome_val")
  
  class(df_twsa_lng) <- c("twsa", "data.frame")

  return(df_twsa_lng)
}

#-----------------------------------------------------------------------------#
#### Internal helpers                                                      ####
#-----------------------------------------------------------------------------#

#' Check the parameters and ranges of a deterministic sensitivity analysis
#'
#' Internal helper shared by \code{owsa_det} and \code{twsa_det}. It verifies
#' that every parameter of interest exists in the base-case parameter list and
#' has a valid range, and returns the list of ranges reordered to match
#' \code{parms} so that callers can index it positionally.
#'
#' @param parms Vector with strings with the name of the parameters of interest.
#' @param ranges A named list with the range of each parameter in \code{parms}.
#' @param params_basecase List with parameters for the base case.
#' @param nsamps Number of parameter values to evaluate for each parameter.
#' @return The \code{ranges} list, reordered to follow \code{parms}.
#' @noRd
check_sa_ranges <- function(parms, ranges, params_basecase, nsamps) {
  v_parms_notfound <- setdiff(parms, names(params_basecase))
  if (length(v_parms_notfound) > 0) {
    stop("The following parameter(s) are not in 'params_basecase': ",
         paste(v_parms_notfound, collapse = ", "))
  }

  if (!is.list(ranges)) {
    stop("'ranges' should be a list of the form ",
         "list(\"", parms[1], "\" = c(lower, upper), ...)")
  }

  if (length(parms) != length(ranges)) {
    stop("The number of parameters (", length(parms), ") is not the same as ",
         "the number of ranges (", length(ranges), ")")
  }

  # Matching by name means a 'ranges' list given in a different order than
  # 'parms' varies each parameter over its own range rather than over another
  # parameter's range
  v_ranges_notfound <- setdiff(parms, names(ranges))
  if (length(v_ranges_notfound) > 0) {
    stop("'ranges' must be a named list with one element per parameter. ",
         "No range was given for: ",
         paste(v_ranges_notfound, collapse = ", "))
  }
  ranges <- ranges[parms]

  v_ranges_notvalid <- names(ranges)[!vapply(ranges,
                                             function(x) is.numeric(x) &&
                                               length(x) == 2 &&
                                               !anyNA(x) && x[1] <= x[2],
                                             logical(1))]
  if (length(v_ranges_notvalid) > 0) {
    stop("Each element of 'ranges' must be a numeric vector of length two, ",
         "c(lower, upper), with lower <= upper. Not valid: ",
         paste(v_ranges_notvalid, collapse = ", "))
  }

  if (!is.numeric(nsamps) || length(nsamps) != 1 || is.na(nsamps)) {
    stop("'nsamps' must be a single number")
  }
  if (nsamps < 2) {
    stop("'nsamps' must be at least 2, got ", nsamps)
  }

  return(ranges)
}

#' Check the outcome function of a deterministic sensitivity analysis
#'
#' Internal helper shared by \code{owsa_det} and \code{twsa_det}. It evaluates
#' \code{FUN} once at the base case, re-raising any error with a message that
#' points at the cause, and checks that the requested outcome is one of the
#' columns \code{FUN} returns.
#'
#' @param FUN Function that takes \code{params_basecase} and \code{...} and
#' produces \code{outcome} of interest.
#' @param params_basecase List with parameters for the base case.
#' @param outcome String with the name of the outcome of interest.
#' @param ... Further arguments to FUN.
#' @return The data frame returned by \code{FUN} at the base case.
#' @noRd
check_sa_fun <- function(FUN, params_basecase, outcome, ...) {
  # The previous version wrapped this call in tryCatch() but tested the result
  # with is.na(sum(is.na(jj))), which is FALSE both when FUN works and when it
  # fails, so the check never fired and the error surfaced further down with no
  # indication of its cause
  funtest <- tryCatch(FUN(params_basecase, ...),
                      error = function(e) {
                        stop("FUN is not well defined by 'params_basecase' ",
                             "and ...: ", conditionMessage(e), call. = FALSE)
                      })

  # '||' short-circuits, so ncol() is only reached for an actual data frame
  if (!is.data.frame(funtest) || ncol(funtest) < 2) {
    stop("FUN must return a data frame whose first column holds the strategy ",
         "names and whose remaining columns hold the outcomes")
  }

  v_outcomes <- colnames(funtest)[-1]
  if (length(outcome) != 1) {
    stop("'outcome' must be a single outcome name")
  }
  if (!(outcome %in% v_outcomes)) {
    stop("outcome '", outcome, "' is not part of FUN outcomes: ",
         paste(v_outcomes, collapse = ", "))
  }

  return(funtest)
}