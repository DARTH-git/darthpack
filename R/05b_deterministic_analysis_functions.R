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
    v_dwc <- 1 / ((1 + d_e) ^ (0:(n_t))) # vector with discount weights for costs
    v_dwe <- 1 / ((1 + d_c) ^ (0:(n_t))) # vector with discount weights for QALYs
    
    ## Run STM model at a parameter set for each intervention
    l_model_out_no_trt <- decision_model(l_params_all = l_params_all)
    l_model_out_trt    <- decision_model(l_params_all = l_params_all)
    
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
#' @param ranges A named list of the form c("parm" = c(0, 1), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of parameter values. If NULL, 100 parameter values are 
#' used
#' @param params_basecase List with parameters for the base case
#' @param FUN Function that takes \code{params_basecase} and \code{...} and 
#' produces \code{outcome} of interest
#' @param outcome String with the outcome of interest produced by \code{nsamp}
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
#' @export
owsa_det <- function(parms, ranges, nsamps = 100, params_basecase, FUN, outcome, 
                     strategies = NULL, ...){
  ### Check for errors
  if(sum(parms %in% names(params_basecase)) != length(parms)){
    stop("parms should be in names of params_basecase")
  }
  
  if(typeof(ranges)!="list"){
    stop("ranges should be a list")
  }
  
  if(length(parms) != length(ranges)){
    stop("The number of parameters is not the same as the number of ranges")
  }
  
  if(sum(parms==names(ranges)) != length(parms)){
    stop("The name of parameters in parms does not match the name in ranges")
  }
  
  jj <- tryCatch({
    funtest <- FUN(params_basecase, ...)  
  }, error = function(e) NA)
  if(is.na(sum(is.na(jj)))){
    stop("FUN is not well defined by 'params_basecase' and ...")
  }
  funtest <- FUN(params_basecase, ...)
  if(is.null(strategies)){
    strategies <- funtest[, 1]
    n_str <- length(strategies) 
  }
  else{
    n_str <- length(strategies)
  }
  if(length(strategies)!=length(funtest[, 1])){
    stop("Number of strategies not the same as in FUN")
  }
  v_outcomes <- colnames(funtest)[-1]
  
  if(!(outcome %in% v_outcomes)){
    stop("outcome is not part of FUN outcomes")
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
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of parameter values. If NULL, 100 parameter values are 
#' used
#' @param params_basecase List with parameters for the base case
#' @param FUN Function that takes \code{params_basecase} and \code{...} and 
#' produces \code{outcome} of interest
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param strategies vector of strategy names. The default (NULL) will use 
#' strategy names in FUN
#' @param ... Further arguments to FUN (not used)
#' @keywords owsa
#' @return 
#' A dataframe with the results of the sensitivity analysis. Can be 
#' visualized with \code{plot.owsa}, and \code{owsa_tornado}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of the columns must be outcomes.
#' @export
twsa_det <- function(parm1, parm2, ranges, nsamps = 40, params_basecase, FUN, outcome, 
                     strategies = NULL, ...){
  ### Check for errors
  if(sum(c(parm1, parm2) %in% names(params_basecase)) != 2){
    stop("parm1 and parm2 should be in names of params_basecase")
  }
  
  if(typeof(ranges)!="list"){
    stop("ranges should be a list")
  }
  
  if(length(ranges)!=2){
    stop("The number of elements in ranges has to be two")
  }
  
  jj <- tryCatch({
    funtest <- FUN(params_basecase, ...)  
  }, error = function(e) NA)
  if(is.na(sum(is.na(jj)))){
    stop("FUN is not well defined by 'params_basecase' and ...")
  }
  funtest <- FUN(params_basecase, ...)
  if(is.null(strategies)){
    strategies <- funtest[,1]
    n_str <- length(strategies) 
  }
  else{
    n_str <- length(strategies)
  }
  if(length(strategies)!=length(funtest[, 1])){
    stop("Number of strategies not the same as in FUN")
  }
  v_outcomes <- colnames(funtest)[-1]
  
  if(!(outcome %in% v_outcomes)){
    stop("outcome is not part of FUN outcomes")
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
  
  ### Initialize matrix to store outcomes from a OWSA of the CEA
  m_out_twsa <- matrix(0, 
                       nrow = n_rows, 
                       ncol = n_str)
  
  ### Run model and capture outcome
  l_twsa_input <- params_basecase
  for (i in 1:n_rows){ # i <- 1
    l_twsa_input[names(l_twsa_input) == parm1] <- df_twsa_params[i,1]
    l_twsa_input[names(l_twsa_input) == parm2] <- df_twsa_params[i,2]
    m_out_twsa[i, ] <- FUN(l_twsa_input, ...)[[outcome]]
    
    ## Display simulation progress
    if(i/(n_rows/10) == round(i/(n_rows/10),0)) {
      cat('\r', paste(i/n_rows * 100, "% done", sep = " "))
    }
  }
  
  df_twsa <- data.frame(df_twsa_params,
                        m_out_twsa)
  names(df_twsa)[-c(1:2)] <- strategies
  
  
  df_twsa_lng <- reshape2::melt(df_twsa, id.vars = c(parm1, parm2), 
                                variable.name = "strategy", 
                                value.name = "outcome_val")
  
  class(df_twsa_lng) <- c("twsa", "data.frame")
  
  return(df_twsa_lng)
}