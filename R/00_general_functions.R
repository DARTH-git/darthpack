#' Update parameters
#'
#' \code{update_param_list} is used to update list of all parameters with new 
#' values for specific parameters.
#'
#' @param l_params_all List with all parameters of decision model
#' @param params_updated Parameters for which values need to be updated
#' @return 
#' A list with all parameters updated.
#' @export
update_param_list <- function(l_params_all, params_updated){

  if (typeof(params_updated)!="list"){
    params_updated <- split(unname(params_updated),names(params_updated)) #converte the named vector to a list
  }
  l_params_all <- modifyList(l_params_all, params_updated) #update the values
  return(l_params_all)
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
#' and the rest of teh columns must be outcomes.
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
#' and the rest of teh columns must be outcomes.
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

