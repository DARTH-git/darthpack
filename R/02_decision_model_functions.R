#' Decision Model
#'
#' \code{decision_model} implements the decision model used.
#'
#' @param l_params_all List with all parameters of decision model
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default = FALSE.
#' @param verbose Logical variable to indicate print out of messages. Default = FALSE
#' @return 
#' The transition probability array and the cohort trace matrix.
#' @export
decision_model <- function(l_params_all, err_stop = FALSE, verbose = FALSE){ # User defined
  ### Definition:
  ##   Decision model implementation function
  ### Arguments:  
  ##   l_params_all: List with all parameters of decision model
  ##   verbose: Logical variable to indicate print out of messages
  ### Returns:
  ##   a_P: Transition probability array
  ##   m_M: Matrix cohort trace
  ##
  with(as.list(l_params_all), {
    #### Error checking ####
    if ((n_t + n_age_init) > nrow(v_r_mort_by_age)) {
      stop("Not all the age in the age range have a corresponding mortality rate")
    }
    
    if ((sum(v_s_init) != 1) | !all(v_s_init >= 0)) {
      stop("vector of initial states (v_s_init) is not valid")
    }

    #### Age-specific transition probabilities ####
    # Mortality for healthy individuals
    p_HDage  <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)])        
    # Mortality for sick individuals
    p_S1Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S1)
    # Mortality for sicker individuals
    p_S2Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S2)
    
    #### Create age-specific transition probability matrices in an array ####
    # Initialize array
    a_P <- array(0, dim = c(n_states, n_states, n_t),
                 dimnames = list(v_n, v_n, 0:(n_t-1)))
    # Fill in array
    # From H
    a_P["H", "H", ]  <- (1-p_HDage) * (1 - p_HS1)
    a_P["H", "S1", ] <- (1-p_HDage) * p_HS1
    a_P["H", "D", ]  <- p_HDage
    # From S1
    a_P["S1", "H", ]  <- (1-p_S1Dage) * p_S1H
    a_P["S1", "S1", ] <- (1-p_S1Dage) * (1 - (p_S1S2 + p_S1H))
    a_P["S1", "S2", ] <- (1-p_S1Dage) * p_S1S2
    a_P["S1", "D", ]  <- p_S1Dage
    # From S2
    a_P["S2", "S2", ] <- 1 - p_S2Dage
    a_P["S2", "D", ]  <- p_S2Dage
    # From D
    a_P["D", "D", ] <- 1
    
    #### Check if transition array is valid ####
    check_transition_probability(a_P, err_stop = err_stop, verbose = verbose)
    check_sum_of_transition_array(a_P, n_states, n_t, err_stop = err_stop, verbose = verbose)
    
    #### Compute cohort trace matrix and transition array for age-dependent STM ####
    # Initialize cohort trace matrix
    m_M <- matrix(0, 
                  nrow = (n_t + 1), ncol = n_states, 
                  dimnames = list(0:n_t, v_n))
    # Set first row of m.M with the initial state vector
    m_M[1, ] <- v_s_init
    
    # Iterate STM over time
    for(t in 1:n_t){
      m_M[t + 1, ] <- m_M[t, ] %*% a_P[, , t]
    }
    return(list(a_P = a_P,
                m_M = m_M))
  }
  )
}

#' Check if transition array is valid
#'
#' \code{check_transition_probability} checks if transition probabilities are in \[0, 1\].
#'
#' @param a_P A transition probability array.
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default = FALSE.
#' @param verbose Logical variable to indicate print out of messages. 
#' Default = FALSE
#'
#' @return
#' This function stops if transition probability array is not valid and shows 
#' what are the entries that are not valid
#' @import utils
#' @export
check_transition_probability <- function(a_P,
                                         err_stop = FALSE, 
                                         verbose = FALSE) {
  
  m_indices_notvalid <- arrayInd(which(a_P < 0 | a_P > 1), 
                                 dim(a_P))
  
  if(dim(m_indices_notvalid)[1] != 0){
    v_rows_notval   <- rownames(a_P)[m_indices_notvalid[, 1]]
    v_cols_notval   <- colnames(a_P)[m_indices_notvalid[, 2]]
    v_cycles_notval <- dimnames(a_P)[[3]][m_indices_notvalid[, 3]]
    
    df_notvalid <- data.frame(`Transition probabilities not valid:` = 
                                matrix(paste0(paste(v_rows_notval, v_cols_notval, sep = "->"),
                                              "; at cycle ",
                                              v_cycles_notval), ncol = 1), 
                              check.names = FALSE)
    
    if(err_stop) {
      stop("Not valid transition probabilities\n",
           paste(capture.output(df_notvalid), collapse = "\n"))
    }
        
    if(verbose){
      warning("Not valid transition probabilities\n",
           paste(capture.output(df_notvalid), collapse = "\n"))
    } 
  }
}


#' Check if the sum of transition probabilities equal to one. 
#'
#' \code{check_sum_of_transition_array} checks if each of the rows of the 
#' transition matrices sum to one. 
#' 
#' @param a_P A transition probability array.
#' @param n_states Number of health states.
#' @param n_t Number of cycles.
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default = FALSE.
#' @param verbose Logical variable to indicate print out of messages. 
#' Default = FALSE
#' @return 
#' The transition probability array and the cohort trace matrix.
#' @import dplyr
#' @export
check_sum_of_transition_array <- function(a_P,
                                          n_states,
                                          n_t,  
                                          err_stop = FALSE, 
                                          verbose = FALSE) {
  
  valid <- (apply(a_P, 3, function(x) sum(rowSums(x))) == n_states)
  if (!isTRUE(all_equal(as.numeric(sum(valid)), as.numeric(n_t)))) {
    if(err_stop) {
      stop("This is not a valid transition Matrix")
    }
    
    if(verbose){
      warning("This is not a valid transition Matrix")
    } 
  }
}
