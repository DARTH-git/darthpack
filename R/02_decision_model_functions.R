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
#' \code{check_transition_probability} checks if transition probabilities are in
#' \[0, 1\] and reports any entry that is not, including entries that are
#' \code{NA} or \code{NaN}.
#'
#' @param a_P A transition probability array.
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default = FALSE.
#' @param verbose Logical variable to indicate print out of messages. 
#' Default = FALSE
#'
#' @return
#' Invisibly, a data frame with one row per entry of \code{a_P} that is not a
#' valid probability, empty when the array is valid. With
#' \code{err_stop = TRUE} the function stops instead, and with
#' \code{verbose = TRUE} it warns, listing the offending entries.
#' @import utils
#' @examples 
#' l_params_all <- load_all_params()
#' a_P <- decision_model(l_params_all)$a_P
#' check_transition_probability(a_P, verbose = TRUE)
#' @export
check_transition_probability <- function(a_P,
                                         err_stop = FALSE, 
                                         verbose = FALSE) {
  
  # An NA or NaN entry means the transition probability is undefined. Comparing
  # it with < or > yields NA, which which() drops, so such an array used to pass
  # this check and the model went on to produce a cohort trace full of NA.
  m_indices_notvalid <- arrayInd(which(a_P < 0 | a_P > 1 | is.na(a_P)), 
                                 dim(a_P))
  
  df_notvalid <- data.frame(`Transition probabilities not valid:` = 
                              character(0), 
                            check.names = FALSE)
  
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
  
  return(invisible(df_notvalid))
}


#' Check if the sum of transition probabilities equal to one. 
#'
#' \code{check_sum_of_transition_array} checks if each of the rows of the 
#' transition matrices sum to one. 
#' 
#' @param a_P A transition probability array.
#' @param n_states Number of health states. Defaults to the first dimension of 
#' \code{a_P}.
#' @param n_t Number of cycles. Defaults to the third dimension of \code{a_P}.
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default = FALSE.
#' @param verbose Logical variable to indicate print out of messages. 
#' Default = FALSE
#' @param tolerance Numeric tolerance for the comparison of each row sum with 
#' one. Default = 1e-08.
#' @return 
#' Invisibly, a data frame with one row per row of \code{a_P} whose transition
#' probabilities do not sum to one, empty when the array is valid. With
#' \code{err_stop = TRUE} the function stops instead, and with
#' \code{verbose = TRUE} it warns, listing the offending rows and cycles.
#' @import dplyr
#' @examples 
#' l_params_all <- load_all_params()
#' a_P <- decision_model(l_params_all)$a_P
#' check_sum_of_transition_array(a_P, verbose = TRUE)
#' @export
check_sum_of_transition_array <- function(a_P,
                                          n_states = dim(a_P)[1],
                                          n_t = dim(a_P)[3],  
                                          err_stop = FALSE, 
                                          verbose = FALSE,
                                          tolerance = 1e-08) {
  
  if (!identical(as.integer(dim(a_P)), 
                 as.integer(c(n_states, n_states, n_t)))) {
    stop("The transition probability array has dimensions ", 
         paste(dim(a_P), collapse = " x "), 
         ", but n_states = ", n_states, " and n_t = ", n_t, 
         " describe an array of ", n_states, " x ", n_states, " x ", n_t)
  }
  
  # The row sums of every transition matrix in the array. The previous version
  # compared sum(rowSums(x)) with n_states, that is, the sum of all the row sums
  # of a cycle against the number of states. Row sums that are wrong in opposite
  # directions cancel in that total, so an array with rows summing to, say, 1.25
  # and 0.75 passed the check. It also used exact == equality on floating point
  # numbers, which can fail on a valid array.
  m_rowsums <- apply(a_P, c(1, 3), sum)
  
  m_indices_notvalid <- arrayInd(which(abs(m_rowsums - 1) > tolerance | 
                                         is.na(m_rowsums)), 
                                 dim(m_rowsums))
  
  df_notvalid <- data.frame(`Rows not summing to one:` = character(0), 
                            check.names = FALSE)
  
  if (dim(m_indices_notvalid)[1] != 0) {
    v_rows_notval   <- rownames(a_P)[m_indices_notvalid[, 1]]
    v_cycles_notval <- dimnames(a_P)[[3]][m_indices_notvalid[, 2]]
    v_sums_notval   <- m_rowsums[m_indices_notvalid]
    
    df_notvalid <- data.frame(`Rows not summing to one:` = 
                                matrix(paste0("from ", v_rows_notval, 
                                              "; at cycle ", v_cycles_notval, 
                                              "; sums to ", 
                                              format(v_sums_notval, digits = 8)), 
                                       ncol = 1), 
                              check.names = FALSE)
    
    if(err_stop) {
      stop("This is not a valid transition Matrix\n",
           paste(capture.output(df_notvalid), collapse = "\n"))
    }
    
    if(verbose){
      warning("This is not a valid transition Matrix\n",
              paste(capture.output(df_notvalid), collapse = "\n"))
    } 
  }
  
  return(invisible(df_notvalid))
}
