#' Load mortality data
#'
#' \code{load_mort_data} is used to load age-specific mortality from .csv file 
#' into vector.
#'
#' @param file String with the location and name of the file with mortality 
#' data. If \code{NULL}, \code{v_r_mort_by_age} will be used as default
#' @return 
#' A vector with mortality by age.
#' @export
load_mort_data <- function(file = NULL){
  # Load mortality data from file
  if(!is.null(file)) {
    df_r_mort_by_age <- read.csv(file = file)}
  else{
    df_r_mort_by_age <- all_cause_mortality
  }
  # Vector with mortality rates
  v_r_mort_by_age  <- as.matrix(dplyr::select(df_r_mort_by_age, .data$Total))
  
  return(v_r_mort_by_age)
}

#' Load all parameters
#'
#' \code{load_all_params} loads all parameters for the decision model from multiple sources and creates a list.
#'
#' @param file.init String with the location and name of the file with initial set of parameters
#' @param file.mort String with the location and name of the file with mortality data
#' @return 
#' A list of all parameters used for the decision model.
#' @export
load_all_params <- function(file.init = NULL,
                            file.mort = NULL){ # User defined
  #### Load initial set of initial parameters from .csv file ####
  if(!is.null(file.init)) {
    df_params_init <- read.csv(file = file.init)
  } else{
    df_params_init <- df_params_init
  }
  
  #### All-cause age-specific mortality from .csv file ####
  v_r_mort_by_age <- load_mort_data(file = file.mort)
  
  l_params_all <- with(as.list(df_params_init), {
    #### General setup ####
    v_names_str <- c("No Treatment", "Treatment")  # CEA strategies
    n_str       <- length(v_names_str) # Number of strategies
    v_age_names <- n_age_init:(n_age_init + n_t - 1) # vector with age names
    v_n <- c("H", "S1", "S2", "D")  # vector with the 4 health states of the model:
                                    # Healthy (H), Sick (S1), Sicker (S2), Dead (D)
    n_states <- length(v_n)         # number of health states 
    v_s_init <- c(H = 1, S1 = 0, S2 = 0, D = 0) # initial state vector
    #### Create list with all parameters ####
    l_params_all <- list(
      v_names_str = v_names_str,
      n_str       = n_str      ,
      n_age_init  = n_age_init, 
      n_t         = n_t       , 
      v_age_names = v_age_names,
      v_n = v_n,
      n_states = n_states,
      v_s_init = c(H = 1, S1 = 0, S2 = 0, D = 0),
      v_r_mort_by_age = v_r_mort_by_age
    )
    return(l_params_all)
  }
  )
  
  l_params_all <- c(l_params_all, 
                    df_params_init) # Add initial set of parameters
}

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