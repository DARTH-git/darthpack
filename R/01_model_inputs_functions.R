#' Load mortality data
#'
#' \code{load_mort_data} is used to load age-specific mortality from .csv file
#' into vector.
#'
#' @param file String with the location and name of the file with mortality
#' data. If \code{NULL}, \code{all_cause_mortality} will be used as default
#' @return
#' A vector with mortality by age.
#' @examples
#' v_r_mort_by_age <- load_mort_data()
#' head(v_r_mort_by_age)
#' @export
load_mort_data <- function(file = NULL){
  # Load mortality data from file
  if(!is.null(file)) {
    if (!file.exists(file)) {
      stop("The file with mortality data does not exist: ", file)
    }
    df_r_mort_by_age <- read.csv(file = file)
  }
  else{
    df_r_mort_by_age <- all_cause_mortality
  }

  if (!("Total" %in% names(df_r_mort_by_age))) {
    stop("The mortality data must have a 'Total' column with the mortality ",
         "rate of the total population. Columns found: ",
         paste(names(df_r_mort_by_age), collapse = ", "))
  }

  # Vector with mortality rates
  v_r_mort_by_age  <- as.matrix(dplyr::select(df_r_mort_by_age, "Total"))

  return(v_r_mort_by_age)
}

#' Load all parameters
#'
#' \code{load_all_params} loads all parameters for the decision model from multiple sources and creates a list.
#'
#' @param file.init String with the location and name of the file with initial
#' set of parameters. If \code{NULL}, \code{df_params_init} will be used as
#' default
#' @param file.mort String with the location and name of the file with mortality
#' data. If \code{NULL}, \code{all_cause_mortality} will be used as default
#' @return
#' A list of all parameters used for the decision model.
#' @examples
#' l_params_all <- load_all_params()
#' str(l_params_all)
#' @export
load_all_params <- function(file.init = NULL,
                            file.mort = NULL){ # User defined
  #### Load initial set of initial parameters from .csv file ####
  if(!is.null(file.init)) {
    if (!file.exists(file.init)) {
      stop("The file with the initial set of parameters does not exist: ",
           file.init)
    }
    df_params_all <- read.csv(file = file.init)
  } else{
    # Package data; the local is named differently so that it does not shadow
    # the data object on the right-hand side
    df_params_all <- df_params_init
  }

  #### Check that the initial set of parameters is complete ####
  # Without this check, a missing parameter surfaces much later as an obscure
  # "object 'n_age_init' not found" from inside with(as.list(...))
  v_params_required <- c("c_H", "c_S1", "c_S2", "c_D", "c_Trt",
                         "u_H", "u_S1", "u_S2", "u_D", "u_Trt",
                         "p_HS1", "p_S1H", "p_S1S2", "hr_S1", "hr_S2",
                         "n_age_init", "n_t", "d_c", "d_e")
  v_params_missing <- setdiff(v_params_required, names(df_params_all))
  if (length(v_params_missing) > 0) {
    stop("The initial set of parameters is missing: ",
         paste(v_params_missing, collapse = ", "))
  }
  if (nrow(df_params_all) != 1) {
    stop("The initial set of parameters must have exactly one row, one column ",
         "per parameter. Rows found: ", nrow(df_params_all))
  }

  #### All-cause age-specific mortality from .csv file ####
  v_r_mort_by_age <- load_mort_data(file = file.mort)

  l_params_all <- with(as.list(df_params_all), {
    #### General setup ####
    v_names_str <- c("No Treatment", "Treatment")  # CEA strategies
    n_str       <- length(v_names_str) # Number of strategies
    v_age_names <- n_age_init:(n_age_init + n_t) # vector with age names, one
                                    # per cycle of the cohort trace (0 to n_t)
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
      v_s_init = v_s_init,
      v_r_mort_by_age = v_r_mort_by_age
    )
    return(l_params_all)
  }
  )

  l_params_all <- c(l_params_all,
                    df_params_all) # Add initial set of parameters

  return(l_params_all)
}

#' Update parameters
#'
#' \code{update_param_list} is used to update list of all parameters with new
#' values for specific parameters.
#'
#' @param l_params_all List with all parameters of decision model
#' @param params_updated Parameters for which values need to be updated
#' @param allow_new Logical variable to allow \code{params_updated} to contain
#' names that are not already in \code{l_params_all}. When \code{FALSE}
#' (default), such names raise an error, which catches misspelled parameter
#' names before they silently leave the model running at its old values.
#' Default = FALSE.
#' @return
#' A list with all parameters updated.
#' @examples
#' l_params_all <- load_all_params()
#' l_params_upd <- update_param_list(l_params_all, c(p_S1S2 = 0.2))
#' l_params_upd$p_S1S2
#' @export
update_param_list <- function(l_params_all, params_updated, allow_new = FALSE){

  #### Error checking ####
  # Checked before the split() below, which fails with "group length is 0 but
  # data length > 0" on an unnamed vector
  if (is.null(names(params_updated)) || any(names(params_updated) == "")) {
    stop("All elements of 'params_updated' must be named after the parameter ",
         "they update")
  }

  if (typeof(params_updated)!="list"){
    params_updated <- split(unname(params_updated),names(params_updated)) #converte the named vector to a list
  }

  # A misspelled name used to be appended to l_params_all while the parameter
  # the user meant to change kept its old value, so the model ran to completion
  # and returned results for the base case with no warning
  v_names_unknown <- setdiff(names(params_updated), names(l_params_all))
  if (length(v_names_unknown) > 0 && !allow_new) {
    stop("The following parameter(s) are not in 'l_params_all': ",
         paste(v_names_unknown, collapse = ", "),
         ". Check the spelling, or set allow_new = TRUE to add them.")
  }

  l_params_all <- modifyList(l_params_all, params_updated) #update the values
  return(l_params_all)
}
