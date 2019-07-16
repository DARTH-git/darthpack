#' Summarize posterior output
#'
#' \code{data_summary} is used to to calculate the mean, standard deviation and 
#' 95% credible interval.
#' @param data Data frame.
#' @param varname Name of a column containing the variable.
#' @param groupnames Vector of column names to be used as grouping variables.
#' @return 
#' A data frame containing the posterior output.
#' @export
data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = TRUE),
      median = quantile(x[[col]], probs = 0.5, names = FALSE),
      sd = sd(x[[col]], na.rm=TRUE),
      lb = quantile(x[[col]], probs = 0.025, names = FALSE),
      ub = quantile(x[[col]], probs = 0.975, names = FALSE))
  }
  data_sum <- plyr::ddply(data, groupnames, .fun = summary_func, 
                    varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}