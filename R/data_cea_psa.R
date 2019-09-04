#' Cost-effectiveness results from probabilistic analysis
#'
#' A dataset with cost and effectiveness outputs for each strategy.
#' @format A \code{data.frame} with with 2 rows, ane per strategy and 
#' 5 variables:
#' \describe{
#'   \item{Strategy}{Strategy name}
#'   \item{Cost}{Cost per strategy}
#'   \item{Effect}{QALYs per strategy}
#'   \item{Inc_Cost}{Incremental cost}
#'   \item{Inc_Effect}{Incremental QALYs}
#'   \item{ICER}{Incremental cost-effectivenes ratio (ICER)}
#'   \item{Status}{Domination status. ND, not dominated (i.e., on the 
#'   cost-effectivenes efficiency frontier); D, strongly dominated; d,
#'   dominated by extension}
#' }
#' @docType data
"df_cea_psa"