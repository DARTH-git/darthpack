#' Cost-effectiveness results from probabilistic analysis
#'
#' A dataset with cost and effectiveness outputs for each strategy.
#' @format A \code{data.frame} with 2 rows, one per strategy, and 7 variables:
#' \describe{
#'   \item{Strategy}{Strategy name}
#'   \item{Cost}{Cost per strategy}
#'   \item{Effect}{QALYs per strategy}
#'   \item{Inc_Cost}{Incremental cost}
#'   \item{Inc_Effect}{Incremental QALYs}
#'   \item{ICER}{Incremental cost-effectiveness ratio (ICER)}
#'   \item{Status}{Domination status. ND, not dominated (i.e., on the 
#'   cost-effectiveness efficiency frontier); D, strongly dominated; d,
#'   dominated by extension}
#' }
#' @docType data
"df_cea_psa"