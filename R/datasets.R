#' Example cost-effectiveness dataset: cua_base
#'
#' Synthetic dataset used to illustrate individual-level cost-effectiveness
#' analysis. Contains simulated observations including total cost, effectiveness,
#' treatment strategy and subgroup variables.
#'
#' @format A data frame with 200 rows and 7 variables:
#' \describe{
#'   \item{id}{Individual identifier}
#'   \item{group}{Strategy identifier}
#'   \item{cost_total}{Total cost}
#'   \item{effect}{Effectiveness (QALYs)}
#'   \item{diabetes}{Binary subgroup variable}
#'   \item{HTA}{Binary subgroup variable}
#'   \item{n_comorbidities}{Number of comorbidities}
#' }
#'
#' @source Synthetic example generated for internal demonstrations.
"cua_base"

#' Example cost-effectiveness dataset: cua_base_discounted
#'
#' Discounted version of the \code{cua_base} dataset. Costs and effectiveness
#' values have been adjusted using a discount rate for demonstration purposes.
#'
#' @format Same structure as \code{cua_base}.
#'
#' @source Synthetic example generated for internal demonstrations.
"cua_base_discounted"

#' Multigroup cost-effectiveness dataset: cua_multi
#'
#' Synthetic dataset containing multiple strategies with individual-level cost
#' and effectiveness values. Designed to illustrate multigroup analysis with
#' several treatment arms and subgroup variables.
#'
#' @format A data frame with 800 rows and 7 variables (same structure as
#'   \code{cua_base}).
#'
#' @source Synthetic example generated for internal demonstrations.
"cua_multi"

#' Multigroup cost-effectiveness dataset: cua_multi_discounted
#'
#' Discounted version of the \code{cua_multi} dataset. All cost and effectiveness
#' values have been adjusted using a discount rate to illustrate scenario-based
#' or sensitivity analyses.
#'
#' @format Same structure as \code{cua_multi}.
#'
#' @source Synthetic example generated for internal demonstrations.
"cua_multi_discounted"
