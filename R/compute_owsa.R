
#' Compute One-Way Deterministic Sensitivity Analysis (OWSA)
#'
#' Performs a one-way deterministic sensitivity analysis of incremental
#' costs, incremental effects, Incremental Cost-Effectiveness Ratios (ICERs),
#' and incremental Net Monetary Benefits (NMBs).
#'
#' The function evaluates the impact of varying individual cost parameters
#' between user-defined lower and upper bounds, while holding all other
#' parameters constant.
#'
#' Each parameter is linked to an individual-level resource utilization
#' variable. Alternative costs are calculated by adjusting the original
#' individual costs according to the difference between the scenario-specific
#' and baseline unit costs.
#'
#' The function supports multiple parameters and multiple intervention
#' strategies compared against a common reference strategy.
#'
#' @param data A data frame containing individual-level costs, effects,
#'   resource utilization and strategy identifiers.
#'
#' @param group Character string specifying the strategy grouping variable.
#'
#' @param cost Character string specifying the individual-level cost variable.
#'
#' @param effect Character string specifying the individual-level effectiveness
#'   variable (e.g. QALYs).
#'
#' @param ref Character string identifying the reference strategy against
#'   which incremental comparisons are computed.
#'
#' @param parameters A data frame containing the parameters to be varied.
#'   The following columns are required:
#'   \itemize{
#'     \item \code{parameter}: parameter identifier.
#'     \item \code{resource}: corresponding resource utilization variable
#'           in \code{data}.
#'     \item \code{base}: baseline unit cost.
#'     \item \code{lower}: lower parameter value.
#'     \item \code{upper}: upper parameter value.
#'   }
#'
#' @param lambda Numeric willingness-to-pay threshold used to calculate
#'   incremental NMB. Defaults to 25000.
#'
#' @details
#' For each parameter, three scenarios are evaluated: baseline, lower
#' and upper. Only one parameter is varied at a time.
#'
#' Individual-level costs are recalculated as:
#' \deqn{C_{i,new} = C_{i,base} + R_i(P_{new} - P_{base})}
#'
#' where \eqn{R_i} represents individual resource utilization and
#' \eqn{P} represents the corresponding unit cost.
#'
#' The analysis is deterministic and does not perform bootstrap resampling.
#' Consequently, the resulting scenarios describe parameter sensitivity
#' rather than sampling uncertainty.
#'
#' Results can be visualized using \code{\link{plot.tornado}}.
#'
#' @return
#' An object of class \code{owsa_result} containing:
#' \itemize{
#'   \item \code{sensitivity_results}: a data frame containing scenario-specific
#'         mean costs, mean effects, incremental costs, incremental effects,
#'         ICERs and incremental NMBs for each alternative strategy.
#'   \item \code{parameters}: the parameter specification table.
#'   \item \code{settings}: analysis settings and metadata.
#' }
#'
#' @examples
#' ## Example 1: One-way sensitivity analysis of healthcare unit costs
#' owsa_parameters <- data.frame(
#'   parameter = c("gp_visit", "specialist_visit", "hospital_day"),
#'   resource  = c("gp_visits", "specialist_visits", "hospital_days"),
#'   base      = c(65, 120, 650),
#'   lower     = c(50, 90, 500),
#'   upper     = c(80, 150, 800)
#' )
#'
#' res_owsa <- compute_owsa(
#'   data       = cua_example,
#'   group      = "group",
#'   cost       = "cost_total",
#'   effect     = "effect",
#'   ref        = "usual_care",
#'   parameters = owsa_parameters
#' )
#'
#' res_owsa$sensitivity_results
#'
#' @export


compute_owsa <- function(data, group, cost, effect, ref,
                         parameters, lambda = 25000) {

  required <- c("parameter", "resource", "base", "lower", "upper")

  if (!is.data.frame(parameters) ||
      !all(required %in% names(parameters)) ||
      nrow(parameters) == 0L) {
    stop("Invalid parameters table.")
  }

  if (!is.data.frame(data) ||
      !all(c(group, cost, effect) %in% names(data))) {
    stop("Invalid dataset or variable names.")
  }

  if (!all(parameters$resource %in% names(data))) {
    stop("Some resource variables are missing from data.")
  }

  if (!all(vapply(data[c(cost, effect, parameters$resource)],
                  is.numeric, logical(1)))) {
    stop("Cost, effect and resource variables must be numeric.")
  }

  if (!all(vapply(parameters[c("base", "lower", "upper")],
                  is.numeric, logical(1))) ||
      anyNA(parameters[required]) ||
      any(parameters$lower > parameters$base) ||
      any(parameters$base > parameters$upper)) {
    stop("Invalid parameter values or limits.")
  }

  if (!ref %in% data[[group]]) {
    stop("Reference group not found.")
  }

  if (!is.numeric(lambda) || length(lambda) != 1L ||
      !is.finite(lambda)) {
    stop("Invalid lambda.")
  }

  results <- lapply(seq_len(nrow(parameters)), function(i) {

    p <- parameters[i, ]

    scenarios <- c(
      baseline = p$base,
      lower = p$lower,
      upper = p$upper
    )

    lapply(names(scenarios), function(scenario) {

      modified_data <- data

      modified_data[[cost]] <- data[[cost]] +
        data[[p$resource]] * (scenarios[[scenario]] - p$base)

      summary <- aggregate(
        modified_data[c(cost, effect)],
        by = list(group = modified_data[[group]]),
        FUN = mean
      )

      reference <- summary[summary$group == ref, ]

      summary$delta_cost <- summary[[cost]] - reference[[cost]]
      summary$delta_effect <- summary[[effect]] - reference[[effect]]

      summary$icer <- ifelse(
        summary$delta_effect == 0,
        NA_real_,
        summary$delta_cost / summary$delta_effect
      )

      summary$nmb <- lambda * summary$delta_effect -
        summary$delta_cost

      summary$scenario <- scenario
      summary$parameter <- p$parameter
      summary$parameter_value <- scenarios[[scenario]]

      summary[summary$group != ref, ]
    })
  })

  structure(
    list(
      sensitivity_results = do.call(
        rbind,
        unlist(results, recursive = FALSE)
      ),
      parameters = parameters,
      settings = list(
        group = group,
        cost = cost,
        effect = effect,
        ref = ref,
        lambda = lambda
      )
    ),
    class = "owsa_result"
  )
}
