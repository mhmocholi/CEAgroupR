
#' Summarize CEA Parameter Definitions
#'
#' Generates structured tables summarizing deterministic,
#' probabilistic and scenario parameter specifications.
#'
#' @param object An object of class \code{cea_parameters}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A named list containing up to three data frames:
#'   \code{deterministic}, \code{probabilistic} and \code{scenarios}.
#'   Only specifications present in the parameter definitions
#'   are included.
#'
#'   Scenario summaries include the baseline value and the
#'   alternative values defined for each scenario.
#'
#' @examples
#' parameters <- define_cea_parameters(
#'   gp_visit = list(
#'     type = "cost",
#'     resource = "gp_visits",
#'     base = 65,
#'     deterministic = list(base = 65, lower = 50, upper = 80),
#'     scenarios = list(optimistic = 21, pessimistic = 134)
#'   )
#' )
#'
#' summary(parameters)
#'
#' @export
summary.cea_parameters <- function(object, ...) {

  if (!inherits(object, "cea_parameters")) {
    stop("'object' must be a 'cea_parameters' object.")
  }

  # ---------------------------------------------------------------------------
  # Detect structured output or original parameter definitions
  # ---------------------------------------------------------------------------

  if (all(c("summary", "parameters") %in% names(object)) &&
      inherits(object$summary, "summary_cea_parameters")) {

    return(object$summary)
  }

  # ---------------------------------------------------------------------------
  # 1. Internal helper: convert records into a data frame
  # ---------------------------------------------------------------------------

  records_to_df <- function(records) {

    if (length(records) == 0L) {
      return(NULL)
    }

    all_fields <- unique(unlist(
      lapply(records, names),
      use.names = FALSE
    ))

    normalized <- lapply(records, function(record) {

      missing_fields <- setdiff(all_fields, names(record))

      for (field in missing_fields) {
        record[field] <- list(NA)
      }

      record[all_fields]
    })

    result <- do.call(
      rbind,
      lapply(normalized, function(record) {
        as.data.frame(
          record,
          stringsAsFactors = FALSE,
          optional = TRUE
        )
      })
    )

    rownames(result) <- NULL

    result
  }

  # ---------------------------------------------------------------------------
  # 2. Prepare records
  # ---------------------------------------------------------------------------

  deterministic_records <- list()
  probabilistic_records <- list()
  scenario_records <- list()

  for (name in names(object)) {

    parameter <- object[[name]]

    common <- list(
      parameter = name,
      type = parameter$type,
      resource = parameter$resource,
      comparison = parameter$comparison
    )

    # -------------------------------------------------------------------------
    # 2.1. Deterministic
    # -------------------------------------------------------------------------

    if (!is.null(parameter$deterministic)) {

      deterministic_records[[name]] <- c(
        common,
        parameter$deterministic
      )
    }

    # -------------------------------------------------------------------------
    # 2.2. Probabilistic
    # -------------------------------------------------------------------------

    if (!is.null(parameter$probabilistic)) {

      probabilistic_records[[name]] <- c(
        common,
        parameter$probabilistic
      )
    }

    # -------------------------------------------------------------------------
    # 2.3. Scenarios
    # -------------------------------------------------------------------------

    if (!is.null(parameter$scenarios)) {

      scenario_records[[name]] <- c(
        common,
        list(base = parameter$base),
        parameter$scenarios
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Generate summary tables
  # ---------------------------------------------------------------------------

  result <- list()

  deterministic_df <- records_to_df(deterministic_records)
  probabilistic_df <- records_to_df(probabilistic_records)
  scenarios_df <- records_to_df(scenario_records)

  if (!is.null(deterministic_df)) {
    result$deterministic <- deterministic_df
  }

  if (!is.null(probabilistic_df)) {
    result$probabilistic <- probabilistic_df
  }

  if (!is.null(scenarios_df)) {
    result$scenarios <- scenarios_df
  }

  class(result) <- c(
    "summary_cea_parameters",
    "list"
  )

  return(result)
}
