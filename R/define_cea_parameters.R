
#' Define Parameters for Cost-Effectiveness Sensitivity and Scenario Analyses
#'
#' Constructs a structured collection of parameters for deterministic
#' sensitivity analysis, probabilistic sensitivity analysis and
#' scenario analysis in CEAgroupR.
#'
#' Each parameter must contain at least one specification:
#' deterministic, probabilistic or scenarios.
#' Any combination of these specifications is permitted.
#'
#' Deterministic specifications are used by OWSA and TWSA.
#' Probabilistic specifications are intended for PSA.
#' Scenario specifications are intended for scenario analysis.
#'
#' @param ... Named parameter definitions created with list().
#'   Each parameter may contain:
#'   \itemize{
#'     \item \code{type}: "cost" or "effect".
#'     \item \code{resource}: resource utilization variable for costs.
#'     \item \code{comparison}: optional target comparison for effects.
#'     \item \code{deterministic}: optional list with base, lower, upper.
#'     \item \code{probabilistic}: optional distribution specification.
#'     \item \code{scenarios}: optional named list of scenario values.
#'   }
#'
#' @details
#' Supported probabilistic distributions:
#'
#' \itemize{
#'   \item \code{normal}: mean, sd.
#'   \item \code{beta}: shape1, shape2.
#'   \item \code{gamma}: shape, rate.
#'   \item \code{lognormal}: meanlog, sdlog.
#'   \item \code{uniform}: min, max.
#' }
#'
#' All distribution parameters must be finite numeric scalars.
#'
#' The gamma distribution uses the rate parameterization.
#' The lognormal distribution uses the mean and standard deviation
#' of the underlying normal distribution on the logarithmic scale.
#'
#' Deterministic effect parameters represent proportional changes
#' in incremental effectiveness and require base = 0.
#'
#' Probabilistic effect parameters are intended to represent
#' proportional adjustments to incremental effectiveness.
#' Their application will be implemented in the PSA engine.
#'
#' Scenario specifications contain one or more named numeric values.
#' Scenario names must be unique and non-empty.
#'
#' For cost parameters, scenario values represent alternative
#' unit costs and must be non-negative.
#'
#' For effect parameters, scenario values represent proportional
#' changes in incremental effectiveness and must be >= -1.
#' For example, a value of 0.20 represents a 20% increase.
#'
#' Scenario names may be shared across different parameters.
#' The interpretation and combination of these scenarios
#' are determined by the scenario analysis engine.
#'
#' Deterministic, probabilistic and scenario specifications
#' are independent. No equality between deterministic baseline
#' values and probabilistic distribution means is imposed.
#'
#' A methodological warning may be issued when a distribution
#' assigned to a cost parameter permits negative values.
#' No truncation or restriction is applied.
#'
#' @return An object of class \code{cea_parameters} containing:
#'   \itemize{
#'     \item \code{summary}: tabular summaries of the available
#'       deterministic, probabilistic and scenario specifications.
#'     \item \code{parameters}: complete validated parameter definitions.
#'   }
#'
#' @examples
#' parameters <- define_cea_parameters(
#'   gp_visit = list(
#'     type = "cost",
#'     resource = "gp_visits",
#'     deterministic = list(
#'       base = 65,
#'       lower = 50,
#'       upper = 80
#'     ),
#'     probabilistic = list(
#'       distribution = "gamma",
#'       shape = 25,
#'       rate = 25 / 65
#'     ),
#'     scenarios = list(
#'       optimistic = 21,
#'       pessimistic = 134
#'     )
#'   ),
#'   hospital_day = list(
#'     type = "cost",
#'     resource = "hospital_days",
#'     deterministic = list(
#'       base = 650,
#'       lower = 500,
#'       upper = 800
#'     ),
#'     scenarios = list(
#'       optimistic = 500,
#'       pessimistic = 900
#'     )
#'   ),
#'   qaly_general = list(
#'     type = "effect",
#'     comparison = "exercise_A",
#'     probabilistic = list(
#'       distribution = "normal",
#'       mean = 0,
#'       sd = 0.10
#'     )
#'   )
#' )
#'
#' parameters
#' parameters$summary$scenarios
#' parameters$parameters$gp_visit
#'
#' @export
define_cea_parameters <- function(...) {

  parameters <- list(...)

  # ---------------------------------------------------------------------------
  # 1. Validate parameter names
  # ---------------------------------------------------------------------------

  if (length(parameters) == 0L) {
    stop("At least one parameter must be defined.")
  }

  parameter_names <- names(parameters)

  if (is.null(parameter_names) ||
      anyNA(parameter_names) ||
      any(!nzchar(parameter_names)) ||
      anyDuplicated(parameter_names)) {

    stop("Parameters must have unique, non-empty names.")
  }

  # ---------------------------------------------------------------------------
  # 2. Internal validation helpers
  # ---------------------------------------------------------------------------

  validate_scalar <- function(value, field, parameter) {

    if (!is.numeric(value) ||
        length(value) != 1L ||
        !is.finite(value)) {

      stop(
        "Parameter '", parameter,
        "': '", field,
        "' must be a single finite numeric value."
      )
    }
  }

  validate_fields <- function(
    object,
    allowed,
    required,
    parameter,
    specification
  ) {

    fields <- names(object)

    if (is.null(fields) ||
        anyNA(fields) ||
        any(!nzchar(fields)) ||
        anyDuplicated(fields) ||
        any(!fields %in% allowed) ||
        !all(required %in% fields)) {

      stop(
        "Parameter '", parameter,
        "': invalid fields in '",
        specification, "' specification."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Validate individual parameters
  # ---------------------------------------------------------------------------

  for (name in parameter_names) {

    parameter <- parameters[[name]]

    if (!is.list(parameter)) {
      stop("Parameter '", name, "' must be a list.")
    }

    allowed_fields <- c(
      "type",
      "resource",
      "comparison",
      "deterministic",
      "probabilistic",
      "scenarios"
    )

    validate_fields(
      object = parameter,
      allowed = allowed_fields,
      required = "type",
      parameter = name,
      specification = "parameter"
    )

    # -------------------------------------------------------------------------
    # 3.1. Validate parameter type
    # -------------------------------------------------------------------------

    type <- parameter$type

    if (!is.character(type) ||
        length(type) != 1L ||
        is.na(type) ||
        !type %in% c("cost", "effect")) {

      stop(
        "Parameter '", name,
        "': 'type' must be 'cost' or 'effect'."
      )
    }

    resource <- parameter$resource
    comparison <- parameter$comparison

    if (type == "cost") {

      if (!is.character(resource) ||
          length(resource) != 1L ||
          is.na(resource) ||
          !nzchar(resource)) {

        stop(
          "Parameter '", name,
          "': cost parameters require a valid 'resource'."
        )
      }

      if (!is.null(comparison) &&
          !(length(comparison) == 1L &&
            is.atomic(comparison) &&
            is.na(comparison))) {

        stop(
          "Parameter '", name,
          "': 'comparison' is not applicable to cost parameters."
        )
      }

      comparison <- NA_character_

    } else {

      if (!is.null(resource) &&
          !(length(resource) == 1L &&
            is.atomic(resource) &&
            is.na(resource))) {

        stop(
          "Parameter '", name,
          "': effect parameters must have 'resource = NA'."
        )
      }

      resource <- NA_character_

      if (is.null(comparison)) {
        comparison <- NA_character_
      }

      if (!is.character(comparison) ||
          length(comparison) != 1L ||
          (!is.na(comparison) && !nzchar(comparison))) {

        stop(
          "Parameter '", name,
          "': invalid 'comparison'."
        )
      }
    }

    # -------------------------------------------------------------------------
    # 3.2. Check available specifications
    # -------------------------------------------------------------------------

    deterministic <- parameter$deterministic
    probabilistic <- parameter$probabilistic
    scenarios <- parameter$scenarios

    has_deterministic <- !is.null(deterministic)
    has_probabilistic <- !is.null(probabilistic)
    has_scenarios <- !is.null(scenarios)

    if (!has_deterministic &&
        !has_probabilistic &&
        !has_scenarios) {

      stop(
        "Parameter '", name,
        "': at least one specification is required: ",
        "'deterministic', 'probabilistic' or 'scenarios'."
      )
    }

    # -------------------------------------------------------------------------
    # 3.3. Validate deterministic specification
    # -------------------------------------------------------------------------

    if (has_deterministic) {

      if (!is.list(deterministic)) {

        stop(
          "Parameter '", name,
          "': 'deterministic' must be a list."
        )
      }

      validate_fields(
        object = deterministic,
        allowed = c("base", "lower", "upper"),
        required = c("base", "lower", "upper"),
        parameter = name,
        specification = "deterministic"
      )

      for (field in c("base", "lower", "upper")) {

        validate_scalar(
          deterministic[[field]],
          field,
          name
        )
      }

      if (deterministic$lower > deterministic$base ||
          deterministic$base > deterministic$upper) {

        stop(
          "Parameter '", name,
          "': expected lower <= base <= upper."
        )
      }

      if (type == "effect") {

        if (deterministic$base != 0) {

          stop(
            "Parameter '", name,
            "': deterministic effect parameters require 'base = 0'."
          )
        }

        if (deterministic$lower < -1) {

          stop(
            "Parameter '", name,
            "': effect variation cannot be below -1."
          )
        }
      }

      deterministic <- list(
        base = deterministic$base,
        lower = deterministic$lower,
        upper = deterministic$upper
      )
    }

    # -------------------------------------------------------------------------
    # 3.4. Validate probabilistic specification
    # -------------------------------------------------------------------------

    if (has_probabilistic) {

      if (!is.list(probabilistic)) {

        stop(
          "Parameter '", name,
          "': 'probabilistic' must be a list."
        )
      }

      distribution <- probabilistic$distribution

      if (!is.character(distribution) ||
          length(distribution) != 1L ||
          is.na(distribution) ||
          !nzchar(distribution)) {

        stop(
          "Parameter '", name,
          "': a valid probabilistic distribution is required."
        )
      }

      supported_distributions <- c(
        "normal",
        "beta",
        "gamma",
        "lognormal",
        "uniform"
      )

      if (!distribution %in% supported_distributions) {

        stop(
          "Parameter '", name,
          "': unsupported distribution '",
          distribution, "'."
        )
      }

      # -----------------------------------------------------------------------
      # Distribution-specific arguments
      # -----------------------------------------------------------------------

      distribution_arguments <- switch(
        distribution,
        normal = c("mean", "sd"),
        beta = c("shape1", "shape2"),
        gamma = c("shape", "rate"),
        lognormal = c("meanlog", "sdlog"),
        uniform = c("min", "max")
      )

      validate_fields(
        object = probabilistic,
        allowed = c("distribution", distribution_arguments),
        required = c("distribution", distribution_arguments),
        parameter = name,
        specification = "probabilistic"
      )

      for (field in distribution_arguments) {

        validate_scalar(
          probabilistic[[field]],
          field,
          name
        )
      }

      # -----------------------------------------------------------------------
      # Normal distribution
      # -----------------------------------------------------------------------

      if (distribution == "normal") {

        if (probabilistic$sd <= 0) {

          stop(
            "Parameter '", name,
            "': normal distribution requires 'sd > 0'."
          )
        }
      }

      # -----------------------------------------------------------------------
      # Beta distribution
      # -----------------------------------------------------------------------

      if (distribution == "beta") {

        if (probabilistic$shape1 <= 0 ||
            probabilistic$shape2 <= 0) {

          stop(
            "Parameter '", name,
            "': beta distribution requires ",
            "'shape1 > 0' and 'shape2 > 0'."
          )
        }
      }

      # -----------------------------------------------------------------------
      # Gamma distribution
      # -----------------------------------------------------------------------

      if (distribution == "gamma") {

        if (probabilistic$shape <= 0 ||
            probabilistic$rate <= 0) {

          stop(
            "Parameter '", name,
            "': gamma distribution requires ",
            "'shape > 0' and 'rate > 0'."
          )
        }
      }

      # -----------------------------------------------------------------------
      # Lognormal distribution
      # -----------------------------------------------------------------------

      if (distribution == "lognormal") {

        if (probabilistic$sdlog <= 0) {

          stop(
            "Parameter '", name,
            "': lognormal distribution requires 'sdlog > 0'."
          )
        }
      }

      # -----------------------------------------------------------------------
      # Uniform distribution
      # -----------------------------------------------------------------------

      if (distribution == "uniform") {

        if (probabilistic$min >= probabilistic$max) {

          stop(
            "Parameter '", name,
            "': uniform distribution requires 'min < max'."
          )
        }
      }

      # -----------------------------------------------------------------------
      # Methodological warnings for cost parameters
      # -----------------------------------------------------------------------

      if (type == "cost") {

        probability_negative <- 0

        if (distribution == "normal") {

          probability_negative <- stats::pnorm(
            0,
            mean = probabilistic$mean,
            sd = probabilistic$sd
          )
        }

        if (distribution == "uniform") {

          probability_negative <- stats::punif(
            0,
            min = probabilistic$min,
            max = probabilistic$max
          )
        }

        if (probability_negative > 0.001) {

          warning(
            "Parameter '", name,
            "': the ", distribution,
            " distribution assigns ",
            round(100 * probability_negative, 3),
            "% probability to negative cost values. ",
            "Consider whether this distribution is appropriate.",
            call. = FALSE
          )
        }
      }

      # -----------------------------------------------------------------------
      # Normalize probabilistic specification
      # -----------------------------------------------------------------------

      probabilistic <- c(
        list(distribution = distribution),
        probabilistic[distribution_arguments]
      )
    }

    # -------------------------------------------------------------------------
    # 3.5. Validate scenario specification
    # -------------------------------------------------------------------------

    if (has_scenarios) {

      if (!is.list(scenarios) || length(scenarios) == 0L) {

        stop(
          "Parameter '", name,
          "': 'scenarios' must be a non-empty named list."
        )
      }

      scenario_names <- names(scenarios)

      if (is.null(scenario_names) ||
          anyNA(scenario_names) ||
          any(!nzchar(scenario_names)) ||
          anyDuplicated(scenario_names)) {

        stop(
          "Parameter '", name,
          "': scenarios must have unique, non-empty names."
        )
      }

      for (scenario_name in scenario_names) {

        value <- scenarios[[scenario_name]]

        validate_scalar(
          value = value,
          field = paste0("scenarios$", scenario_name),
          parameter = name
        )

        if (type == "cost" && value < 0) {

          stop(
            "Parameter '", name,
            "': scenario '", scenario_name,
            "' cannot have a negative unit cost."
          )
        }

        if (type == "effect" && value < -1) {

          stop(
            "Parameter '", name,
            "': scenario '", scenario_name,
            "' cannot represent an effect variation below -1."
          )
        }
      }

      # Preserve scenario names and values
      scenarios <- scenarios[scenario_names]
    }

    # -------------------------------------------------------------------------
    # 3.6. Store normalized parameter
    # -------------------------------------------------------------------------

    normalized_parameter <- list(
      type = type,
      resource = resource,
      comparison = comparison
    )

    if (has_deterministic) {
      normalized_parameter$deterministic <- deterministic
    }

    if (has_probabilistic) {
      normalized_parameter$probabilistic <- probabilistic
    }

    if (has_scenarios) {
      normalized_parameter$scenarios <- scenarios
    }

    parameters[[name]] <- normalized_parameter
  }

  # ---------------------------------------------------------------------------
  # 4. Build structured output
  # ---------------------------------------------------------------------------

  # Generate summary from validated definitions using the
  # existing summary.cea_parameters() implementation.

  parameter_definitions <- parameters

  class(parameter_definitions) <- c(
    "cea_parameters",
    "list"
  )

  parameter_summary <- summary(parameter_definitions)

  # Store summary and definitions as separate components.
  result <- list(
    summary = parameter_summary,
    parameters = parameters
  )

  class(result) <- c(
    "cea_parameters",
    "list"
  )

  return(result)
}
