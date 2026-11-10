#' Compute One-Way Deterministic Sensitivity Analysis (OWSA)
#'
#' Performs a one-way deterministic sensitivity analysis of incremental
#' costs, incremental effects, Incremental Cost-Effectiveness Ratios (ICERs),
#' and incremental Net Monetary Benefits (NMBs).
#'
#' The function supports variations in resource unit costs and proportional
#' variations in incremental effectiveness.
#'
#' Cost parameters modify individual-level costs according to resource
#' utilization. Effect parameters modify the incremental effect relative
#' to the reference strategy, without modifying the reference effect.
#'
#' Each parameter is varied independently while all other parameters
#' remain at their baseline values.
#'
#' Multiple datasets, intervention strategies, subgroup analyses and
#' willingness-to-pay thresholds are supported.
#'
#' @param data A data frame or a named list of data frames containing
#'   individual-level costs, effects, resource utilization and strategies.
#'
#' @param group Character string specifying the strategy grouping variable.
#'
#' @param cost Character string specifying the individual-level cost variable.
#'
#' @param effect Character string specifying the effectiveness variable.
#'
#' @param ref Character string identifying the reference strategy.
#'
#' @param parameters Either:
#'   \itemize{
#'     \item An object of class \code{cea_parameters}, created using
#'       \code{define_cea_parameters()}.
#'     \item A data frame specifying the sensitivity parameters.
#'   }
#'
#'   For data frames, required columns are:
#'   \itemize{
#'     \item \code{parameter}: unique parameter identifier.
#'     \item \code{resource}: resource utilization variable for cost
#'       parameters; \code{NA} for effect parameters.
#'     \item \code{base}: baseline unit cost for cost parameters;
#'       zero for effect parameters.
#'     \item \code{lower}: lower unit cost or proportional effect variation.
#'     \item \code{upper}: upper unit cost or proportional effect variation.
#'   }
#'
#'   Optional columns:
#'   \itemize{
#'     \item \code{type}: \code{"cost"} or \code{"effect"}.
#'       Defaults to \code{"cost"} for backward compatibility.
#'     \item \code{comparison}: target alternative strategy for effect
#'       parameters. \code{NA} applies the same proportional variation
#'       to each alternative's own incremental effect.
#'       Not applicable to cost parameters.
#'   }
#'
#'   When \code{parameters} is a \code{cea_parameters} object, its
#'   deterministic specifications are automatically converted into
#'   the internal parameter table. Parameters without deterministic
#'   specifications are excluded from OWSA.
#'
#' @param lambda Numeric vector of willingness-to-pay thresholds.
#'   Defaults to 25000.
#'
#' @param subgroup_vars Optional character vector of subgroup variables.
#'
#' @details
#' Three scenarios are evaluated independently for each parameter:
#' \code{baseline}, \code{lower} and \code{upper}.
#'
#' For cost parameters, individual-level costs are recalculated as:
#' \deqn{C_{i,new} = C_{i,base} + R_i(P_{new} - P_{base})}
#'
#' For effect parameters, incremental effectiveness is recalculated as:
#' \deqn{\Delta E_{new} = \Delta E_{base}(1+p)}
#'
#' Here, \eqn{p} is the proportional variation. For example,
#' \code{-0.20} represents a 20 percent reduction and \code{0.20}
#' represents a 20 percent increase.
#'
#' Effect parameters must have \code{base = 0}, and their lower
#' variation must be greater than or equal to \code{-1}.
#' The reference strategy remains unchanged.
#'
#' When \code{comparison} identifies a particular strategy, only its
#' incremental effect is modified. Other comparisons retain their
#' baseline incremental effects.
#'
#' Incremental NMB is calculated as:
#' \deqn{NMB(\lambda) = \lambda \Delta E - \Delta C}
#'
#' NMB columns follow the naming convention used by
#' \code{compute_icers()}, such as \code{NMB_25000}.
#'
#' The analysis is deterministic and does not perform bootstrap resampling.
#'
#' @return
#' An object of class \code{owsa_result} containing:
#' \itemize{
#'   \item One element per dataset with \code{Overall} and
#'     \code{Subgroups} results.
#'   \item \code{sensitivity_results}: consolidated results table.
#'   \item \code{parameters}: normalized parameter specification table.
#'   \item \code{settings}: analysis settings and metadata.
#' }
#'
#' @examples
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
#'   parameters = owsa_parameters,
#'   lambda     = c(15000, 25000, 35000)
#' )
#'
#' @export
compute_owsa <- function(
    data,
    group,
    cost,
    effect,
    ref,
    parameters,
    lambda = 25000,
    subgroup_vars = NULL
) {


  # ---------------------------------------------------------------------------
  # 1. Convert cea_parameters objects to the internal OWSA parameter table
  # ---------------------------------------------------------------------------
  if (inherits(parameters, "cea_parameters")) {

    parameter_names <- names(parameters)

    # Identify parameters without deterministic specifications
    excluded_parameters <- parameter_names[
      vapply(
        parameters,
        function(p) is.null(p$deterministic),
        logical(1)
      )
    ]

    # Inform the user about excluded parameters
    if (length(excluded_parameters) > 0L) {

      warning(
        "The following parameters were excluded from OWSA ",
        "because they have no deterministic specification: ",
        paste(excluded_parameters, collapse = ", "),
        call. = FALSE
      )
    }

    # Convert eligible parameters to the internal data.frame
    parameters <- do.call(
      rbind,
      lapply(parameter_names, function(name) {

        p <- parameters[[name]]

        if (is.null(p$deterministic)) {
          return(NULL)
        }

        data.frame(
          parameter = name,
          type = p$type,
          resource = p$resource,
          comparison = p$comparison,
          base = p$deterministic$base,
          lower = p$deterministic$lower,
          upper = p$deterministic$upper,
          stringsAsFactors = FALSE
        )
      })
    )

    if (is.null(parameters) || nrow(parameters) == 0L) {
      stop(
        "No parameters with deterministic specifications ",
        "are available for OWSA."
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 2. Validate and normalize parameters
  # ---------------------------------------------------------------------------
  required <- c("parameter", "resource", "base", "lower", "upper")

  if (!is.data.frame(parameters) ||
      !all(required %in% names(parameters)) ||
      nrow(parameters) == 0L) {
    stop("Invalid parameters table.")
  }

  if (!"type" %in% names(parameters)) {
    parameters$type <- "cost"
  }

  if (!"comparison" %in% names(parameters)) {
    parameters$comparison <- NA_character_
  }

  parameters$type <- as.character(parameters$type)
  parameters$comparison <- as.character(parameters$comparison)
  parameters$resource <- as.character(parameters$resource)

  if (!is.character(parameters$parameter) &&
      !is.factor(parameters$parameter)) {
    stop("'parameter' must contain character identifiers.")
  }

  parameters$parameter <- as.character(parameters$parameter)

  if (anyNA(parameters$parameter) ||
      any(!nzchar(parameters$parameter)) ||
      anyDuplicated(parameters$parameter)) {
    stop("Invalid or duplicated parameter identifiers.")
  }

  if (anyNA(parameters$type) ||
      !all(parameters$type %in% c("cost", "effect"))) {
    stop("'type' must be 'cost' or 'effect'.")
  }

  if (!all(vapply(
    parameters[c("base", "lower", "upper")],
    is.numeric,
    logical(1)
  ))) {
    stop("Parameter values must be numeric.")
  }

  parameter_values <- as.matrix(
    parameters[c("base", "lower", "upper")]
  )

  if (any(!is.finite(parameter_values)) ||
      any(parameters$lower > parameters$base) ||
      any(parameters$base > parameters$upper)) {
    stop("Invalid parameter values or limits.")
  }

  cost_rows <- parameters$type == "cost"
  effect_rows <- parameters$type == "effect"

  if (any(cost_rows)) {

    if (anyNA(parameters$resource[cost_rows]) ||
        any(!nzchar(parameters$resource[cost_rows]))) {
      stop("Cost parameters require a valid resource variable.")
    }

    if (any(!is.na(parameters$comparison[cost_rows]))) {
      stop(
        "'comparison' is only applicable to effect parameters."
      )
    }
  }

  if (any(effect_rows)) {

    if (any(parameters$base[effect_rows] != 0)) {
      stop("Effect parameters must have 'base = 0'.")
    }

    if (any(parameters$lower[effect_rows] < -1)) {
      stop(
        "Effect variations cannot be lower than -1 ",
        "(a 100 percent reduction)."
      )
    }

    if (any(!is.na(parameters$resource[effect_rows]))) {
      stop("Effect parameters must have 'resource = NA'.")
    }

    specified_comparisons <- parameters$comparison[effect_rows]
    specified_comparisons <- specified_comparisons[
      !is.na(specified_comparisons)
    ]

    if (any(!nzchar(specified_comparisons))) {
      stop("Effect comparison identifiers cannot be empty.")
    }
  }

  # Validate willingness-to-pay thresholds
  if (!is.numeric(lambda) ||
      length(lambda) == 0L ||
      any(!is.finite(lambda)) ||
      any(lambda < 0) ||
      anyDuplicated(lambda)) {
    stop(
      "'lambda' must be a non-empty numeric vector ",
      "of unique, finite, non-negative thresholds."
    )
  }

  nmb_names <- paste0("NMB_", lambda)

  if (anyDuplicated(nmb_names)) {
    stop("Lambda values produce duplicated NMB column names.")
  }

  if (!is.null(subgroup_vars) &&
      !is.character(subgroup_vars)) {
    stop("'subgroup_vars' must be a character vector.")
  }

  if (anyDuplicated(subgroup_vars)) {
    stop("Duplicated subgroup variables.")
  }

  # ---------------------------------------------------------------------------
  # 3. Normalize datasets
  # ---------------------------------------------------------------------------
  if (is.data.frame(data)) {

    datasets <- list(Dataset_1 = data)

  } else if (is.list(data) &&
             length(data) > 0L &&
             all(vapply(data, is.data.frame, logical(1)))) {

    datasets <- data

  } else {

    stop("'data' must be a data.frame or a list of data.frames.")
  }

  dataset_names <- names(datasets)

  if (is.null(dataset_names)) {
    dataset_names <- rep("", length(datasets))
  }

  missing_names <- is.na(dataset_names) | !nzchar(dataset_names)

  dataset_names[missing_names] <- paste0(
    "Dataset_",
    which(missing_names)
  )

  if (anyDuplicated(dataset_names)) {
    stop("Dataset names must be unique.")
  }

  reserved_names <- c(
    "sensitivity_results",
    "parameters",
    "settings"
  )

  if (any(dataset_names %in% reserved_names)) {
    stop(
      "Dataset names cannot match reserved result components: ",
      paste(reserved_names, collapse = ", ")
    )
  }

  names(datasets) <- dataset_names

  # ---------------------------------------------------------------------------
  # 4. Validate each dataset
  # ---------------------------------------------------------------------------
  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    cost_resources <- parameters$resource[cost_rows]

    required_vars <- unique(c(
      group,
      cost,
      effect,
      cost_resources,
      subgroup_vars
    ))

    if (!all(required_vars %in% names(df))) {
      stop(
        "Missing variables in dataset: ",
        dataset_name
      )
    }

    numeric_vars <- unique(c(
      cost,
      effect,
      cost_resources
    ))

    if (!all(vapply(
      df[numeric_vars],
      is.numeric,
      logical(1)
    ))) {
      stop(
        "Cost, effect and resource variables must be numeric in: ",
        dataset_name
      )
    }

    if (anyNA(df[unique(c(group, numeric_vars))])) {
      stop(
        "Missing values in required analysis variables: ",
        dataset_name
      )
    }

    if (!ref %in% df[[group]]) {
      stop(
        "Reference group not found in dataset: ",
        dataset_name
      )
    }

    alternatives <- unique(as.character(df[[group]]))
    alternatives <- alternatives[alternatives != ref]

    specified_comparisons <- unique(
      parameters$comparison[effect_rows]
    )

    specified_comparisons <- specified_comparisons[
      !is.na(specified_comparisons)
    ]

    if (!all(specified_comparisons %in% alternatives)) {
      stop(
        "Unknown effect comparison in dataset: ",
        dataset_name
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Core OWSA calculation
  # ---------------------------------------------------------------------------
  compute_stratum <- function(
    df,
    dataset_name,
    subgroup_var,
    subgroup_level
  ) {

    if (!ref %in% df[[group]]) {
      return(NULL)
    }

    alternatives <- unique(as.character(df[[group]]))
    alternatives <- alternatives[alternatives != ref]

    if (length(alternatives) == 0L) {
      return(NULL)
    }

    results <- lapply(seq_len(nrow(parameters)), function(i) {

      parameter_row <- parameters[i, , drop = FALSE]

      scenarios <- c(
        baseline = parameter_row$base,
        lower = parameter_row$lower,
        upper = parameter_row$upper
      )

      lapply(names(scenarios), function(scenario) {

        modified_data <- df

        # Cost parameters: adjust individual-level costs
        if (parameter_row$type == "cost") {

          modified_data[[cost]] <- df[[cost]] +
            df[[parameter_row$resource]] *
            (scenarios[[scenario]] - parameter_row$base)
        }

        summary <- stats::aggregate(
          modified_data[c(cost, effect)],
          by = list(group = modified_data[[group]]),
          FUN = mean
        )

        reference <- summary[
          summary$group == ref,
          ,
          drop = FALSE
        ]

        summary$delta_cost <-
          summary[[cost]] - reference[[cost]]

        summary$delta_effect <-
          summary[[effect]] - reference[[effect]]

        # Effect parameters: adjust incremental effectiveness
        if (parameter_row$type == "effect") {

          target <- parameter_row$comparison

          selected <- summary$group != ref

          if (!is.na(target)) {
            selected <- selected &
              as.character(summary$group) == target
          }

          summary$delta_effect[selected] <-
            summary$delta_effect[selected] *
            (1 + scenarios[[scenario]])
        }

        summary$icer <- ifelse(
          summary$delta_effect == 0,
          NA_real_,
          summary$delta_cost / summary$delta_effect
        )

        for (j in seq_along(lambda)) {

          summary[[nmb_names[j]]] <-
            lambda[j] * summary$delta_effect -
            summary$delta_cost
        }

        summary <- summary[
          summary$group != ref,
          ,
          drop = FALSE
        ]

        if (nrow(summary) == 0L) {
          return(NULL)
        }

        summary$dataset <- dataset_name
        summary$subgroup_var <- subgroup_var
        summary$subgroup_level <- subgroup_level

        summary$comparison <- as.character(summary$group)

        summary$parameter <- parameter_row$parameter
        summary$scenario <- scenario
        summary$parameter_value <- scenarios[[scenario]]

        summary$group_uid <- paste(
          dataset_name,
          subgroup_var,
          subgroup_level,
          summary$comparison,
          sep = "_"
        )

        summary
      })
    })

    do.call(
      rbind,
      unlist(results, recursive = FALSE)
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Iterate over datasets and subgroups
  # ---------------------------------------------------------------------------
  all_results <- list()
  result_index <- 0L

  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    result_index <- result_index + 1L

    all_results[[result_index]] <- compute_stratum(
      df = df,
      dataset_name = dataset_name,
      subgroup_var = "Overall",
      subgroup_level = "Overall"
    )

    for (subgroup_var in subgroup_vars) {

      levels <- unique(df[[subgroup_var]])
      levels <- levels[!is.na(levels)]

      for (level in levels) {

        subset_df <- df[
          !is.na(df[[subgroup_var]]) &
            df[[subgroup_var]] == level,
          ,
          drop = FALSE
        ]

        result_index <- result_index + 1L

        all_results[[result_index]] <- compute_stratum(
          df = subset_df,
          dataset_name = dataset_name,
          subgroup_var = subgroup_var,
          subgroup_level = as.character(level)
        )
      }
    }
  }

  all_results <- Filter(Negate(is.null), all_results)

  if (length(all_results) == 0L) {
    stop("No valid comparisons could be computed.")
  }

  sensitivity_results <- dplyr::bind_rows(all_results)

  # ---------------------------------------------------------------------------
  # 7. Standardize dimensions
  # ---------------------------------------------------------------------------
  sensitivity_results <- sensitivity_results |>
    dplyr::relocate(
      dataset,
      subgroup_var,
      subgroup_level,
      comparison,
      group_uid,
      parameter,
      scenario,
      parameter_value
    )

  # ---------------------------------------------------------------------------
  # 8. Organize results by dataset and subgroup
  # ---------------------------------------------------------------------------
  nested_results <- lapply(names(datasets), function(dataset_name) {

    dataset_results <- sensitivity_results[
      sensitivity_results$dataset == dataset_name,
      ,
      drop = FALSE
    ]

    overall_results <- dataset_results[
      dataset_results$subgroup_var == "Overall" &
        dataset_results$subgroup_level == "Overall",
      ,
      drop = FALSE
    ]

    subgroup_results <- NULL

    if (!is.null(subgroup_vars)) {

      subgroup_results <- lapply(subgroup_vars, function(subgroup_var) {

        variable_results <- dataset_results[
          dataset_results$subgroup_var == subgroup_var,
          ,
          drop = FALSE
        ]

        subgroup_levels <- unique(
          as.character(variable_results$subgroup_level)
        )

        level_results <- lapply(subgroup_levels, function(level) {

          variable_results[
            variable_results$subgroup_level == level,
            ,
            drop = FALSE
          ]
        })

        names(level_results) <- subgroup_levels

        level_results
      })

      names(subgroup_results) <- subgroup_vars
    }

    structure(
      list(
        Overall = overall_results,
        Subgroups = subgroup_results
      ),
      class = "owsa_dataset"
    )
  })

  names(nested_results) <- names(datasets)

  # ---------------------------------------------------------------------------
  # 9. Return structured results
  # ---------------------------------------------------------------------------
  result <- c(
    nested_results,
    list(
      sensitivity_results = sensitivity_results,
      parameters = parameters,
      settings = list(
        group = group,
        cost = cost,
        effect = effect,
        ref = ref,
        lambda = lambda,
        subgroup_vars = subgroup_vars,
        datasets = names(datasets)
      )
    )
  )

  class(result) <- c("owsa_result", "list")

  return(result)
}
