
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
#' The function supports multiple datasets, intervention strategies,
#' subgroup analyses and willingness-to-pay thresholds.
#'
#' Results are organized hierarchically by dataset, overall population
#' and subgroup, while also providing a consolidated results table.
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
#' @param parameters A data frame containing the parameters to be varied.
#'   The following columns are required:
#'   \itemize{
#'     \item \code{parameter}: parameter identifier.
#'     \item \code{resource}: corresponding resource utilization variable.
#'     \item \code{base}: baseline unit cost.
#'     \item \code{lower}: lower parameter value.
#'     \item \code{upper}: upper parameter value.
#'   }
#'
#' @param lambda Numeric vector of willingness-to-pay thresholds used
#'   to calculate incremental NMBs. Defaults to 25000.
#'
#' @param subgroup_vars Optional character vector specifying subgroup
#'   variables. Overall and subgroup-specific results are calculated.
#'
#' @details
#' Three scenarios are evaluated for each parameter: baseline, lower
#' and upper. Only one parameter is varied at a time.
#'
#' Individual-level costs are recalculated as:
#' \deqn{C_{i,new} = C_{i,base} + R_i(P_{new} - P_{base})}
#'
#' Incremental NMB is calculated separately for each threshold:
#' \deqn{NMB(\lambda) = \lambda \Delta E - \Delta C}
#'
#' NMB columns follow the same naming convention as compute_icers():
#' NMB_15000, NMB_25000, etc.
#'
#' The analysis is deterministic and does not perform bootstrap resampling.
#'
#' @return
#' An object of class \code{owsa_result} containing:
#' \itemize{
#'   \item One element per dataset, with \code{Overall} and
#'         \code{Subgroups} components containing scenario-specific
#'         economic results.
#'   \item \code{sensitivity_results}: consolidated results table.
#'   \item \code{parameters}: parameter specification table.
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
  # 1. Validate parameters
  # ---------------------------------------------------------------------------
  required <- c("parameter", "resource", "base", "lower", "upper")

  if (!is.data.frame(parameters) ||
      !all(required %in% names(parameters)) ||
      nrow(parameters) == 0L) {
    stop("Invalid parameters table.")
  }

  if (!all(vapply(
    parameters[c("base", "lower", "upper")],
    is.numeric,
    logical(1)
  )) ||
  anyNA(parameters[required]) ||
  any(parameters$lower > parameters$base) ||
  any(parameters$base > parameters$upper) ||
  anyDuplicated(parameters$parameter)) {
    stop("Invalid parameter values, limits or identifiers.")
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

  # NMB column names consistent with compute_icers()
  nmb_names <- paste0("NMB_", lambda)

  if (anyDuplicated(nmb_names)) {
    stop("Lambda values produce duplicated NMB column names.")
  }

  if (!is.character(subgroup_vars) &&
      !is.null(subgroup_vars)) {
    stop("'subgroup_vars' must be a character vector.")
  }

  if (anyDuplicated(subgroup_vars)) {
    stop("Duplicated subgroup variables.")
  }

  # ---------------------------------------------------------------------------
  # 2. Normalize datasets
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

  # Prevent collisions with reserved result components
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
  # 3. Validate each dataset
  # ---------------------------------------------------------------------------
  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    required_vars <- unique(c(
      group,
      cost,
      effect,
      parameters$resource,
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
      parameters$resource
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
  }

  # ---------------------------------------------------------------------------
  # 4. Core OWSA calculation
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

        modified_data[[cost]] <- df[[cost]] +
          df[[parameter_row$resource]] *
          (scenarios[[scenario]] - parameter_row$base)

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

        summary$icer <- ifelse(
          summary$delta_effect == 0,
          NA_real_,
          summary$delta_cost / summary$delta_effect
        )

        # Calculate NMB for every willingness-to-pay threshold
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
  # 5. Iterate over datasets and subgroups
  # ---------------------------------------------------------------------------
  all_results <- list()
  result_index <- 0L

  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    # Overall population
    result_index <- result_index + 1L

    all_results[[result_index]] <- compute_stratum(
      df = df,
      dataset_name = dataset_name,
      subgroup_var = "Overall",
      subgroup_level = "Overall"
    )

    # Individual subgroup variables
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
  # 6. Standardize dimensions
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
  # 7. Organize results by dataset and subgroup
  # ---------------------------------------------------------------------------
  nested_results <- lapply(names(datasets), function(dataset_name) {

    dataset_results <- sensitivity_results[
      sensitivity_results$dataset == dataset_name,
      ,
      drop = FALSE
    ]

    # Overall population
    overall_results <- dataset_results[
      dataset_results$subgroup_var == "Overall" &
        dataset_results$subgroup_level == "Overall",
      ,
      drop = FALSE
    ]

    # Subgroup-specific results
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
  # 8. Return structured results
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
