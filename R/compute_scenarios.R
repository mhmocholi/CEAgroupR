
#' Compute Cost-Effectiveness Scenarios
#'
#' Generates alternative datasets by applying scenario-specific
#' changes to resource unit costs and incremental effectiveness.
#'
#' Parameters sharing a scenario name are applied jointly.
#' The original datasets are preserved.
#'
#' @param data A data frame or a named list of data frames.
#' @param group Name of the treatment group variable.
#' @param cost Name of the total cost variable.
#' @param effect Name of the effectiveness variable.
#' @param ref Reference treatment group.
#' @param parameters An object created by define_cea_parameters().
#'
#' @return A list containing:
#' \describe{
#'   \item{datasets}{Named list of baseline and scenario datasets.}
#'   \item{summary}{Data frame documenting parameter changes.}
#' }
#'
#' @export
compute_scenarios <- function(
    data,
    group,
    cost,
    effect,
    ref,
    parameters
) {

  # ---------------------------------------------------------------------------
  # 1. Validate arguments
  # ---------------------------------------------------------------------------

  if (!inherits(parameters, "cea_parameters") ||
      is.null(parameters$parameters)) {
    stop(
      "'parameters' must be a valid 'cea_parameters' object."
    )
  }

  definitions <- parameters$parameters

  scenario_parameters <- definitions[
    vapply(
      definitions,
      function(p) !is.null(p$scenarios),
      logical(1)
    )
  ]

  if (length(scenario_parameters) == 0L) {
    stop("No scenario specifications were found.")
  }

  for (argument in c("group", "cost", "effect", "ref")) {

    value <- get(argument)

    if (!is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)) {

      stop(
        "'", argument,
        "' must be a non-empty character string."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Normalize datasets
  # ---------------------------------------------------------------------------

  if (is.data.frame(data)) {

    datasets <- list(Dataset_1 = data)

  } else if (
    is.list(data) &&
    length(data) > 0L &&
    all(vapply(data, is.data.frame, logical(1)))
  ) {

    datasets <- data

    dataset_names <- names(datasets)

    if (is.null(dataset_names) ||
        anyNA(dataset_names) ||
        any(!nzchar(dataset_names)) ||
        anyDuplicated(dataset_names)) {

      names(datasets) <- paste0(
        "Dataset_", seq_along(datasets)
      )
    }

  } else {

    stop(
      "'data' must be a data frame or a non-empty list of data frames."
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Identify scenarios
  # ---------------------------------------------------------------------------

  scenario_names <- unique(
    unlist(
      lapply(
        scenario_parameters,
        function(p) names(p$scenarios)
      ),
      use.names = FALSE
    )
  )

  if ("base_case" %in% scenario_names) {
    stop(
      "'base_case' is reserved for the baseline dataset."
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Validate datasets and parameter variables
  # ---------------------------------------------------------------------------

  required_variables <- c(group, cost, effect)

  cost_parameters <- scenario_parameters[
    vapply(
      scenario_parameters,
      function(p) identical(p$type, "cost"),
      logical(1)
    )
  ]

  resource_variables <- vapply(
    cost_parameters,
    function(p) p$resource,
    character(1)
  )

  required_variables <- unique(
    c(required_variables, resource_variables)
  )

  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    missing_variables <- setdiff(
      required_variables,
      names(df)
    )

    if (length(missing_variables) > 0L) {

      stop(
        "Dataset '", dataset_name,
        "' is missing variables: ",
        paste(missing_variables, collapse = ", ")
      )
    }

    if (!ref %in% as.character(df[[group]])) {

      stop(
        "Reference group '", ref,
        "' was not found in dataset '",
        dataset_name, "'."
      )
    }

    numeric_variables <- unique(
      c(cost, effect, resource_variables)
    )

    for (variable in numeric_variables) {

      values <- df[[variable]]

      if (!is.numeric(values) ||
          any(!is.finite(values))) {

        stop(
          "Variable '", variable,
          "' in dataset '", dataset_name,
          "' must contain finite numeric values."
        )
      }
    }

    if (anyNA(df[[group]])) {
      stop(
        "Grouping variable contains missing values in dataset '",
        dataset_name, "'."
      )
    }

    if (length(resource_variables) > 0L) {

      if (any(
        vapply(
          df[resource_variables],
          function(x) any(x < 0),
          logical(1)
        )
      )) {

        stop(
          "Resource utilization cannot contain negative values."
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Generate scenario datasets
  # ---------------------------------------------------------------------------

  output_datasets <- list()
  summary_records <- list()

  for (dataset_name in names(datasets)) {

    original <- datasets[[dataset_name]]

    # Preserve baseline dataset
    output_datasets[[paste0(dataset_name, "_base_case")]] <- original

    reference_mean <- mean(
      original[[effect]][
        as.character(original[[group]]) == ref
      ]
    )

    group_means <- tapply(
      original[[effect]],
      as.character(original[[group]]),
      mean
    )

    for (scenario_name in scenario_names) {

      scenario_data <- original

      for (parameter_name in names(scenario_parameters)) {

        parameter <- scenario_parameters[[parameter_name]]

        if (!scenario_name %in% names(parameter$scenarios)) {
          next
        }

        scenario_value <- parameter$scenarios[[scenario_name]]
        baseline_value <- parameter$base

        # ---------------------------------------------------------------------
        # 5.1. Cost parameters
        # ---------------------------------------------------------------------

        if (parameter$type == "cost") {

          resource <- parameter$resource

          scenario_data[[cost]] <-
            scenario_data[[cost]] +
            original[[resource]] *
            (scenario_value - baseline_value)

        }

        # ---------------------------------------------------------------------
        # 5.2. Effect parameters
        # ---------------------------------------------------------------------

        if (parameter$type == "effect") {

          target_groups <- if (
            is.na(parameter$comparison)
          ) {

            setdiff(
              names(group_means),
              ref
            )

          } else {

            parameter$comparison
          }

          for (target in target_groups) {

            if (!target %in% names(group_means)) {

              stop(
                "Comparison group '", target,
                "' was not found in dataset '",
                dataset_name, "'."
              )
            }

            incremental_effect <-
              group_means[[target]] - reference_mean

            adjustment <-
              incremental_effect * scenario_value

            selected <- (
              as.character(scenario_data[[group]]) == target
            )

            scenario_data[[effect]][selected] <-
              scenario_data[[effect]][selected] +
              adjustment
          }
        }

        # ---------------------------------------------------------------------
        # 5.3. Record parameter changes
        # ---------------------------------------------------------------------

        summary_records[[length(summary_records) + 1L]] <-
          data.frame(
            dataset = dataset_name,
            scenario = scenario_name,
            parameter = parameter_name,
            type = parameter$type,
            resource = parameter$resource,
            comparison = parameter$comparison,
            base = baseline_value,
            scenario_value = scenario_value,
            stringsAsFactors = FALSE
          )
      }

      output_name <- paste0(
        dataset_name, "_", scenario_name
      )

      output_datasets[[output_name]] <- scenario_data
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Build output
  # ---------------------------------------------------------------------------

  scenario_summary <- do.call(
    rbind,
    summary_records
  )

  rownames(scenario_summary) <- NULL

  result <- list(
    datasets = output_datasets,
    summary = scenario_summary
  )

  class(result) <- c(
    "cea_scenarios",
    "list"
  )

  return(result)
}
