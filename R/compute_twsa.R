
#' Compute Two-Way Deterministic Sensitivity Analysis (TWSA)
#'
#' Performs a two-way deterministic sensitivity analysis by varying
#' pairs of parameters simultaneously across their deterministic ranges.
#'
#' Supports resource unit costs, proportional changes in incremental
#' effectiveness, multiple datasets, comparisons, subgroups and
#' willingness-to-pay thresholds.
#'
#' @param data A data frame or a named list of data frames.
#' @param group Name of the strategy grouping variable.
#' @param cost Name of the individual-level cost variable.
#' @param effect Name of the effectiveness variable.
#' @param ref Reference strategy.
#' @param parameters A cea_parameters object created with
#'   define_cea_parameters(), or a deterministic parameter data frame
#'   following the same specification as compute_owsa().
#' @param lambda Numeric vector of willingness-to-pay thresholds.
#' @param subgroup_vars Optional character vector of subgroup variables.
#' @param parameter_pairs Optional two-column matrix or data frame
#'   containing parameter names. NULL evaluates all unique pairs.
#' @param n_levels Odd integer specifying the number of deterministic
#'   values evaluated for each parameter. Defaults to 3, corresponding
#'   to lower, baseline and upper values. Higher values generate
#'   equally spaced levels on each side of the baseline.
#'
#' @details
#' Each parameter pair generates n_levels squared combinations.
#' The default n_levels = 3 preserves the conventional nine
#' combinations of baseline, lower and upper values.
#'
#' For higher resolutions, intermediate parameter values are generated
#' separately between lower and baseline, and between baseline and
#' upper. This ensures that the baseline is always included, even
#' when the deterministic limits are asymmetric.
#'
#' Cost adjustments are additive at the individual level:
#' \deqn{C_{i,new}=C_{i,base}+\sum_j R_{ij}(P_{j,new}-P_{j,base})}
#'
#' Effect adjustments are additive relative to baseline incremental
#' effectiveness:
#' \deqn{\Delta E_{new}=\Delta E_{base}(1+\sum_j p_j)}
#'
#' The reference strategy remains unchanged.
#' The analysis is deterministic and does not use bootstrap resampling.
#'
#' @return An object of class twsa_result containing results for each
#'   dataset, a consolidated sensitivity_results table, normalized
#'   parameters and analysis settings.
#'
#' @export
compute_twsa <- function(
    data,
    group,
    cost,
    effect,
    ref,
    parameters,
    lambda = 25000,
    subgroup_vars = NULL,
    parameter_pairs = NULL,
    n_levels = 3L
) {

  # -------------------------------------------------------------------------
  # 1. Normalize parameter definitions
  # -------------------------------------------------------------------------

  if (inherits(parameters, "cea_parameters")) {

    definitions <- parameters$parameters

    if (!is.list(definitions) || length(definitions) == 0L) {
      stop("Invalid 'cea_parameters' object.")
    }

    excluded <- names(definitions)[
      !vapply(
        definitions,
        function(p) !is.null(p$deterministic),
        logical(1)
      )
    ]

    if (length(excluded) > 0L) {
      warning(
        "Parameters without deterministic specifications excluded: ",
        paste(excluded, collapse = ", "),
        call. = FALSE
      )
    }

    parameters <- do.call(
      rbind,
      lapply(names(definitions), function(name) {

        p <- definitions[[name]]

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
  }

  if (!is.data.frame(parameters) || nrow(parameters) < 2L) {
    stop("TWSA requires at least two deterministic parameters.")
  }

  required <- c(
    "parameter", "resource", "base", "lower", "upper"
  )

  if (!all(required %in% names(parameters))) {
    stop("Invalid parameters table.")
  }

  if (!"type" %in% names(parameters)) {
    parameters$type <- "cost"
  }

  if (!"comparison" %in% names(parameters)) {
    parameters$comparison <- NA_character_
  }

  parameters$parameter <- as.character(parameters$parameter)
  parameters$type <- as.character(parameters$type)
  parameters$resource <- as.character(parameters$resource)
  parameters$comparison <- as.character(parameters$comparison)

  if (anyNA(parameters$parameter) ||
      any(!nzchar(parameters$parameter)) ||
      anyDuplicated(parameters$parameter)) {
    stop("Invalid or duplicated parameter identifiers.")
  }

  if (anyNA(parameters$type) ||
      !all(parameters$type %in% c("cost", "effect"))) {
    stop("Invalid parameter types.")
  }

  if (!all(vapply(
    parameters[c("base", "lower", "upper")],
    is.numeric,
    logical(1)
  ))) {
    stop("Parameter values must be numeric.")
  }

  values <- as.matrix(
    parameters[c("base", "lower", "upper")]
  )

  if (any(!is.finite(values)) ||
      any(parameters$lower > parameters$base) ||
      any(parameters$base > parameters$upper)) {
    stop("Invalid parameter limits.")
  }

  cost_rows <- parameters$type == "cost"
  effect_rows <- parameters$type == "effect"

  if (any(cost_rows)) {

    if (anyNA(parameters$resource[cost_rows]) ||
        any(!nzchar(parameters$resource[cost_rows])) ||
        any(parameters$lower[cost_rows] < 0)) {
      stop("Invalid cost parameter specifications.")
    }

    if (any(!is.na(parameters$comparison[cost_rows]))) {
      stop("Cost parameters cannot specify comparisons.")
    }
  }

  if (any(effect_rows)) {

    if (any(parameters$base[effect_rows] != 0) ||
        any(parameters$lower[effect_rows] < -1) ||
        any(!is.na(parameters$resource[effect_rows]))) {
      stop("Invalid effect parameter specifications.")
    }

    comparison_values <- parameters$comparison[effect_rows]
    comparison_values <- comparison_values[
      !is.na(comparison_values)
    ]

    if (any(!nzchar(comparison_values))) {
      stop("Invalid effect comparison identifiers.")
    }
  }

  # -------------------------------------------------------------------------
  # 2. Validate thresholds and arguments
  # -------------------------------------------------------------------------

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

  if (!is.numeric(lambda) ||
      length(lambda) == 0L ||
      any(!is.finite(lambda)) ||
      any(lambda < 0) ||
      anyDuplicated(lambda)) {
    stop("Invalid willingness-to-pay thresholds.")
  }

  nmb_names <- paste0("NMB_", lambda)

  if (anyDuplicated(nmb_names)) {
    stop("Lambda values produce duplicated NMB column names.")
  }

  if (!is.null(subgroup_vars)) {

    if (!is.character(subgroup_vars) ||
        anyNA(subgroup_vars) ||
        any(!nzchar(subgroup_vars)) ||
        anyDuplicated(subgroup_vars)) {
      stop("Invalid subgroup variables.")
    }
  }

  # Validate grid resolution

  if (!is.numeric(n_levels) ||
      length(n_levels) != 1L ||
      is.na(n_levels) ||
      !is.finite(n_levels) ||
      n_levels < 3 ||
      n_levels %% 2 != 1 ||
      n_levels > .Machine$integer.max) {

    stop(
      "'n_levels' must be an odd integer greater than or equal to 3."
    )
  }

  n_levels <- as.integer(n_levels)

  # -------------------------------------------------------------------------
  # 3. Resolve parameter pairs
  # -------------------------------------------------------------------------

  if (is.null(parameter_pairs)) {

    parameter_pairs <- t(
      utils::combn(parameters$parameter, 2L)
    )

  } else {

    if (!is.matrix(parameter_pairs) &&
        !is.data.frame(parameter_pairs)) {
      stop(
        "'parameter_pairs' must be a two-column matrix or data frame."
      )
    }

    if (ncol(parameter_pairs) != 2L ||
        nrow(parameter_pairs) == 0L) {
      stop(
        "'parameter_pairs' must contain two columns and at least one row."
      )
    }

    parameter_pairs <- as.matrix(parameter_pairs)

    if (!is.character(parameter_pairs) ||
        anyNA(parameter_pairs) ||
        any(!nzchar(parameter_pairs))) {
      stop("Parameter pairs must contain valid parameter names.")
    }
  }

  if (!all(parameter_pairs %in% parameters$parameter)) {
    stop("Unknown parameter names in 'parameter_pairs'.")
  }

  if (any(parameter_pairs[, 1] == parameter_pairs[, 2])) {
    stop("A parameter cannot be paired with itself.")
  }

  pair_keys <- apply(
    parameter_pairs,
    1,
    function(x) paste(sort(x), collapse = "\r")
  )

  if (anyDuplicated(pair_keys)) {
    stop("Duplicated parameter pairs.")
  }

  colnames(parameter_pairs) <- c("parameter_1", "parameter_2")

  # -------------------------------------------------------------------------
  # 4. Normalize datasets
  # -------------------------------------------------------------------------

  if (is.data.frame(data)) {

    datasets <- list(Dataset_1 = data)

  } else if (
    is.list(data) &&
    length(data) > 0L &&
    all(vapply(data, is.data.frame, logical(1)))
  ) {

    datasets <- data

  } else {

    stop(
      "'data' must be a data frame or a non-empty list of data frames."
    )
  }

  dataset_names <- names(datasets)

  if (is.null(dataset_names)) {
    dataset_names <- rep("", length(datasets))
  }

  missing_names <- is.na(dataset_names) | !nzchar(dataset_names)

  dataset_names[missing_names] <- paste0(
    "Dataset_", which(missing_names)
  )

  if (anyDuplicated(dataset_names)) {
    stop("Dataset names must be unique.")
  }

  reserved <- c(
    "sensitivity_results", "parameters", "settings"
  )

  if (any(dataset_names %in% reserved)) {
    stop("Dataset names conflict with reserved result components.")
  }

  names(datasets) <- dataset_names

  # -------------------------------------------------------------------------
  # 5. Validate datasets
  # -------------------------------------------------------------------------

  resources <- unique(parameters$resource[cost_rows])

  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    required_vars <- unique(c(
      group, cost, effect, resources, subgroup_vars
    ))

    missing_vars <- setdiff(required_vars, names(df))

    if (length(missing_vars) > 0L) {
      stop(
        "Missing variables in dataset '",
        dataset_name, "': ",
        paste(missing_vars, collapse = ", ")
      )
    }

    numeric_vars <- unique(c(cost, effect, resources))

    if (!all(vapply(
      df[numeric_vars],
      is.numeric,
      logical(1)
    ))) {
      stop("Cost, effect and resource variables must be numeric.")
    }

    if (anyNA(df[unique(c(group, numeric_vars))])) {
      stop("Missing values in required analysis variables.")
    }

    if (any(!vapply(
      df[numeric_vars],
      function(x) all(is.finite(x)),
      logical(1)
    ))) {
      stop("Non-finite numeric values in dataset.")
    }

    if (length(resources) > 0L &&
        any(vapply(
          df[resources],
          function(x) any(x < 0),
          logical(1)
        ))) {
      stop("Resource utilization cannot be negative.")
    }

    groups <- unique(as.character(df[[group]]))

    if (!ref %in% groups) {
      stop("Reference group not found in dataset.")
    }

    alternatives <- setdiff(groups, ref)

    if (length(alternatives) == 0L) {
      stop("At least one alternative strategy is required.")
    }

    targets <- unique(parameters$comparison[effect_rows])
    targets <- targets[!is.na(targets)]

    if (!all(targets %in% alternatives)) {
      stop("Unknown effect comparison in dataset.")
    }
  }

  # -------------------------------------------------------------------------
  # 6. Generate deterministic parameter levels
  # -------------------------------------------------------------------------

  generate_parameter_levels <- function(parameter) {

    lower <- parameter$lower
    base <- parameter$base
    upper <- parameter$upper

    # Preserve original behavior for the conventional 3-level analysis

    if (n_levels == 3L) {

      return(data.frame(
        scenario = c("baseline", "lower", "upper"),
        value = c(base, lower, upper),
        stringsAsFactors = FALSE
      ))
    }

    # Divide the range into two segments around the baseline

    half_levels <- (n_levels - 1L) / 2L

    lower_segment <- seq(
      from = lower,
      to = base,
      length.out = half_levels + 1L
    )

    upper_segment <- seq(
      from = base,
      to = upper,
      length.out = half_levels + 1L
    )

    parameter_values <- c(
      lower_segment,
      upper_segment[-1L]
    )

    # Guarantee exact baseline and boundary values

    baseline_index <- half_levels + 1L

    parameter_values[1L] <- lower
    parameter_values[baseline_index] <- base
    parameter_values[n_levels] <- upper

    # Assign unique scenario identifiers

    scenario_names <- sprintf(
      paste0(
        "level_%0",
        nchar(as.character(n_levels)),
        "d"
      ),
      seq_len(n_levels)
    )

    scenario_names[1L] <- "lower"
    scenario_names[baseline_index] <- "baseline"
    scenario_names[n_levels] <- "upper"

    data.frame(
      scenario = scenario_names,
      value = parameter_values,
      stringsAsFactors = FALSE
    )
  }

  # Precompute levels for every parameter

  parameter_levels <- lapply(
    seq_len(nrow(parameters)),
    function(i) {
      generate_parameter_levels(
        parameters[i, , drop = FALSE]
      )
    }
  )

  names(parameter_levels) <- parameters$parameter

  # -------------------------------------------------------------------------
  # 7. Internal calculation for one analytical stratum
  # -------------------------------------------------------------------------

  compute_stratum <- function(
    df,
    dataset_name,
    subgroup_var,
    subgroup_level
  ) {

    groups <- unique(as.character(df[[group]]))

    if (!ref %in% groups) {
      return(NULL)
    }

    alternatives <- setdiff(groups, ref)

    if (length(alternatives) == 0L) {
      return(NULL)
    }

    baseline_summary <- stats::aggregate(
      df[c(cost, effect)],
      by = list(group = df[[group]]),
      FUN = mean
    )

    reference <- baseline_summary[
      as.character(baseline_summary$group) == ref,
      ,
      drop = FALSE
    ]

    if (nrow(reference) != 1L) {
      return(NULL)
    }

    # Baseline incremental effectiveness for each comparison

    baseline_delta_effect <- stats::setNames(
      baseline_summary[[effect]] - reference[[effect]],
      as.character(baseline_summary$group)
    )

    results <- vector("list", nrow(parameter_pairs))

    for (pair_index in seq_len(nrow(parameter_pairs))) {

      p1_name <- parameter_pairs[pair_index, 1]
      p2_name <- parameter_pairs[pair_index, 2]

      p1 <- parameters[
        parameters$parameter == p1_name,
        ,
        drop = FALSE
      ]

      p2 <- parameters[
        parameters$parameter == p2_name,
        ,
        drop = FALSE
      ]

      # Retrieve the deterministic grid for each parameter

      levels_1 <- parameter_levels[[p1_name]]
      levels_2 <- parameter_levels[[p2_name]]

      # Generate every combination of parameter levels

      combinations <- expand.grid(
        index_1 = seq_len(nrow(levels_1)),
        index_2 = seq_len(nrow(levels_2)),
        KEEP.OUT.ATTRS = FALSE
      )

      pair_results <- vector(
        "list",
        nrow(combinations)
      )

      for (combination_index in seq_len(nrow(combinations))) {

        index_1 <- combinations$index_1[combination_index]
        index_2 <- combinations$index_2[combination_index]

        scenario_1 <- levels_1$scenario[index_1]
        scenario_2 <- levels_2$scenario[index_2]

        value_1 <- levels_1$value[index_1]
        value_2 <- levels_2$value[index_2]

        modified_data <- df

        # Apply both cost adjustments

        for (entry in list(
          list(parameter = p1, value = value_1),
          list(parameter = p2, value = value_2)
        )) {

          parameter <- entry$parameter
          value <- entry$value

          if (parameter$type == "cost") {

            resource <- parameter$resource

            modified_data[[cost]] <-
              modified_data[[cost]] +
              df[[resource]] * (value - parameter$base)
          }
        }

        summary <- stats::aggregate(
          modified_data[c(cost, effect)],
          by = list(group = modified_data[[group]]),
          FUN = mean
        )

        reference_new <- summary[
          as.character(summary$group) == ref,
          ,
          drop = FALSE
        ]

        summary$delta_cost <-
          summary[[cost]] - reference_new[[cost]]

        summary$delta_effect <-
          summary[[effect]] - reference_new[[effect]]

        # Apply both proportional effect variations

        for (entry in list(
          list(parameter = p1, value = value_1),
          list(parameter = p2, value = value_2)
        )) {

          parameter <- entry$parameter
          value <- entry$value

          if (parameter$type == "effect") {

            selected <- as.character(summary$group) != ref

            if (!is.na(parameter$comparison)) {

              selected <- selected &
                as.character(summary$group) ==
                parameter$comparison
            }

            target_groups <- as.character(
              summary$group[selected]
            )

            summary$delta_effect[selected] <-
              summary$delta_effect[selected] +
              baseline_delta_effect[target_groups] * value
          }
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
          as.character(summary$group) != ref,
          ,
          drop = FALSE
        ]

        summary$dataset <- dataset_name
        summary$subgroup_var <- subgroup_var
        summary$subgroup_level <- subgroup_level
        summary$comparison <- as.character(summary$group)

        summary$parameter_1 <- p1_name
        summary$scenario_1 <- scenario_1
        summary$value_1 <- value_1

        summary$parameter_2 <- p2_name
        summary$scenario_2 <- scenario_2
        summary$value_2 <- value_2

        summary$pair_uid <- paste(
          p1_name, p2_name, sep = "__"
        )

        summary$group_uid <- paste(
          dataset_name,
          subgroup_var,
          subgroup_level,
          summary$comparison,
          sep = "_"
        )

        pair_results[[combination_index]] <- summary
      }

      results[[pair_index]] <- dplyr::bind_rows(
        pair_results
      )
    }

    dplyr::bind_rows(results)
  }

  # -------------------------------------------------------------------------
  # 8. Iterate over datasets and subgroups
  # -------------------------------------------------------------------------

  all_results <- list()

  for (dataset_name in names(datasets)) {

    df <- datasets[[dataset_name]]

    all_results[[length(all_results) + 1L]] <-
      compute_stratum(
        df = df,
        dataset_name = dataset_name,
        subgroup_var = "Overall",
        subgroup_level = "Overall"
      )

    for (subgroup_var in subgroup_vars) {

      levels <- unique(df[[subgroup_var]])
      levels <- levels[!is.na(levels)]

      for (level in levels) {

        selected <- !is.na(df[[subgroup_var]]) &
          df[[subgroup_var]] == level

        subset_df <- df[
          selected,
          ,
          drop = FALSE
        ]

        result <- compute_stratum(
          df = subset_df,
          dataset_name = dataset_name,
          subgroup_var = subgroup_var,
          subgroup_level = as.character(level)
        )

        if (!is.null(result)) {
          all_results[[length(all_results) + 1L]] <- result
        }
      }
    }
  }

  all_results <- Filter(
    function(x) !is.null(x) && nrow(x) > 0L,
    all_results
  )

  if (length(all_results) == 0L) {
    stop("No valid comparisons could be computed.")
  }

  sensitivity_results <- dplyr::bind_rows(
    all_results
  )

  # -------------------------------------------------------------------------
  # 9. Standardize column order
  # -------------------------------------------------------------------------

  sensitivity_results <- sensitivity_results |>
    dplyr::relocate(
      dataset,
      subgroup_var,
      subgroup_level,
      comparison,
      group_uid,
      pair_uid,
      parameter_1,
      scenario_1,
      value_1,
      parameter_2,
      scenario_2,
      value_2
    )

  # -------------------------------------------------------------------------
  # 10. Organize results by dataset and subgroup
  # -------------------------------------------------------------------------

  nested_results <- lapply(
    names(datasets),
    function(dataset_name) {

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

        subgroup_results <- lapply(
          subgroup_vars,
          function(subgroup_var) {

            variable_results <- dataset_results[
              dataset_results$subgroup_var == subgroup_var,
              ,
              drop = FALSE
            ]

            subgroup_levels <- unique(
              as.character(variable_results$subgroup_level)
            )

            level_results <- lapply(
              subgroup_levels,
              function(level) {

                variable_results[
                  variable_results$subgroup_level == level,
                  ,
                  drop = FALSE
                ]
              }
            )

            names(level_results) <- subgroup_levels

            level_results
          }
        )

        names(subgroup_results) <- subgroup_vars
      }

      structure(
        list(
          Overall = overall_results,
          Subgroups = subgroup_results
        ),
        class = "twsa_dataset"
      )
    }
  )

  names(nested_results) <- names(datasets)

  # -------------------------------------------------------------------------
  # 11. Return structured results
  # -------------------------------------------------------------------------

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
        datasets = names(datasets),
        parameter_pairs = parameter_pairs,
        n_levels = n_levels,
        effect_combination = "additive"
      )
    )
  )

  class(result) <- c(
    "twsa_result",
    "list"
  )

  return(result)
}
