
#' Probabilistic Sensitivity Analysis (PSA)
#'
#' Performs Monte Carlo simulations for cost-effectiveness
#' and cost-utility analyses.
#'
#' Probabilistic model parameters are sampled from their specified
#' distributions, and the resulting uncertainty is propagated
#' through incremental costs, effects, cost-effectiveness ratios,
#' and net monetary benefits.
#'
#' @param data A data frame or named list of data frames.
#'   The current implementation supports one dataset.
#' @param group Character string identifying the variable containing
#'   intervention strategies.
#' @param cost Character string identifying individual total costs.
#' @param effect Character string identifying individual effectiveness
#'   outcomes.
#' @param parameters A `cea_parameters` object created by
#'   [define_cea_parameters()], containing probabilistic
#'   parameter specifications.
#' @param R Positive integer specifying the number of Monte Carlo
#'   iterations. Defaults to 1000.
#' @param lambda Numeric vector of willingness-to-pay thresholds.
#'   Defaults to 25000.
#' @param subgroup_vars Optional subgroup variables.
#'   Subgroup analysis is not yet supported.
#' @param seed Optional random seed for reproducibility.
#'   If `NULL`, a seed is generated from the system time.
#' @param verbose Logical. If `TRUE`, displays a completion message.
#' @param ref_group Character string identifying the reference strategy.
#' @param alt_groups Optional character vector identifying alternative
#'   strategies. If `NULL`, all non-reference strategies are included.
#'
#' @details
#' Probabilistic parameters are sampled independently according to
#' the distributions specified in `parameters`. Supported
#' distributions are normal, beta, gamma, lognormal, and uniform.
#'
#' Cost uncertainty is incorporated by adjusting individual costs
#' according to the difference between simulated and baseline
#' resource unit costs.
#'
#' Effect uncertainty is incorporated through multiplicative
#' adjustments to incremental effectiveness.
#'
#' In addition to the economic results, the function produces
#' numerical diagnostics comparing the theoretical distributions
#' with their Monte Carlo realizations.
#'
#' For each probabilistic parameter, the diagnostics include
#' theoretical and simulated means, medians, standard deviations,
#' and 2.5th and 97.5th percentiles.
#'
#' Differences are calculated as simulated minus theoretical
#' values. These diagnostics are descriptive and do not constitute
#' formal goodness-of-fit tests.
#'
#' The diagnostic object also retains the original distribution
#' specifications and simulated parameter samples as attributes,
#' allowing graphical diagnostics through an S3 plot method.
#'
#' The current implementation assumes independence between
#' probabilistic parameters.
#'
#' @return
#' An object of class `cea_results_list` containing:
#'
#' \describe{
#'   \item{Dataset results}{
#'     A dataset-specific `cea_results` object containing
#'     `Overall` results for each alternative strategy.
#'     Each comparison is represented by a `cea_psa` object
#'     containing its Monte Carlo economic outcomes.
#'   }
#'   \item{combined_replicates}{
#'     A tibble containing the economic results for every
#'     simulation and comparison, including incremental costs,
#'     incremental effects, ICERs, mean costs, mean effects,
#'     and net monetary benefits.
#'   }
#'   \item{parameter_samples}{
#'     A tibble containing the simulated values of each
#'     probabilistic parameter, with one row per iteration.
#'   }
#'   \item{parameter_diagnostics}{
#'     A tibble containing theoretical and simulated distribution
#'     statistics, together with their differences, for each
#'     probabilistic parameter.
#'
#'     The object has class `cea_parameter_diagnostics` and
#'     retains the original distribution specifications and
#'     simulated parameter samples as attributes.
#'   }
#'   \item{settings}{
#'     A list containing the analysis configuration, random seed,
#'     number of simulations, willingness-to-pay thresholds,
#'     and original probabilistic distribution specifications.
#'   }
#' }
#'
#' @export
compute_psa <- function(
    data, group, cost, effect,
    parameters,
    R = 1000,
    lambda = 25000,
    subgroup_vars = NULL,
    seed = NULL,
    verbose = FALSE,
    ref_group,
    alt_groups = NULL
) {

  # ============================================================
  # 0. Seed + Settings
  # ============================================================

  if (is.null(seed)) {
    seed <- as.integer(Sys.time()) %% 1e6
  }

  set.seed(seed)

  settings_env <- list(
    ref_group     = ref_group,
    alt_groups    = alt_groups,
    lambda        = lambda,
    R             = R,
    subgroup_vars = subgroup_vars,
    seed          = seed,
    computed_by   = "psa"
  )

  # ============================================================
  # 1. Normalize Input
  # ============================================================

  if (is.data.frame(data)) {

    data_list <- list(single_dataset = data)

  } else {

    if (!is.list(data) ||
        !all(vapply(data, is.data.frame, logical(1)))) {
      stop("'data' must be a data frame or list of data frames.")
    }

    if (is.null(names(data))) {
      names(data) <- paste0("dataset_", seq_along(data))
    }

    data_list <- data
  }

  # Initial version: one dataset, no subgroup analysis
  if (length(data_list) != 1L) {
    stop("This initial PSA version supports one dataset.")
  }

  if (!is.null(subgroup_vars)) {
    stop("Subgroup analysis will be implemented in the next version.")
  }

  if (length(R) != 1L || !is.numeric(R) ||
      is.na(R) || !is.finite(R) ||
      R < 1 || R != as.integer(R)) {
    stop("'R' must be a positive integer.")
  }

  if (!is.numeric(lambda) || length(lambda) == 0L ||
      any(!is.finite(lambda))) {
    stop("'lambda' must contain finite numeric thresholds.")
  }

  # ============================================================
  # 2. Validate parameters and dataset
  # ============================================================

  if (!inherits(parameters, "cea_parameters")) {
    stop(
      "'parameters' must be an object created by ",
      "define_cea_parameters()."
    )
  }

  df <- data_list[[1]]
  dataset_name <- names(data_list)[1]

  required_cols <- c(group, cost, effect)

  if (!all(required_cols %in% names(df))) {
    stop("Required group, cost or effect variable not found.")
  }

  if (anyNA(df[, required_cols, drop = FALSE])) {
    stop("Missing values in group, cost or effect are not supported.")
  }

  if (!is.numeric(df[[cost]]) ||
      !is.numeric(df[[effect]]) ||
      any(!is.finite(df[[cost]])) ||
      any(!is.finite(df[[effect]]))) {
    stop("Cost and effect variables must contain finite numeric values.")
  }

  groups <- unique(as.character(df[[group]]))

  if (!ref_group %in% groups) {
    stop("Reference group not found in dataset.")
  }

  alts <- if (is.null(alt_groups)) {
    setdiff(groups, ref_group)
  } else {
    intersect(groups, alt_groups)
  }

  if (length(alts) == 0L) {
    stop("No alternative strategies available.")
  }

  # ============================================================
  # 3. Generate probabilistic parameter samples
  # ============================================================

  draw_parameter <- function(spec, R) {

    switch(
      spec$distribution,

      normal = stats::rnorm(
        R,
        mean = spec$mean,
        sd = spec$sd
      ),

      beta = stats::rbeta(
        R,
        shape1 = spec$shape1,
        shape2 = spec$shape2
      ),

      gamma = stats::rgamma(
        R,
        shape = spec$shape,
        rate = spec$rate
      ),

      lognormal = stats::rlnorm(
        R,
        meanlog = spec$meanlog,
        sdlog = spec$sdlog
      ),

      uniform = stats::runif(
        R,
        min = spec$min,
        max = spec$max
      ),

      stop(
        "Unsupported probability distribution: ",
        spec$distribution
      )
    )
  }

  prob_params <- Filter(
    function(p) !is.null(p$probabilistic),
    parameters$parameters
  )

  if (length(prob_params) == 0L) {
    stop("No probabilistic parameters were defined.")
  }

  parameter_samples <- data.frame(
    replicate = seq_len(R)
  )

  for (nm in names(prob_params)) {

    parameter_samples[[nm]] <- draw_parameter(
      prob_params[[nm]]$probabilistic,
      R
    )
  }

  # ============================================================
  # 3A. Numerical diagnostics of parameter distributions
  # ============================================================

  diagnose_parameter_distribution <- function(
    parameter_name, spec, samples
  ) {

    distribution <- spec$distribution

    # Theoretical mean and standard deviation
    theoretical_moments <- switch(
      distribution,

      normal = c(
        mean = spec$mean,
        sd = spec$sd
      ),

      beta = c(
        mean = spec$shape1 /
          (spec$shape1 + spec$shape2),
        sd = sqrt(
          (spec$shape1 * spec$shape2) /
            (
              (spec$shape1 + spec$shape2)^2 *
                (spec$shape1 + spec$shape2 + 1)
            )
        )
      ),

      gamma = c(
        mean = spec$shape / spec$rate,
        sd = sqrt(spec$shape) / spec$rate
      ),

      lognormal = c(
        mean = exp(
          spec$meanlog + spec$sdlog^2 / 2
        ),
        sd = sqrt(
          (exp(spec$sdlog^2) - 1) *
            exp(2 * spec$meanlog + spec$sdlog^2)
        )
      ),

      uniform = c(
        mean = (spec$min + spec$max) / 2,
        sd = (spec$max - spec$min) / sqrt(12)
      ),

      stop(
        "Unsupported probability distribution: ",
        distribution
      )
    )

    # Theoretical quantiles
    probabilities <- c(0.025, 0.5, 0.975)

    theoretical_quantiles <- switch(
      distribution,

      normal = stats::qnorm(
        probabilities,
        mean = spec$mean,
        sd = spec$sd
      ),

      beta = stats::qbeta(
        probabilities,
        shape1 = spec$shape1,
        shape2 = spec$shape2
      ),

      gamma = stats::qgamma(
        probabilities,
        shape = spec$shape,
        rate = spec$rate
      ),

      lognormal = stats::qlnorm(
        probabilities,
        meanlog = spec$meanlog,
        sdlog = spec$sdlog
      ),

      uniform = stats::qunif(
        probabilities,
        min = spec$min,
        max = spec$max
      )
    )

    theoretical <- c(
      mean = unname(theoretical_moments["mean"]),
      median = theoretical_quantiles[2],
      sd = unname(theoretical_moments["sd"]),
      p2.5 = theoretical_quantiles[1],
      p97.5 = theoretical_quantiles[3]
    )

    # Empirical statistics
    empirical_quantiles <- stats::quantile(
      samples,
      probs = probabilities,
      names = FALSE
    )

    empirical <- c(
      mean = mean(samples),
      median = empirical_quantiles[2],
      sd = stats::sd(samples),
      p2.5 = empirical_quantiles[1],
      p97.5 = empirical_quantiles[3]
    )

    # Differences: simulated minus theoretical
    differences <- empirical - theoretical

    data.frame(
      parameter = parameter_name,
      distribution = distribution,
      R = length(samples),

      mean_theoretical = theoretical["mean"],
      mean_simulated = empirical["mean"],
      mean_difference = differences["mean"],

      median_theoretical = theoretical["median"],
      median_simulated = empirical["median"],
      median_difference = differences["median"],

      sd_theoretical = theoretical["sd"],
      sd_simulated = empirical["sd"],
      sd_difference = differences["sd"],

      p2.5_theoretical = theoretical["p2.5"],
      p2.5_simulated = empirical["p2.5"],
      p2.5_difference = differences["p2.5"],

      p97.5_theoretical = theoretical["p97.5"],
      p97.5_simulated = empirical["p97.5"],
      p97.5_difference = differences["p97.5"],

      row.names = NULL,
      check.names = FALSE
    )
  }

  parameter_diagnostics <- do.call(
    rbind,
    lapply(names(prob_params), function(nm) {

      diagnose_parameter_distribution(
        parameter_name = nm,
        spec = prob_params[[nm]]$probabilistic,
        samples = parameter_samples[[nm]]
      )
    })
  )

  rownames(parameter_diagnostics) <- NULL

  # Preserve original probabilistic specifications
  settings_env$probabilistic_parameters <- lapply(
    prob_params,
    function(p) p$probabilistic
  )

  # ============================================================
  # 4. Simulate individual costs
  # ============================================================

  cost_matrix <- matrix(
    df[[cost]],
    nrow = nrow(df),
    ncol = R
  )

  for (nm in names(prob_params)) {

    p <- prob_params[[nm]]

    if (p$type != "cost") next

    resource <- p$resource
    base <- p$base

    if (is.null(resource) ||
        is.na(resource) ||
        !resource %in% names(df)) {
      stop("Resource variable not found for parameter: ", nm)
    }

    if (!is.numeric(df[[resource]]) ||
        any(!is.finite(df[[resource]]))) {
      stop(
        "Resource variable must contain finite numeric values: ",
        resource
      )
    }

    cost_matrix <- cost_matrix + outer(
      df[[resource]],
      parameter_samples[[nm]] - base
    )
  }

  # ============================================================
  # 5. Compute PSA results for each comparison
  # ============================================================

  stat_names <- c(
    "Delta_Cost",
    "Delta_Effect",
    "ICER",
    "Mean_Cost_ref",
    "Mean_Cost_alt",
    "Mean_Effect_ref",
    "Mean_Effect_alt",
    paste0("NMB_", lambda)
  )

  comparison_results <- list()
  combined_list <- list()

  for (alt in alts) {

    ref_idx <- as.character(df[[group]]) == ref_group
    alt_idx <- as.character(df[[group]]) == alt

    # Mean simulated costs
    mean_cost_ref <- colMeans(
      cost_matrix[ref_idx, , drop = FALSE]
    )

    mean_cost_alt <- colMeans(
      cost_matrix[alt_idx, , drop = FALSE]
    )

    delta_cost <- mean_cost_alt - mean_cost_ref

    # Observed mean effectiveness
    mean_effect_ref <- mean(df[[effect]][ref_idx])
    mean_effect_alt <- mean(df[[effect]][alt_idx])

    delta_effect <- rep(
      mean_effect_alt - mean_effect_ref,
      R
    )

    # Apply probabilistic effect parameters
    for (nm in names(prob_params)) {

      p <- prob_params[[nm]]

      if (p$type != "effect") next

      comparison <- p$comparison

      if (!is.null(comparison) &&
          !all(is.na(comparison)) &&
          !alt %in% comparison) {
        next
      }

      delta_effect <- delta_effect *
        (1 + parameter_samples[[nm]])
    }

    # ICER
    icer <- ifelse(
      delta_effect == 0,
      NA_real_,
      delta_cost / delta_effect
    )

    # Construct results
    samples <- data.frame(
      Delta_Cost = delta_cost,
      Delta_Effect = delta_effect,
      ICER = icer,
      Mean_Cost_ref = mean_cost_ref,
      Mean_Cost_alt = mean_cost_alt,
      Mean_Effect_ref = rep(mean_effect_ref, R),
      Mean_Effect_alt = mean_effect_ref + delta_effect
    )

    # Net Monetary Benefit
    for (l in lambda) {
      samples[[paste0("NMB_", l)]] <-
        l * delta_effect - delta_cost
    }

    samples <- samples[, stat_names, drop = FALSE]

    # Store comparison-level results
    out <- list(
      comparison = alt,
      psa_samples = samples,
      computed_by = "psa"
    )

    class(out) <- "cea_psa"
    attr(out, "settings_env") <- settings_env

    comparison_results[[alt]] <- out

    # Prepare tidy combined replicates
    combined <- samples

    combined$replicate <- seq_len(R)
    combined$dataset <- dataset_name
    combined$subgroup_var <- "Overall"
    combined$subgroup_level <- "Overall"
    combined$comparison <- alt
    combined$computed_by <- "psa"

    combined$group_uid <- paste(
      dataset_name,
      "Overall",
      "Overall",
      alt,
      "psa",
      sep = "_"
    )

    combined_list[[alt]] <- combined
  }

  # ============================================================
  # 6. Build hierarchical result structure
  # ============================================================

  class(comparison_results) <- "cea_multicomparison"

  dataset_result <- structure(
    list(
      Overall = comparison_results,
      Subgroups = NULL
    ),
    class = "cea_results"
  )

  result <- list()
  result[[dataset_name]] <- dataset_result

  # ============================================================
  # 7. Combine simulation replicates
  # ============================================================

  combined_df <- dplyr::bind_rows(combined_list)

  combined_df$subgroup_var <- factor(
    combined_df$subgroup_var
  )

  combined_df$subgroup_level <- factor(
    combined_df$subgroup_level
  )

  col_order <- c(
    "dataset",
    "subgroup_var",
    "subgroup_level",
    "comparison",
    "group_uid",
    "replicate",
    "computed_by",
    stat_names
  )

  result$combined_replicates <- tibble::as_tibble(
    combined_df[, col_order, drop = FALSE]
  )

  # ============================================================
  # 8. Store parameter samples, diagnostics and settings
  # ============================================================

  result$parameter_samples <- tibble::as_tibble(
    parameter_samples
  )

  result$parameter_diagnostics <- structure(
    tibble::as_tibble(parameter_diagnostics),
    class = c(
      "cea_parameter_diagnostics",
      "tbl_df",
      "tbl",
      "data.frame"
    ),
    distribution_specs = settings_env$probabilistic_parameters,
    parameter_samples = result$parameter_samples
  )

  result$settings <- settings_env

  class(result) <- c("cea_results_list", "list")

  if (verbose) {
    message(
      "PSA completed: ",
      R,
      " Monte Carlo iterations."
    )
  }

  return(result)
}
