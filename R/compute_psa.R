
#' Probabilistic Sensitivity Analysis (PSA)
#'
#' Performs Monte Carlo simulations for cost-effectiveness
#' and cost-utility analyses.
#'
#' @param data A data frame or named list of data frames.
#' @param group Variable identifying intervention strategies.
#' @param cost Variable containing individual total costs.
#' @param effect Variable containing individual effectiveness outcomes.
#' @param parameters Object created by define_cea_parameters().
#' @param R Number of Monte Carlo iterations.
#' @param lambda Willingness-to-pay thresholds.
#' @param subgroup_vars Optional subgroup variables.
#' @param seed Random seed for reproducibility.
#' @param verbose Display simulation progress.
#' @param ref_group Reference strategy.
#' @param alt_groups Optional alternative strategies.
#'
#' @return A cea_results_list object containing PSA results.
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
      stop("Resource variable must contain finite numeric values: ",
           resource)
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
  # 8. Store parameter samples and settings
  # ============================================================

  result$parameter_samples <- tibble::as_tibble(
    parameter_samples
  )

  result$settings <- settings_env

  class(result) <- c("cea_results_list", "list")

  if (verbose) {
    message("PSA completed: ", R, " Monte Carlo iterations.")
  }

  return(result)
}
