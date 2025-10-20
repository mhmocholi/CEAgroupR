#' Bootstrap estimation of incremental cost-effectiveness ratios (ICERs)
#'
#' Performs nonparametric bootstrap resampling to estimate incremental cost,
#' incremental effectiveness, ICER, and Net Monetary Benefit (NMB) between two
#' comparison groups (e.g., usual care vs. intervention). The function also
#' reports group-specific mean costs and effects, and optionally performs the
#' analysis by predefined subgroups. It supports multiple datasets, allowing
#' deterministic sensitivity analyses or scenario comparisons.
#'
#' @param data A data frame or a named list of data frames containing all
#'   variables required for the analysis. All datasets in the list must share
#'   the same structure and variable names.
#' @param group A string indicating the column name that defines the two
#'   comparison groups (reference and intervention).
#' @param cost A string indicating the column name for total cost.
#' @param effect A string indicating the column name for the measure of
#'   effectiveness (e.g., QALYs, life-years).
#' @param R Integer. Number of bootstrap replications (default = 1000).
#' @param lambda Numeric. Willingness-to-pay threshold (default = 25000).
#' @param ci_type Character. Type of confidence interval for \code{boot.ci}
#'   ("bca", "perc", "basic", "norm").
#' @param subgroup_vars Optional character vector. Names of subgroup variables
#'   to stratify the analysis. These variables will be automatically converted
#'   to factors (if not already) to ensure consistent subgrouping.
#'
#' @return If a single data frame is supplied, returns a list of class
#'   \code{cea_results} containing:
#'   \itemize{
#'     \item \code{Overall}: A \code{cea_base} object with results for the full sample.
#'     \item \code{Subgroups}: A named list of subgroup-specific results.
#'     \item \code{combined_replicates}: A tibble combining all bootstrap samples.
#'     \item \code{settings}: A list with metadata (analysis parameters and reproducibility info).
#'   }
#'
#'   If a list of data frames is supplied, returns a named list of
#'   \code{cea_results} objects (class \code{cea_results_list}) plus the element
#'   \code{combined_replicates} and \code{settings}.
#'
#' @importFrom boot boot boot.ci
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
icers_base <- function(data, group, cost, effect,
                       R = 1000, lambda = 25000,
                       ci_type = "bca", subgroup_vars = NULL) {

  # ---- Internal bootstrap statistic function ----
  ratio_fun <- function(d, i, group_col, cost_col, effect_col, lambda) {
    d_boot <- d[i, ]
    d_boot[[group_col]] <- as.factor(d_boot[[group_col]])

    if (length(unique(d_boot[[group_col]])) < 2) return(rep(NA, 8))

    cost_means <- tapply(d_boot[[cost_col]], d_boot[[group_col]], mean, na.rm = TRUE)
    eff_means  <- tapply(d_boot[[effect_col]], d_boot[[group_col]], mean, na.rm = TRUE)

    delta_cost <- cost_means[2] - cost_means[1]
    delta_eff  <- eff_means[2] - eff_means[1]
    icer <- ifelse(delta_eff == 0, NA, delta_cost / delta_eff)
    nmb  <- lambda * delta_eff - delta_cost

    c(delta_cost, delta_eff, icer,
      cost_means[1], cost_means[2],
      eff_means[1], eff_means[2],
      nmb)
  }

  # ---- Safe extraction of bootstrap confidence intervals ----
  safe_ci <- function(boot_obj, index) {
    ci_obj <- tryCatch(
      boot::boot.ci(boot_obj, type = ci_type, index = index),
      error = function(e) NULL
    )
    if (is.null(ci_obj)) {
      result <- c(NA, NA)
    } else if (!is.null(ci_obj[[ci_type]])) {
      result <- ci_obj[[ci_type]][4:5]
    } else if (!is.null(ci_obj$bca)) {
      result <- ci_obj$bca[4:5]
    } else if (!is.null(ci_obj$perc)) {
      result <- ci_obj$perc[4:5]
    } else {
      result <- c(NA, NA)
    }
    names(result) <- c("lower_95", "upper_95")
    result
  }

  # ---- Analysis for one dataset ----
  analyze_data <- function(df) {
    df[[group]] <- as.factor(df[[group]])

    boot_res <- suppressMessages(
      boot::boot(
        data = df,
        statistic = function(d, i)
          ratio_fun(d, i,
                    group_col = group,
                    cost_col = cost,
                    effect_col = effect,
                    lambda = lambda),
        R = R,
        strata = as.numeric(df[[group]])
      )
    )

    stat_names <- c(
      "Delta_Cost", "Delta_Effect", "ICER",
      "Mean_Cost_g1", "Mean_Cost_g2",
      "Mean_Effect_g1", "Mean_Effect_g2", "NMB"
    )

    colnames(boot_res$t) <- stat_names
    names(boot_res$t0) <- stat_names
    boot_res$call$data <- substitute(df)

    ci_list <- setNames(lapply(seq_along(stat_names),
                               function(i) safe_ci(boot_res, i)),
                        stat_names)

    bootstrap_df <- as.data.frame(boot_res$t)
    names(bootstrap_df) <- stat_names
    bootstrap_df <- as.data.frame(lapply(bootstrap_df, as.numeric))

    out <- list(
      boot_results = boot_res,
      boot_ci = ci_list,
      bootstrap_samples = bootstrap_df
    )
    class(out) <- "cea_base"
    out
  }

  # ---- Wrapper for subgroups ----
  analyze_with_subgroups <- function(df) {

    # Convert subgroup variables to factors automatically
    if (!is.null(subgroup_vars)) {
      for (v in subgroup_vars) {
        if (!is.factor(df[[v]])) {
          df[[v]] <- as.factor(df[[v]])
        }
      }
    }

    overall_result <- analyze_data(df)
    subgroup_results <- NULL

    if (!is.null(subgroup_vars)) {
      subgroup_results <- lapply(subgroup_vars, function(var) {
        sub_data <- df %>% dplyr::filter(!is.na(.data[[var]]))
        split_data <- split(sub_data, sub_data[[var]])
        split_data <- Filter(function(x) length(unique(x[[group]])) == 2, split_data)
        lapply(split_data, analyze_data)
      })
      names(subgroup_results) <- subgroup_vars
    }

    structure(
      list(Overall = overall_result, Subgroups = subgroup_results),
      class = "cea_results"
    )
  }

  # ---- Create metadata (settings) ----
  settings <- list(
    call = match.call(),
    lambda = lambda,
    R = R,
    subgroup_vars = subgroup_vars,
    datasets = if (is.list(data)) names(data) else "single_dataset",
    # Capture the numeric seed used to initialize the RNG
    seed = if (exists(".Random.seed", envir = .GlobalEnv)) {
      as.integer(get(".Random.seed", envir = .GlobalEnv)[2])
    } else {
      NULL
    },
    date = Sys.time()
  )

  # ---- Main logic: single dataset vs. list of datasets ----
  if (is.data.frame(data)) {
    result <- analyze_with_subgroups(data)
    result$combined_replicates <- combine_icers_results(result)
    result$settings <- settings

  } else if (is.list(data)) {
    if (is.null(names(data)) || any(names(data) == "")) {
      names(data) <- paste0("dataset_", seq_along(data))
    }

    # Run analyses for each dataset
    result <- lapply(data, analyze_with_subgroups)
    class(result) <- c("cea_results_list", "list")

    # Add metadata
    result$settings <- settings

    # Combine results directly; combine_icers_results ignores non-result elements
    result$combined_replicates <- combine_icers_results(result)
  } else {
    stop("`data` must be either a data frame or a named list of data frames.")
  }

  return(result)
}
