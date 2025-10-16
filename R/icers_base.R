#' Bootstrap estimation of incremental cost-effectiveness ratios (ICERs)
#'
#' Performs nonparametric bootstrap resampling to estimate incremental cost,
#' incremental effectiveness, ICER, and Net Monetary Benefit (NMB) between two
#' comparison groups (e.g., usual care vs. intervention). The function also
#' reports group-specific mean costs and effects, and optionally performs the
#' analysis by predefined subgroups.
#'
#' @param data A data frame containing all variables required for the analysis.
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
#'   to stratify the analysis.
#'
#' @return A list of class `cea_results` containing:
#' \itemize{
#'   \item \code{Overall}: A `cea_base` object with results for the full sample.
#'   \item \code{Subgroups}: A named list of subgroup-specific results.
#' }
#'
#' Each `cea_base` object includes:
#' \itemize{
#'   \item \code{boot_results}: The `boot()` object.
#'   \item \code{boot_ci}: Bootstrap confidence intervals for all parameters.
#'   \item \code{bootstrap_samples}: Data frame of bootstrap replicates with 8
#'   columns: Delta_Cost, Delta_Effect, ICER, Mean_Cost_g1, Mean_Cost_g2,
#'   Mean_Effect_g1, Mean_Effect_g2, NMB.
#' }
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

    # Assign names within boot object
    colnames(boot_res$t) <- stat_names
    names(boot_res$t0) <- stat_names

    # Restore call to boot
    boot_res$call$data <- substitute(df)

    # Compute confidence intervals
    ci_list <- setNames(lapply(seq_along(stat_names),
                               function(i) safe_ci(boot_res, i)),
                        stat_names)

    # Bootstrap samples table
    bootstrap_df <- as.data.frame(boot_res$t)
    names(bootstrap_df) <- stat_names
    bootstrap_df <- as.data.frame(lapply(bootstrap_df, function(x) round(x, 3)))

    out <- list(
      boot_results = boot_res,
      boot_ci = ci_list,
      bootstrap_samples = bootstrap_df
    )
    class(out) <- "cea_base"
    out
  }

  overall_result <- analyze_data(data)

  subgroup_results <- NULL
  if (!is.null(subgroup_vars)) {
    subgroup_results <- lapply(subgroup_vars, function(var) {
      sub_data <- data %>% dplyr::filter(!is.na(.data[[var]]))
      split_data <- split(sub_data, sub_data[[var]])
      split_data <- Filter(function(df) length(unique(df[[group]])) == 2, split_data)
      lapply(split_data, analyze_data)
    })
    names(subgroup_results) <- subgroup_vars
  }

  structure(list(Overall = overall_result,
                 Subgroups = subgroup_results),
            class = "cea_results")
}
