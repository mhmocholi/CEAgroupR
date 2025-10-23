#' Summarize base statistics for CEA datasets
#'
#' Produces descriptive summaries of the datasets used in a cost-effectiveness
#' analysis, including counts per group and subgroup, means and standard deviations
#' of cost and effect, and incremental statistics (ΔCost, ΔEffect, ICER, NMB).
#' Supports multiple datasets and multiple willingness-to-pay thresholds.
#'
#' @param data A data frame or a named list of data frames containing the
#'   variables required for analysis.
#' @param group Character. Column name defining the comparison groups.
#' @param cost Character. Column name for total cost.
#' @param effect Character. Column name for effectiveness measure.
#' @param subgroup_vars Optional character vector. Variables for subgrouping.
#' @param lambda Numeric or numeric vector. Willingness-to-pay threshold(s)
#'   for calculating NMB. Defaults to 25000.
#'
#' @return A tibble summarizing sample sizes, mean and SD of costs/effects,
#'   and incremental statistics per dataset and subgroup.
#' @importFrom dplyr summarise group_by bind_rows mutate n across
#' @importFrom tibble tibble
#' @export
summarize_cea_data <- function(data, group, cost, effect,
                               subgroup_vars = NULL, lambda = 25000) {

  summarize_one <- function(df, dataset_name) {

    # Helper to compute summary for a given subset (overall or subgroup)
    compute_summary <- function(sub_df, subgroup_var = "Overall", subgroup_level = NA) {

      sub_df[[group]] <- as.factor(sub_df[[group]])
      if (length(unique(sub_df[[group]])) < 2) return(NULL)

      # --- Calcular medias y SD por grupo ---
      means_cost <- tapply(sub_df[[cost]], sub_df[[group]], mean, na.rm = TRUE)
      sds_cost   <- tapply(sub_df[[cost]], sub_df[[group]], sd,   na.rm = TRUE)

      means_eff  <- tapply(sub_df[[effect]], sub_df[[group]], mean, na.rm = TRUE)
      sds_eff    <- tapply(sub_df[[effect]], sub_df[[group]], sd,   na.rm = TRUE)

      ns <- table(sub_df[[group]])

      # --- Calcular incrementales ---
      delta_cost <- means_cost[2] - means_cost[1]
      delta_eff  <- means_eff[2] - means_eff[1]
      icer <- ifelse(delta_eff == 0, NA, delta_cost / delta_eff)

      nmb_values <- sapply(lambda, function(l) l * delta_eff - delta_cost)
      names(nmb_values) <- paste0("NMB_", lambda)

      tibble::tibble(
        dataset = dataset_name,
        subgroup_var = subgroup_var,
        subgroup_level = subgroup_level,
        n_g0 = as.integer(ns[1]),
        n_g1 = as.integer(ns[2]),
        mean_cost_g0 = as.numeric(means_cost[1]),
        sd_cost_g0   = as.numeric(sds_cost[1]),
        mean_cost_g1 = as.numeric(means_cost[2]),
        sd_cost_g1   = as.numeric(sds_cost[2]),
        mean_eff_g0  = as.numeric(means_eff[1]),
        sd_eff_g0    = as.numeric(sds_eff[1]),
        mean_eff_g1  = as.numeric(means_eff[2]),
        sd_eff_g1    = as.numeric(sds_eff[2]),
        delta_cost = delta_cost,
        delta_effect = delta_eff,
        ICER = icer,
        !!!as.list(nmb_values)
      )
    }

    # Overall summary
    overall_summary <- compute_summary(df, "Overall", NA)

    # Subgroup summaries (if specified)
    subgroup_summaries <- NULL
    if (!is.null(subgroup_vars)) {
      subgroup_summaries <- dplyr::bind_rows(lapply(subgroup_vars, function(var) {
        lvls <- unique(na.omit(df[[var]]))
        dplyr::bind_rows(lapply(lvls, function(lvl) {
          compute_summary(df[df[[var]] == lvl, , drop = FALSE],
                          subgroup_var = var,
                          subgroup_level = lvl)
        }))
      }))
    }

    dplyr::bind_rows(overall_summary, subgroup_summaries)
  }

  # Handle single dataset or list
  if (is.data.frame(data)) {
    summarize_one(data, "single_dataset")
  } else if (is.list(data)) {
    dplyr::bind_rows(lapply(names(data), function(ds_name) {
      summarize_one(data[[ds_name]], ds_name)
    }))
  } else {
    stop("`data` must be a data frame or a list of data frames.")
  }
}
