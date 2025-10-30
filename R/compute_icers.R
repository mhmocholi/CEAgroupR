#' Compute Incremental Cost-Effectiveness Ratios (ICERs) using Nonparametric Bootstrap
#'
#' Performs nonparametric bootstrap resampling to estimate incremental cost,
#' incremental effectiveness, ICER, and Net Monetary Benefit (NMB) between two
#' comparison groups. Supports multiple datasets and subgroup analyses.
#' Includes progress tracking during bootstrap execution and confidence interval
#' estimation.
#'
#' @param data Data frame or list of data frames with required variables.
#' @param group Character. Name of grouping variable.
#' @param cost Character. Name of cost variable.
#' @param effect Character. Name of effectiveness variable.
#' @param R Integer. Number of bootstrap replications.
#' @param lambda Numeric vector. Willingness-to-pay thresholds.
#' @param ci_type Character. Type of confidence interval ("bca", "perc", etc.).
#' @param subgroup_vars Character vector of subgrouping variables.
#' @return A list of class \code{cea_results_list}.
#' @export
compute_icers <- function(data, group, cost, effect,
                          R = 1000, lambda = 25000,
                          ci_type = "bca", subgroup_vars = NULL) {

  # ---- 1. Initialize reproducible seed ----
  global_seed <- 1902
  set.seed(global_seed)

  # ---- 2. Validate input ----
  if (is.data.frame(data)) {
    dataset_names <- "single_dataset"
  } else if (is.list(data)) {
    colnames_ref <- names(data[[1]])
    inconsistent <- any(sapply(data[-1], function(df) !identical(names(df), colnames_ref)))
    if (inconsistent) stop("All datasets must have identical structure.")
    if (is.null(names(data)) || any(names(data) == "")) {
      dataset_names <- paste0("dataset_", seq_along(data))
      names(data) <- dataset_names
    } else {
      dataset_names <- names(data)
    }
  } else stop("`data` must be a data frame or list of data frames.")

  # ---- 3. Statistic function ----
  ratio_fun <- function(d, i, group_col, cost_col, effect_col, lambda_vec) {
    d_boot <- d[i, ]
    d_boot[[group_col]] <- as.factor(d_boot[[group_col]])
    if (length(unique(d_boot[[group_col]])) < 2)
      return(rep(NA, 8 + length(lambda_vec)))

    cost_means <- tapply(d_boot[[cost_col]], d_boot[[group_col]], mean, na.rm = TRUE)
    eff_means  <- tapply(d_boot[[effect_col]], d_boot[[group_col]], mean, na.rm = TRUE)
    delta_cost <- cost_means[2] - cost_means[1]
    delta_eff  <- eff_means[2] - eff_means[1]
    icer <- ifelse(delta_eff == 0, NA, delta_cost / delta_eff)
    nmb_values <- sapply(lambda_vec, function(l) l * delta_eff - delta_cost)
    names(nmb_values) <- paste0("NMB_", lambda_vec)
    c(delta_cost, delta_eff, icer,
      cost_means[1], cost_means[2],
      eff_means[1], eff_means[2],
      nmb_values)
  }

  # ---- 4. Safe CI extraction ----
  safe_ci <- function(boot_obj, index, pb_ci) {
    setTxtProgressBar(pb_ci, index)
    ci_obj <- tryCatch(
      boot::boot.ci(boot_obj, type = ci_type, index = index),
      error = function(e) NULL
    )
    if (is.null(ci_obj)) return(c(NA, NA))
    if (!is.null(ci_obj[[ci_type]])) return(ci_obj[[ci_type]][4:5])
    if (!is.null(ci_obj$bca)) return(ci_obj$bca[4:5])
    if (!is.null(ci_obj$perc)) return(ci_obj$perc[4:5])
    c(NA, NA)
  }

  # ---- 5. Analysis for one dataset (stable tracking version) ----
  analyze_data <- function(df, dataset_name = NULL, subgroup_info = NULL) {
    df[[group]] <- as.factor(df[[group]])

    # Context message
    context <- if (is.null(subgroup_info)) {
      paste0("[Dataset: ", dataset_name, "] [Overall]")
    } else {
      paste0("[Dataset: ", dataset_name, "] [Subgroup: ",
             subgroup_info$var, " = ", subgroup_info$level, "]")
    }
    message(context)
    message("Bootstrap analysis started (", nrow(df), " observations, R = ", R, ")")

    # Progress bar for bootstrap
    pb <- txtProgressBar(min = 0, max = R, style = 3)
    progress_counter <- 0

    boot_res <- boot::boot(
      data = df,
      statistic = function(d, i) {
        progress_counter <<- progress_counter + 1
        if (progress_counter %% (R / 20) == 0 || progress_counter == R) {
          setTxtProgressBar(pb, progress_counter)
        }
        ratio_fun(d, i,
                  group_col = group,
                  cost_col = cost,
                  effect_col = effect,
                  lambda_vec = lambda)
      },
      R = R,
      strata = as.numeric(df[[group]])
    )
    close(pb)

    message("Computing confidence intervals...")
    pb_ci <- txtProgressBar(min = 0, max = ncol(boot_res$t), style = 3)

    base_names <- c(
      "Delta_Cost", "Delta_Effect", "ICER",
      "Mean_Cost_g1", "Mean_Cost_g2",
      "Mean_Effect_g1", "Mean_Effect_g2"
    )
    nmb_names <- paste0("NMB_", lambda)
    stat_names <- c(base_names, nmb_names)
    colnames(boot_res$t) <- stat_names
    names(boot_res$t0) <- stat_names

    ci_list <- setNames(lapply(seq_along(stat_names),
                               function(i) safe_ci(boot_res, i, pb_ci)),
                        stat_names)
    close(pb_ci)

    message("Confidence intervals computed successfully.\n")

    bootstrap_df <- as.data.frame(boot_res$t)
    names(bootstrap_df) <- stat_names
    bootstrap_df <- as.data.frame(lapply(bootstrap_df, as.numeric))
    t0_values <- as.numeric(boot_res$t0)
    names(t0_values) <- stat_names
    bias_values <- colMeans(boot_res$t, na.rm = TRUE) - t0_values

    out <- list(
      bootstrap_samples = bootstrap_df,
      boot_ci = ci_list,
      t0 = t0_values,
      bias = bias_values
    )
    class(out) <- "cea_base"
    out
  }

  # ---- 6. Wrapper for subgroups ----
  analyze_with_subgroups <- function(df, dataset_name) {
    if (!is.null(subgroup_vars)) {
      for (v in subgroup_vars) if (!is.factor(df[[v]])) df[[v]] <- as.factor(df[[v]])
    }

    overall_result <- analyze_data(df, dataset_name = dataset_name)
    subgroup_results <- NULL
    if (!is.null(subgroup_vars)) {
      subgroup_results <- lapply(subgroup_vars, function(var) {
        sub_data <- df %>% dplyr::filter(!is.na(.data[[var]]))
        split_data <- split(sub_data, sub_data[[var]])
        split_data <- Filter(function(x) length(unique(x[[group]])) == 2, split_data)
        lapply(names(split_data), function(lvl) {
          analyze_data(split_data[[lvl]],
                       dataset_name = dataset_name,
                       subgroup_info = list(var = var, level = lvl))
        }) |> setNames(names(split_data))
      })
      names(subgroup_results) <- subgroup_vars
    }
    structure(list(Overall = overall_result, Subgroups = subgroup_results),
              class = "cea_results")
  }

  # ---- 7. Run analyses ----
  if (is.data.frame(data)) {
    result <- list(single_dataset = analyze_with_subgroups(data, "single_dataset"))
  } else {
    result <- lapply(names(data), function(nm) analyze_with_subgroups(data[[nm]], nm))
    names(result) <- names(data)
  }
  class(result) <- c("cea_results_list", "list")

  # ---- 8. Combine and summarize ----
  message("Combining bootstrap replicates and computing summary statistics...")
  result$combined_replicates <- combine_icers_results(result)
  result$summary_stats <- summarize_cea_data(
    data = data,
    group = group,
    cost = cost,
    effect = effect,
    subgroup_vars = subgroup_vars,
    lambda = lambda
  )
  class(result$summary_stats) <- c("summary_stats", class(result$summary_stats))

  # ---- 9. Metadata ----
  settings <- list(
    call = match.call(),
    lambda = lambda,
    R = R,
    subgroup_vars = subgroup_vars,
    datasets = dataset_names,
    seed = global_seed,
    date = Sys.time(),
    ci_type = ci_type
  )
  result$settings <- settings

  message("Analysis completed successfully.")
  return(result)
}
