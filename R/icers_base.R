#' Bootstrap estimation of incremental cost-effectiveness ratios (ICERs)
#'
#' Performs nonparametric bootstrap resampling to estimate incremental cost,
#' incremental effectiveness, ICER, and Net Monetary Benefit (NMB) between two
#' comparison groups (e.g., usual care vs. intervention). The function supports
#' multiple datasets (for scenario or sensitivity analyses) and optional subgroup
#' analyses. It can also compute NMB for multiple willingness-to-pay thresholds
#' (\code{lambda}) in a single run.
#'
#' The returned object is hierarchical: each dataset contains its own overall and
#' subgroup analyses, while the root level stores the combined bootstrap replicates
#' and metadata. The structure is designed for transparent, reproducible
#' cost-effectiveness workflows.
#'
#' @param data A data frame or a named list of data frames containing all
#'   variables required for the analysis. All datasets must share the same structure.
#' @param group Character. Column name defining the comparison groups.
#' @param cost Character. Column name for total cost.
#' @param effect Character. Column name for the effectiveness measure
#'   (e.g., QALYs, life-years).
#' @param R Integer. Number of bootstrap replications (default = 1000).
#' @param lambda Numeric or numeric vector. Willingness-to-pay threshold(s)
#'   used to compute NMB. Defaults to 25000.
#' @param ci_type Character. Type of confidence interval for \code{boot.ci}
#'   ("bca", "perc", "basic", "norm").
#' @param subgroup_vars Optional character vector. Names of subgroup variables
#'   to stratify the analysis.
#'
#' @return A list of class \code{cea_results_list} containing:
#'   \itemize{
#'     \item One element per dataset (each a \code{cea_results} list with
#'           Overall, Subgroups, and Summary).
#'     \item A single tibble \code{$combined_replicates} combining all bootstrap
#'           samples across datasets and subgroups.
#'     \item A global \code{$settings} list (lambda, R, seed, date, etc.).
#'   }
#'
#' @importFrom boot boot boot.ci
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
icers_base <- function(data, group, cost, effect,
                       R = 1000, lambda = 25000,
                       ci_type = "bca", subgroup_vars = NULL) {

  # ---- Initialize reproducible seed ----
  global_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    as.integer(get(".Random.seed", envir = .GlobalEnv)[2])
  } else {
    sample.int(.Machine$integer.max, 1)
  }
  set.seed(global_seed)

  # ---- Validate data input and consistency ----
  if (is.data.frame(data)) {
    dataset_names <- "single_dataset"
  } else if (is.list(data)) {
    # Validate that all datasets have the same structure
    colnames_ref <- names(data[[1]])
    inconsistent <- any(sapply(data[-1], function(df) !identical(names(df), colnames_ref)))
    if (inconsistent) {
      stop("All datasets provided in `data` must have identical variable names and structure.")
    }
    # Assign names if missing
    if (is.null(names(data)) || any(names(data) == "")) {
      dataset_names <- paste0("dataset_", seq_along(data))
      names(data) <- dataset_names
    } else {
      dataset_names <- names(data)
    }
  } else {
    stop("`data` must be either a data frame or a named list of data frames.")
  }

  # ---- Internal bootstrap statistic function ----
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

    # Multiple NMBs if lambda is a vector
    nmb_values <- sapply(lambda_vec, function(l) l * delta_eff - delta_cost)
    names(nmb_values) <- paste0("NMB_", lambda_vec)

    c(delta_cost, delta_eff, icer,
      cost_means[1], cost_means[2],
      eff_means[1], eff_means[2],
      nmb_values)
  }

  # ---- Safe extraction of bootstrap confidence intervals ----
  safe_ci <- function(boot_obj, index) {
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

  # ---- Analysis for one dataset ----
  analyze_data <- function(df) {
    df[[group]] <- as.factor(df[[group]])
    set.seed(global_seed)

    boot_res <- suppressMessages(
      boot::boot(
        data = df,
        statistic = function(d, i)
          ratio_fun(d, i,
                    group_col = group,
                    cost_col = cost,
                    effect_col = effect,
                    lambda_vec = lambda),
        R = R,
        strata = as.numeric(df[[group]])
      )
    )

    base_names <- c(
      "Delta_Cost", "Delta_Effect", "ICER",
      "Mean_Cost_g1", "Mean_Cost_g2",
      "Mean_Effect_g1", "Mean_Effect_g2"
    )
    nmb_names <- paste0("NMB_", lambda)
    stat_names <- c(base_names, nmb_names)

    # Asignar nombres a los resultados bootstrap
    colnames(boot_res$t) <- stat_names
    names(boot_res$t0) <- stat_names

    # Calcular IC 95% para cada parámetro
    ci_list <- setNames(lapply(seq_along(stat_names),
                               function(i) safe_ci(boot_res, i)),
                        stat_names)

    # Construir data.frame con todas las réplicas
    bootstrap_df <- as.data.frame(boot_res$t)
    names(bootstrap_df) <- stat_names
    bootstrap_df <- as.data.frame(lapply(bootstrap_df, as.numeric))

    # Valores originales (t0) y sesgos
    t0_values <- as.numeric(boot_res$t0)
    names(t0_values) <- stat_names
    bias_values <- colMeans(boot_res$t, na.rm = TRUE) - t0_values

    # Crear objeto limpio y autocontenido
    out <- list(
      bootstrap_samples = bootstrap_df,
      boot_ci = ci_list,
      t0 = t0_values,
      bias = bias_values
    )
    class(out) <- "cea_base"
    out
  }

  # ---- Wrapper for subgroups ----
  analyze_with_subgroups <- function(df) {
    if (!is.null(subgroup_vars)) {
      for (v in subgroup_vars) {
        if (!is.factor(df[[v]])) df[[v]] <- as.factor(df[[v]])
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

    structure(list(Overall = overall_result, Subgroups = subgroup_results),
              class = "cea_results")
  }

  # ---- Run analyses (single or multiple datasets) ----
  if (is.data.frame(data)) {
    result <- list(single_dataset = analyze_with_subgroups(data))
  } else {
    result <- lapply(data, analyze_with_subgroups)
  }
  class(result) <- c("cea_results_list", "list")

  # ---- Combine all replicates globally ----
  result$combined_replicates <- combine_icers_results(result)

  # ---- Generate descriptive summary statistics ----
  result$summary_stats <- summarize_cea_data(
    data = data,
    group = group,
    cost = cost,
    effect = effect,
    subgroup_vars = subgroup_vars,
    lambda = lambda
  )
  class(result$summary_stats) <- c("summary_stats", class(result$summary_stats))


  # ---- Create global metadata ----
  settings <- list(
    call = match.call(),
    lambda = lambda,
    R = R,
    subgroup_vars = subgroup_vars,
    datasets = dataset_names,
    seed = global_seed,
    date = Sys.time()
  )
  result$settings <- settings

  return(result)
}
