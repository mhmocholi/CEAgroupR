#' Compute Incremental Cost-Effectiveness Ratios (ICERs) using Nonparametric Bootstrap
#'
#' Performs nonparametric bootstrap resampling to estimate incremental cost,
#' incremental effectiveness, ICER, and Net Monetary Benefit (NMB) between one
#' reference strategy and one or more alternative strategies.
#'
#' Subgroup variables are internally normalized to factors to ensure complete
#' structural consistency across all datasets and subgroup levels.
#'   - Numeric/Integer subgroups are converted to ordered factors.
#'   - Character subgroups are converted to unordered factors preserving the
#'     observed level order.
#'
#' @param data Data frame or list of data frames.
#' @param group Strategy variable name.
#' @param cost Cost variable name.
#' @param effect Effectiveness variable name.
#' @param R Number of bootstrap replications.
#' @param lambda Numeric WTP threshold(s).
#' @param ci_type Type of CI ("bca", "perc"...).
#' @param subgroup_vars Optional subgroup variables (character vector).
#' @param seed Optional seed for reproducibility.
#' @param verbose Logical. Show progress bars and messages.
#' @param ref_group Reference strategy.
#' @param alt_groups Vector of alternative strategies (optional).
#'
#' @return A \code{cea_results_list} object containing all analyses.
#' @export
compute_icers <- function(data, group, cost, effect,
                          R = 1000, lambda = 25000,
                          ci_type = "bca", subgroup_vars = NULL,
                          seed = NULL, verbose = FALSE,
                          ref_group, alt_groups = NULL) {

  # ============================================================
  # 0. Shared analysis environment
  # ============================================================
  .settings_env <- new.env(parent = emptyenv())
  .settings_env$ref_group     <- ref_group
  .settings_env$alt_groups    <- alt_groups
  .settings_env$lambda        <- lambda
  .settings_env$R             <- R
  .settings_env$ci_type       <- ci_type
  .settings_env$subgroup_vars <- subgroup_vars

  # ============================================================
  # 1. Seed
  # ============================================================
  if (is.null(seed)) seed <- as.integer(Sys.time()) %% 1e6
  set.seed(seed)
  .settings_env$seed <- seed

  # ============================================================
  # 2. Normalize input data into list
  # ============================================================
  if (is.data.frame(data)) {

    data_list <- list(single_dataset = data)

  } else if (is.list(data)) {

    if (is.null(names(data)))
      names(data) <- paste0("dataset_", seq_along(data))

    data_list <- data

  } else {
    stop("`data` must be a data frame or list.")
  }

  # ============================================================
  # 3. Bootstrap statistic function
  # ============================================================
  ratio_fun <- function(d, i,
                        group_col, cost_col, effect_col,
                        lambda_vec, ref_name, alt_name) {

    d_boot <- d[i, ]
    d_boot[[group_col]] <- as.factor(d_boot[[group_col]])

    # Strategies must both exist in this bootstrap sample
    if (!all(c(ref_name, alt_name) %in% unique(d_boot[[group_col]])))
      return(rep(NA, 7 + length(lambda_vec)))

    cost_means <- tapply(d_boot[[cost_col]],   d_boot[[group_col]], mean, na.rm = TRUE)
    eff_means  <- tapply(d_boot[[effect_col]], d_boot[[group_col]], mean, na.rm = TRUE)

    delta_cost <- cost_means[[alt_name]] - cost_means[[ref_name]]
    delta_eff  <- eff_means[[alt_name]]  - eff_means[[ref_name]]
    icer       <- ifelse(delta_eff == 0, NA, delta_cost / delta_eff)

    nmb_vals <- sapply(lambda_vec, function(l)
      l * delta_eff - delta_cost)

    c(delta_cost, delta_eff, icer,
      cost_means[[ref_name]], cost_means[[alt_name]],
      eff_means[[ref_name]],  eff_means[[alt_name]],
      nmb_vals)
  }

  # ============================================================
  # 4. CI wrapper
  # ============================================================
  safe_ci <- function(boot_obj, index) {

    ci <- tryCatch(
      boot::boot.ci(boot_obj, type = ci_type, index = index),
      error = function(e) NULL
    )

    if (is.null(ci)) return(c(NA, NA))

    if (!is.null(ci[[ci_type]])) return(ci[[ci_type]][4:5])
    if (!is.null(ci$bca))        return(ci$bca[4:5])
    if (!is.null(ci$perc))       return(ci$perc[4:5])

    c(NA, NA)
  }

  # ============================================================
  # 5. Analyze a dataset
  # ============================================================
  analyze_data <- function(df, dataset_name) {

    if (verbose)
      message("[Dataset: ", dataset_name, "] Starting analysis...")

    df[[group]] <- as.factor(df[[group]])
    lvls <- levels(df[[group]])

    if (!ref_group %in% lvls)
      stop("Reference group not found in dataset ", dataset_name)

    alts <- if (is.null(alt_groups))
      setdiff(lvls, ref_group)
    else
      intersect(lvls, alt_groups)

    results <- list()

    for (alt in alts) {

      if (verbose)
        message("  - Comparison: ", alt, " vs ", ref_group)

      df_comp <- df[df[[group]] %in% c(ref_group, alt), , drop = FALSE]
      df_comp[[group]] <- factor(df_comp[[group]], levels = c(ref_group, alt))

      # Missing strategy → NA block
      if (any(table(df_comp[[group]]) == 0)) {

        empty <- rep(NA, 7 + length(lambda))
        names(empty) <- c(
          "Delta_Cost","Delta_Effect","ICER",
          "Mean_Cost_ref","Mean_Cost_alt",
          "Mean_Effect_ref","Mean_Effect_alt",
          paste0("NMB_", lambda)
        )

        out <- list(
          comparison        = alt,
          bootstrap_samples = as.data.frame(t(empty)),
          boot_ci           = setNames(
            replicate(length(empty), c(NA, NA), simplify = FALSE),
            names(empty)
          ),
          t0                = empty,
          bias              = rep(NA, length(empty))
        )

        attr(out, "settings_env") <- .settings_env
        class(out) <- "cea_base"
        results[[alt]] <- out
        next
      }

      # Bootstrap
      pb <- NULL
      if (verbose) {
        pb <- txtProgressBar(min = 0, max = R, style = 3)
        counter <- 0
      }

      boot_res <- boot::boot(
        data = df_comp,
        statistic = function(d, i) {

          if (verbose) {
            counter <<- counter + 1
            if (counter %% 10 == 0 || counter == R)
              setTxtProgressBar(pb, counter)
          }

          ratio_fun(d, i, group, cost, effect, lambda, ref_group, alt)
        },
        R = R,
        strata = as.numeric(df_comp[[group]])
      )

      if (verbose && !is.null(pb)) close(pb)

      stat_names <- c(
        "Delta_Cost","Delta_Effect","ICER",
        "Mean_Cost_ref","Mean_Cost_alt",
        "Mean_Effect_ref","Mean_Effect_alt",
        paste0("NMB_", lambda)
      )

      colnames(boot_res$t) <- stat_names
      names(boot_res$t0)  <- stat_names

      ci_list <- lapply(seq_along(stat_names),
                        function(j) safe_ci(boot_res, j))

      out <- list(
        comparison        = alt,
        bootstrap_samples = as.data.frame(boot_res$t),
        boot_ci           = setNames(ci_list, stat_names),
        t0                = boot_res$t0,
        bias              = colMeans(boot_res$t, na.rm = TRUE) - boot_res$t0
      )

      attr(out, "settings_env") <- .settings_env
      class(out) <- "cea_base"
      results[[alt]] <- out
    }

    class(results) <- "cea_multicomparison"
    results
  }

  # ============================================================
  # 6. Analyze datasets + subgroups (CORRECTED)
  # ============================================================
  analyze_with_subgroups <- function(df, dataset_name) {

    if (verbose)
      message("[Dataset: ", dataset_name, "] Processing subgroups...")

    # ---- CRITICAL FIX: Normalize subgroup variables to factors ----
    if (!is.null(subgroup_vars)) {

      for (v in subgroup_vars) {

        if (is.numeric(df[[v]]) || is.integer(df[[v]])) {

          df[[v]] <- factor(
            df[[v]],
            levels  = sort(unique(df[[v]])),
            ordered = TRUE
          )

        } else {

          df[[v]] <- factor(
            df[[v]],
            levels = unique(df[[v]]),
            ordered = FALSE
          )
        }
      }
    }

    # Overall analysis
    overall <- analyze_data(df, dataset_name)

    # Subgroup analyses
    subs <- NULL
    if (!is.null(subgroup_vars)) {

      subs <- lapply(subgroup_vars, function(v) {

        split_df <- split(df, df[[v]])

        lapply(split_df, function(sub_df)
          analyze_data(sub_df, dataset_name))
      })

      names(subs) <- subgroup_vars
    }

    structure(
      list(Overall = overall, Subgroups = subs),
      class = "cea_results"
    )
  }

  # ============================================================
  # 7. Apply to all datasets
  # ============================================================
  result <- lapply(
    names(data_list),
    function(nm) analyze_with_subgroups(data_list[[nm]], nm)
  )

  names(result) <- names(data_list)
  class(result) <- c("cea_results_list","list")

  # ============================================================
  # 8. Combine replicates + summary stats
  # ============================================================
  result$combined_replicates <- combine_icers_results(result)

  result$summary_stats <- summarize_cea_data(
    data         = data,
    group        = group,
    cost         = cost,
    effect       = effect,
    subgroup_vars = subgroup_vars,
    lambda       = lambda,
    ref_group    = ref_group,
    alt_groups   = alt_groups
  )

  class(result$summary_stats) <- c("summary_stats",
                                   class(result$summary_stats))

  attr(result$summary_stats, "settings_env") <- .settings_env

  # ============================================================
  # 9. Store settings
  # ============================================================
  result$settings <- as.list(.settings_env)

  if (verbose)
    message("\nAnalysis completed successfully.\n")

  return(result)
}
