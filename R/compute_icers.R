#' Compute Incremental Cost-Effectiveness Ratios (ICERs) Using Nonparametric Bootstrap
#'
#' Implements a nonparametric bootstrap procedure to estimate incremental cost,
#' incremental effectiveness, ICERs and NMBs for one reference strategy versus
#' alternatives, supporting multi-dataset and subgroup-based analyses.
#'
#' Subgroup variables are normalized automatically. When verbose = TRUE, the
#' function prints global progress (0–100%) accumulated across all bootstrap
#' iterations, with dataset and subgroup headers.
#'
#' @export
compute_icers <- function(
    data, group, cost, effect,
    R = 1000, lambda = 25000,
    ci_type = "bca", subgroup_vars = NULL,
    seed = NULL, verbose = FALSE,
    ref_group, alt_groups = NULL
) {

  # ============================================================
  # 0. Seed + Settings
  # ============================================================
  if (is.null(seed)) seed <- as.integer(Sys.time()) %% 1e6
  set.seed(seed)

  settings_env <- list(
    ref_group     = ref_group,
    alt_groups    = alt_groups,
    lambda        = lambda,
    R             = R,
    ci_type       = ci_type,
    subgroup_vars = subgroup_vars,
    seed          = seed
  )

  # ============================================================
  # 1. Normalize Input
  # ============================================================
  if (is.data.frame(data)) {
    data_list <- list(single_dataset = data)
  } else {
    if (is.null(names(data)))
      names(data) <- paste0("dataset_", seq_along(data))
    data_list <- data
  }

  # ============================================================
  # 2. Count TOTAL ITERATIONS for global verbose
  # ============================================================
  count_iters <- function(df) {
    g <- factor(df[[group]], levels = sort(unique(df[[group]])))
    alts <- if (is.null(alt_groups)) setdiff(levels(g), ref_group)
    else intersect(levels(g), alt_groups)

    n_overall <- length(alts)
    n_sub <- 0
    if (!is.null(subgroup_vars))
      for (s in subgroup_vars)
        n_sub <- n_sub + length(unique(df[[s]])) * length(alts)

    R * (n_overall + n_sub)
  }

  TOTAL_ITER <- sum(sapply(data_list, count_iters))
  global_iter <- 0

  # ============================================================
  # 3. Statistic + CI
  # ============================================================
  ratio_fun <- function(d, i, g, cst, eff, lambda_vec, ref, alt) {
    d <- d[i, ]
    d[[g]] <- factor(d[[g]])
    if (!all(c(ref, alt) %in% unique(d[[g]])))
      return(rep(NA, 7 + length(lambda_vec)))

    cm <- tapply(d[[cst]], d[[g]], mean)
    em <- tapply(d[[eff]], d[[g]], mean)
    dc <- cm[[alt]] - cm[[ref]]
    de <- em[[alt]] - em[[ref]]
    icer <- ifelse(de == 0, NA, dc / de)
    nmb <- sapply(lambda_vec, function(l) l * de - dc)
    c(dc, de, icer, cm[[ref]], cm[[alt]], em[[ref]], em[[alt]], nmb)
  }

  safe_ci <- function(boot_obj, idx, ci_type) {
    ci <- tryCatch(boot::boot.ci(boot_obj, type = ci_type, index = idx),
                   error = function(e) NULL)
    if (is.null(ci)) return(c(NA, NA))
    if (!is.null(ci[[ci_type]])) return(ci[[ci_type]][4:5])
    if (!is.null(ci$bca))        return(ci$bca[4:5])
    if (!is.null(ci$perc))       return(ci$perc[4:5])
    c(NA, NA)
  }

  # ============================================================
  # 4. Bootstrap runner + Global verbose
  # ============================================================
  run_boot_block <- function(df_comp, alt, label, indent = "") {
    boot_res <- boot::boot(
      data = df_comp,
      statistic = function(d, i) {
        global_iter <<- global_iter + 1
        ratio_fun(d, i, group, cost, effect, lambda, ref_group, alt)
      },
      R = R,
      strata = as.numeric(df_comp[[group]])
    )

    if (verbose) {
      pct <- round(100 * global_iter / TOTAL_ITER)
      if (pct > 100) pct <- 100
      message(indent, label, " ... ", pct, "%")
    }

    boot_res
  }

  # ============================================================
  # 5. OVERALL / SUBGROUP analysis
  # ============================================================
  analyze_data <- function(df, dataset_name, verbose_prefix = NULL) {

    df[[group]] <- factor(df[[group]])
    lv <- levels(df[[group]])

    if (!ref_group %in% lv)
      stop("Reference group not found in dataset ", dataset_name)

    alts <- if (is.null(alt_groups)) setdiff(lv, ref_group)
    else intersect(lv, alt_groups)

    results <- list()

    for (alt in alts) {

      df_comp <- df[df[[group]] %in% c(ref_group, alt), , drop = FALSE]
      df_comp[[group]] <- factor(df_comp[[group]], levels = c(ref_group, alt))

      label <- if (is.null(verbose_prefix)) {
        paste0("Overall comparison (", alt, " vs ", ref_group, ")")
      } else {
        paste0(verbose_prefix, " | ", alt, " vs ", ref_group)
      }

      boot_res <- run_boot_block(
        df_comp, alt,
        label  = label,
        indent = "  "
      )

      stat_names <- c("Delta_Cost","Delta_Effect","ICER",
                      "Mean_Cost_ref","Mean_Cost_alt",
                      "Mean_Effect_ref","Mean_Effect_alt",
                      paste0("NMB_", lambda))

      colnames(boot_res$t) <- stat_names
      ci_list <- lapply(seq_along(stat_names), function(j) safe_ci(boot_res, j, ci_type))

      out <- list(
        comparison        = alt,
        bootstrap_samples = as.data.frame(boot_res$t),
        boot_ci           = setNames(ci_list, stat_names),
        t0                = setNames(boot_res$t0, stat_names),
        bias              = colMeans(boot_res$t, na.rm = TRUE) - boot_res$t0
      )

      class(out) <- "cea_base"
      attr(out, "settings_env") <- settings_env
      results[[alt]] <- out
    }

    class(results) <- "cea_multicomparison"
    results
  }

  # ============================================================
  # 6. SUBGROUP analysis
  # ============================================================
  analyze_with_subgroups <- function(df, dataset_name) {

    if (verbose) {
      pct <- round(100 * global_iter / TOTAL_ITER)
      message("[Dataset: ", dataset_name, "] ... ", pct, "%")
    }

    if (!is.null(subgroup_vars)) {
      for (v in subgroup_vars) {
        if (is.numeric(df[[v]]) || is.integer(df[[v]]))
          df[[v]] <- factor(df[[v]], levels = sort(unique(df[[v]])))
        else
          df[[v]] <- factor(df[[v]], levels = unique(df[[v]]))
      }
    }

    overall <- analyze_data(df, dataset_name)

    subs <- NULL
    if (!is.null(subgroup_vars)) {

      subs <- lapply(subgroup_vars, function(v) {

        split_df <- split(df, df[[v]])
        levs <- names(split_df)

        out_list <- lapply(levs, function(lvl) {

          if (verbose) {
            pct <- round(100 * global_iter / TOTAL_ITER)
            message("  Subgroup ", v, " = ", lvl, " ... ", pct, "%")
          }

          prefix <- paste0("Subgroup ", v, " = ", lvl)

          sub_res <- analyze_data(
            split_df[[lvl]],
            dataset_name,
            verbose_prefix = prefix
          )

          attr(sub_res, "subgroup_var")   <- v
          attr(sub_res, "subgroup_level") <- lvl

          sub_res
        })

        names(out_list) <- levs
        out_list
      })

      names(subs) <- subgroup_vars
    }

    structure(list(Overall = overall, Subgroups = subs), class = "cea_results")
  }

  # ============================================================
  # 7. APPLY TO ALL DATASETS
  # ============================================================
  result <- lapply(
    names(data_list),
    function(nm) analyze_with_subgroups(data_list[[nm]], nm)
  )

  names(result) <- names(data_list)
  class(result) <- c("cea_results_list","list")

  # ============================================================
  # 8. Combine + Summary
  # ============================================================
  result$combined_replicates <- combine_icers_results(result)

  result$summary_stats <- summarize_cea_data(
    data          = data,
    group         = group,
    cost          = cost,
    effect        = effect,
    subgroup_vars = subgroup_vars,
    lambda        = lambda,
    ref_group     = ref_group,
    alt_groups    = alt_groups
  )

  class(result$summary_stats) <- c("summary_stats", class(result$summary_stats))
  attr(result$summary_stats, "settings_env") <- settings_env

  # ============================================================
  # 9. Store Settings + Return
  # ============================================================
  result$settings <- settings_env

  if (verbose) message("Analysis completed.")

  return(result)
}
