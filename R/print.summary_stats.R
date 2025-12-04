#' Print method for summary statistics of CEA datasets
#'
#' Produces three compact tables per dataset:
#'   1) Sample sizes
#'   2) Mean and SD of costs and effects
#'   3) Incremental outcomes (Delta Cost, Delta Effect, ICER, NMB)
#'
#' Each row corresponds to one combination:
#'   subgroup_var × subgroup_level × comparison
#'
#' @param x Tibble from summarize_cea_data().
#' @param digits Number of decimal places (except n_ref/n_alt which are integers).
#' @param ... Unused.
#' @export
print.summary_stats <- function(x, digits = 3, ...) {

  if (is.null(x) || nrow(x) == 0) {
    cat("No summary statistics available.\n")
    return(invisible(x))
  }

  df <- as.data.frame(x)

  # Replace missing subgroup levels
  if ("subgroup_level" %in% names(df))
    df$subgroup_level[is.na(df$subgroup_level)] <- "Overall"

  # Extract NMB columns
  nmb_cols <- grep("^NMB_", names(df), value = TRUE)

  # ---- Safe ref_group detection (CRAN-friendly, knitting-safe) ----
  .find_ref_group <- function(x) {

    # 1) If settings_env exists, use it directly
    env <- attr(x, "settings_env")
    if (!is.null(env) && !is.null(env$ref_group)) {
      return(env$ref_group)
    }

    # 2) If comparison column exists, inference is ambiguous → return NA safely
    if ("comparison" %in% names(x)) {
      return(NA_character_)
    }

    # 3) Fallback
    return(NA_character_)
  }

  ref_group <- .find_ref_group(x)

  # ---- List of datasets ----
  datasets <- unique(df$dataset)

  for (ds in datasets) {

    df_ds <- df[df$dataset == ds, , drop = FALSE]

    # --------------------------------------------------------------------------
    # HEADER
    # --------------------------------------------------------------------------
    cat("Dataset: ", ds, "\n", sep = "")
    cat("Reference group: ",
        if (is.na(ref_group)) "Not available" else ref_group, "\n", sep = "")
    cat("------------------------------------------------------------\n\n")

    # ========================================================================
    # TABLE 1 — SAMPLE SIZES
    # ========================================================================
    cat("SAMPLE SIZES\n")
    cat("------------------------------------------------------------\n")

    samp_cols <- c("subgroup_var","subgroup_level","comparison","n_ref","n_alt")
    samp_cols <- intersect(samp_cols, names(df_ds))
    samp <- df_ds[, samp_cols, drop = FALSE]

    int_cols <- intersect(c("n_ref","n_alt"), names(samp))
    num_cols <- sapply(samp, is.numeric)

    samp <- lapply(names(samp), function(coln) {
      col <- samp[[coln]]
      if (coln %in% int_cols) {
        format(as.integer(col), justify = "right")
      } else if (is.numeric(col)) {
        format(round(col, digits), nsmall = digits, justify = "right")
      } else {
        format(col, justify = "left")
      }
    })
    samp <- as.data.frame(samp, col.names = samp_cols)

    print.data.frame(samp, row.names = FALSE, right = TRUE)
    cat("\n\n")

    # ========================================================================
    # TABLE 2 — MEANS AND SD
    # ========================================================================
    cat("MEANS AND SD (Costs and Effects)\n")
    cat("------------------------------------------------------------\n")

    desc_cols <- c(
      "subgroup_var","subgroup_level","comparison",
      "mean_cost_ref","sd_cost_ref","mean_cost_alt","sd_cost_alt",
      "mean_eff_ref","sd_eff_ref","mean_eff_alt","sd_eff_alt"
    )
    desc_cols <- intersect(desc_cols, names(df_ds))
    desc <- df_ds[, desc_cols, drop = FALSE]

    num_cols <- sapply(desc, is.numeric)
    desc[num_cols] <- lapply(desc[num_cols], function(col)
      format(round(col, digits), nsmall = digits, justify = "right")
    )
    desc[!num_cols] <- lapply(desc[!num_cols], format)

    print.data.frame(desc, row.names = FALSE, right = TRUE)
    cat("\n\n")

    # ========================================================================
    # TABLE 3 — INCREMENTAL OUTCOMES
    # ========================================================================
    cat("INCREMENTAL OUTCOMES\n")
    cat("------------------------------------------------------------\n")

    inc_cols <- c(
      "subgroup_var","subgroup_level","comparison",
      "delta_cost","delta_effect","ICER", nmb_cols
    )
    inc_cols <- intersect(inc_cols, names(df_ds))
    inc <- df_ds[, inc_cols, drop = FALSE]

    num_cols <- sapply(inc, is.numeric)
    inc[num_cols] <- lapply(inc[num_cols], function(col)
      format(round(col, digits), nsmall = digits, justify = "right")
    )
    inc[!num_cols] <- lapply(inc[!num_cols], format)

    print.data.frame(inc, row.names = FALSE, right = TRUE)
    cat("\n\n")
  }

  invisible(x)
}
