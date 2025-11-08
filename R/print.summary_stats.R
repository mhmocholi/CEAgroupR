#' Print method for summary statistics of CEA datasets
#'
#' Formats and prints descriptive summaries produced by
#' \code{\link{summarize_cea_data}}. Compatible with the output of
#' \code{\link{compute_icers}}, displaying also stored analysis settings
#' if available.
#'
#' @param x A tibble produced by \code{\link{summarize_cea_data}}.
#' @param digits Integer. Number of decimal places to display (default = 3).
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.summary_stats <- function(x, digits = 3, ...) {

  if (is.null(x) || nrow(x) == 0) {
    cat("No summary statistics available.\n")
    return(invisible(x))
  }

  df <- as.data.frame(x)
  if ("subgroup_level" %in% names(df)) {
    df$subgroup_level[is.na(df$subgroup_level)] <- ""
  }

  nmb_cols <- grep("^NMB_", names(df), value = TRUE)
  lambda_vals <- gsub("NMB_", "", nmb_cols)

  cat("------------------------------------------------------------\n")
  cat(" Summary Statistics of Input Data\n")
  cat("------------------------------------------------------------\n")
  if (length(lambda_vals) > 0) {
    cat("Willingness-to-pay thresholds (λ): ",
        paste(lambda_vals, collapse = ", "), "\n", sep = "")
  }
  cat("\n")

  mean_cols <- grep("^mean_", names(df), value = TRUE)
  for (m in mean_cols) {
    sd_name <- sub("mean_", "sd_", m)
    if (!sd_name %in% names(df)) df[[sd_name]] <- NA
  }

  base_cols <- c(
    "subgroup_var", "subgroup_level", "n_g0", "n_g1",
    "mean_cost_g0", "sd_cost_g0", "mean_cost_g1", "sd_cost_g1",
    "mean_eff_g0", "sd_eff_g0", "mean_eff_g1", "sd_eff_g1",
    "delta_cost", "delta_effect", "ICER"
  )
  cols_to_show <- c(base_cols, nmb_cols)
  cols_to_show <- intersect(cols_to_show, names(df))

  num_cols <- sapply(df, is.numeric)
  int_cols <- intersect(c("n_g0", "n_g1"), names(df))
  df[num_cols] <- lapply(names(df)[num_cols], function(col_name) {
    col <- df[[col_name]]
    if (col_name %in% int_cols) {
      format(as.integer(round(col)), justify = "right", scientific = FALSE)
    } else {
      format(round(col, digits),
             nsmall = digits,
             justify = "right",
             scientific = FALSE)
    }
  })
  char_cols <- !num_cols
  df[char_cols] <- lapply(df[char_cols], function(col)
    format(col, justify = "left"))

  for (ds in unique(df$dataset)) {
    cat("Dataset:", ds, "\n")
    cat("------------------------------------------------------------\n")
    sub_df <- df[df$dataset == ds, , drop = FALSE]
    sub_df_print <- sub_df[, cols_to_show, drop = FALSE]
    print.data.frame(sub_df_print, row.names = FALSE, right = TRUE)
    cat("\n")
  }

  invisible(x)
}
