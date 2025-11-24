#' Print method for cea_base objects
#'
#' Displays bootstrap summary statistics for a single cost-effectiveness
#' comparison between the reference strategy and one alternative strategy.
#'
#' The comparison label contains only the name of the alternative strategy
#' for clarity. The reference strategy is retrieved from a shared analysis
#' environment (`settings_env`) attached to each comparison object by
#' \code{compute_icers()}, avoiding duplication of metadata while ensuring
#' consistent reporting across the analytical layer.
#'
#' Reported values include:
#' - Original point estimates
#' - Bootstrap means
#' - Bias
#' - 95% confidence intervals (when available)
#'
#' @param x An object of class \code{cea_base}.
#' @param digits Integer, number of decimal places to display (default = 3).
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.cea_base <- function(x, digits = 3, ...) {

  # ---- Retrieve settings environment ----
  settings_env <- attr(x, "settings_env")
  ref_group <- if (!is.null(settings_env$ref_group))
    settings_env$ref_group else NA

  alt_group <- if (!is.null(x$comparison))
    x$comparison else "Alternative strategy"

  # ---- Validate availability of bootstrap samples ----
  if (is.null(x$bootstrap_samples)) {
    cat("No bootstrap samples available in this object.\n")
    return(invisible(x))
  }

  # ---- Extract bootstrap quantities ----
  means  <- colMeans(x$bootstrap_samples, na.rm = TRUE)
  lower  <- sapply(x$boot_ci, function(ci) ci[1])
  upper  <- sapply(x$boot_ci, function(ci) ci[2])
  orig   <- if (!is.null(x$t0))  x$t0  else rep(NA, length(means))
  bias   <- if (!is.null(x$bias)) x$bias else rep(NA, length(means))

  summary_df <- tibble::tibble(
    Parameter = names(means),
    Original  = round(orig,  digits),
    Bias      = round(bias,  digits),
    Mean      = round(means, digits),
    Lower_95  = round(lower, digits),
    Upper_95  = round(upper, digits)
  )

  # ---- Extract lambda values ----
  nmb_params <- grep("^NMB_", names(means), value = TRUE)
  lambda_values <- gsub("NMB_", "", nmb_params)

  # ---- Print header ----
  cat("------------------------------------------------------------\n")
  cat(" Cost-Effectiveness Analysis: Bootstrap Summary\n")
  cat("------------------------------------------------------------\n")

  cat("Reference strategy: ",
      ifelse(is.na(ref_group), "Not available", ref_group), "\n", sep = "")
  cat("Alternative strategy: ", alt_group, "\n", sep = "")

  if (length(lambda_values) > 0) {
    cat("Willingness-to-pay thresholds (λ): ",
        paste(lambda_values, collapse = ", "), "\n", sep = "")
  }

  cat("\nDisplayed values: Original estimates, bootstrap means, bias, and 95% CIs.\n\n")

  # ---- Format numeric columns ----
  num_cols <- sapply(summary_df, is.numeric)
  summary_df[num_cols] <- lapply(summary_df[num_cols], function(col)
    format(round(col, digits),
           nsmall = digits,
           justify = "right",
           scientific = FALSE))

  summary_df$Parameter <- format(summary_df$Parameter, justify = "left")

  # ---- Display table ----
  print.data.frame(summary_df, row.names = FALSE, right = TRUE)
  cat("\n")

  invisible(x)
}
