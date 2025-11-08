#' Print method for cea_base objects
#'
#' Displays the main bootstrap summary statistics for a single cost-effectiveness
#' analysis comparing two strategies (reference vs. intervention). The function
#' reports mean estimates and 95% confidence intervals (95% CI), together with
#' the original point estimates and bootstrap bias for each parameter.
#'
#' @param x An object of class \code{cea_base}.
#' @param digits Integer. Number of decimal places to display (default = 3).
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x} after printing its summary.
#' @importFrom tibble tibble
#' @export
print.cea_base <- function(x, digits = 3, ...) {

  if (is.null(x$bootstrap_samples)) {
    cat("No bootstrap samples available in this object.\n")
    return(invisible(x))
  }

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

  nmb_params <- grep("^NMB_", names(means), value = TRUE)
  lambda_values <- gsub("NMB_", "", nmb_params)

  cat("------------------------------------------------------------\n")
  cat(" Cost-Effectiveness Analysis (Bootstrap Summary)\n")
  cat("------------------------------------------------------------\n")
  if (length(lambda_values) > 0) {
    cat("Willingness-to-pay thresholds (λ): ",
        paste(lambda_values, collapse = ", "), "\n", sep = "")
  }
  cat("\nDisplayed values: Original estimates, bootstrap means and 95% CIs.\n\n")

  num_cols <- sapply(summary_df, is.numeric)
  summary_df[num_cols] <- lapply(summary_df[num_cols], function(col)
    format(round(col, digits),
           nsmall = digits,
           justify = "right",
           scientific = FALSE))
  summary_df$Parameter <- format(summary_df$Parameter, justify = "left")

  print.data.frame(summary_df, row.names = FALSE, right = TRUE)
  cat("\n")

  invisible(x)
}
