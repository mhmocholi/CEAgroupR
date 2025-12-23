#' Print Method for \code{cea_base} Objects
#'
#' Displays a formatted summary of bootstrap-based cost-effectiveness results
#' for a single incremental comparison between a reference strategy and one
#' alternative strategy. The printed output includes original estimates,
#' bootstrap means, bias, and percentile-based confidence intervals when
#' available.
#'
#' This method is intended for detailed inspection of a specific comparison
#' extracted from a broader cost-effectiveness analysis, and complements the
#' higher-level summaries provided by \code{print.cea_results}.
#'
#' The reference strategy is retrieved from the internal analysis settings
#' attached to the object by \code{\link{compute_icers}}. The alternative
#' strategy corresponds to the comparison stored in the \code{cea_base} object.
#'
#' Although primarily designed as an internal print method, this function can
#' also be called explicitly by users to control the formatting of the output,
#' in particular the number of decimal places displayed.
#'
#' @param x An object of class \code{cea_base}, representing a single
#'   strategy comparison.
#' @param digits Integer specifying the number of decimal places to display.
#'   Default is 3.
#' @param ... Additional arguments (unused).
#'
#' @details
#' This method is automatically invoked when a \code{cea_base} object is printed
#' to the console. It is most commonly accessed by extracting a single
#' comparison from a \code{cea_results} object.
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @examples
#' ## Example: Inspect a single comparison with higher precision
#' res <- compute_icers(
#'   data      = cua_base,
#'   group     = "group",
#'   cost      = "cost_total",
#'   effect    = "effect",
#'   ref_group = "g0",
#'   R         = 50,
#'   seed      = 123
#' )
#'
#' ## Extract and print one comparison
#' print(res[[1]][["g1"]], digits = 4)
#'
#' @export

print.cea_base <- function(x, digits = 3, ...) {

  # Retrieve settings ----------------------------------------------------------
  settings_env <- attr(x, "settings_env")

  ref_group <- if (!is.null(settings_env$ref_group)) {
    settings_env$ref_group
  } else {
    NA_character_
  }

  alt_group <- if (!is.null(x$comparison)) {
    x$comparison
  } else {
    "Alternative strategy"
  }

  # Validate bootstrap samples -------------------------------------------------
  if (is.null(x$bootstrap_samples)) {
    cat("No bootstrap samples available for this comparison.\n")
    return(invisible(x))
  }

  # Extract quantities ---------------------------------------------------------
  means <- colMeans(x$bootstrap_samples, na.rm = TRUE)
  lower <- sapply(x$boot_ci, function(ci) ci[1])
  upper <- sapply(x$boot_ci, function(ci) ci[2])

  orig <- if (!is.null(x$t0)) x$t0 else rep(NA_real_, length(means))
  bias <- if (!is.null(x$bias)) x$bias else rep(NA_real_, length(means))

  summary_df <- tibble::tibble(
    Parameter = names(means),
    Original  = round(orig,  digits),
    Bias      = round(bias,  digits),
    Mean      = round(means, digits),
    Lower_95  = round(lower, digits),
    Upper_95  = round(upper, digits)
  )

  # Extract lambda values ------------------------------------------------------
  nmb_params <- grep("^NMB_", names(means), value = TRUE)
  lambda_values <- gsub("NMB_", "", nmb_params)

  # Header ---------------------------------------------------------------------
  cat("------------------------------------------------------------\n")
  cat(" Cost-Effectiveness Analysis: Bootstrap Summary\n")
  cat("------------------------------------------------------------\n")

  cat("Reference strategy:  ",
      if (is.na(ref_group)) "Not available" else ref_group, "\n", sep = "")
  cat("Alternative strategy: ", alt_group, "\n", sep = "")

  if (length(lambda_values) > 0) {
    cat("WTP thresholds (lambda): ",
        paste(lambda_values, collapse = ", "), "\n", sep = "")
  }

  cat("\nDisplayed values: original estimate (t0), bootstrap mean, bias, and 95% CI.\n\n")

  # Format numeric columns -----------------------------------------------------
  num_cols <- sapply(summary_df, is.numeric)
  summary_df[num_cols] <- lapply(summary_df[num_cols], function(col) {
    format(
      round(col, digits),
      nsmall = digits,
      justify = "right",
      scientific = FALSE
    )
  })

  summary_df$Parameter <- format(summary_df$Parameter, justify = "left")

  # Display table --------------------------------------------------------------
  print.data.frame(summary_df, row.names = FALSE, right = TRUE)
  cat("\n")

  invisible(x)
}
