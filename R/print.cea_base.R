#' Print method for CEA base objects
#'
#' Provides a compact and readable summary of bootstrap results
#' (ICER, cost, effect, and NMB) using the simplified object structure.
#'
#' @param x An object of class `cea_base`.
#' @param digits Number of digits to display (default = 3).
#' @param ... Additional arguments (ignored).
#' @export
print.cea_base <- function(x, digits = 3, ...) {
  cat("STRATIFIED BOOTSTRAP SUMMARY\n\n")

  # ---- Validate object ----
  if (is.null(x$bootstrap_samples) || nrow(x$bootstrap_samples) == 0) {
    cat("No bootstrap samples available.\n")
    return(invisible(x))
  }

  # ---- Extract bootstrap samples ----
  t_reps <- x$bootstrap_samples

  # ---- Compute summary statistics ----
  t0 <- colMeans(t_reps, na.rm = TRUE)
  boot_bias <- rep(NA, length(t0))
  boot_se   <- apply(t_reps, 2, sd, na.rm = TRUE)

  # ---- Format table ----
  stats <- data.frame(
    Statistic = names(t0),
    Mean = round(t0, digits),
    StdError = round(boot_se, digits)
  )

  # ---- Format and align ----
  stats$Statistic <- format(stats$Statistic, justify = "left")
  stats$Mean <- format(round(stats$Mean, digits), nsmall = digits, justify = "right")
  stats$StdError <- format(round(stats$StdError, digits), nsmall = digits, justify = "right")

  # ---- Print ----
  print(stats, row.names = FALSE, right = TRUE)
  cat("\n")
  invisible(x)
}
