#' Print method for cea_base objects
#'
#' Provides a concise and clean console output for `cea_base` objects.
#' This print method hides the internal `boot` call and dataset details,
#' displaying only the relevant summary of the bootstrap analysis.
#'
#' @param x An object of class `cea_base`.
#' @param ... Additional arguments (not used).
#' @export
print.cea_base <- function(x, ...) {
  cat("CEA bootstrap analysis\n")
  cat("----------------------\n")

  # Show number of bootstrap replications
  if (!is.null(x$boot_results$R)) {
    cat("Bootstrap replications:", x$boot_results$R, "\n\n")
  }

  # Show estimated parameters
  if (!is.null(colnames(x$boot_results$t))) {
    params <- colnames(x$boot_results$t)
    cat("Estimated parameters:\n")
    cat(paste(params, collapse = ", "), "\n\n")
  }

  # Guidance for the user
  cat("Use summary() for detailed results.\n")

  invisible(x)
}
