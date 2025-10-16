#' Print method for cea_results objects
#'
#' Displays the main summary table of results directly when printing a
#' `cea_results` object. This method automatically calls `summary()` internally
#' and prints the formatted table for both the overall analysis and all
#' subgroup analyses.
#'
#' @param x An object of class `cea_results`.
#' @param digits Number of decimal places to display (default = 3).
#' @param ... Additional arguments (currently unused).
#' @export
print.cea_results <- function(x, digits = 3, ...) {

  # Generate the summary table
  res_summary <- summary(x, digits = digits)

  # Header
  cat("Cost-effectiveness analysis results\n")
  cat("-----------------------------------\n")
  cat("The table below shows mean estimates and 95% confidence intervals\n")
  cat("for the overall sample and all available subgroups.\n\n")

  # Print formatted summary table
  print(res_summary)

  invisible(x)
}
