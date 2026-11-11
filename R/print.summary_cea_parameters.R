
#' Print CEA Parameter Summary
#'
#' Displays deterministic, probabilistic and scenario
#' parameter tables in a readable console format.
#'
#' @param x An object of class \code{summary_cea_parameters}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.summary_cea_parameters <- function(x, ...) {

  if (!inherits(x, "summary_cea_parameters")) {
    stop("'x' must be a 'summary_cea_parameters' object.")
  }

  titles <- c(
    deterministic = "DETERMINISTIC PARAMETERS",
    probabilistic = "PROBABILISTIC PARAMETERS",
    scenarios = "SCENARIO PARAMETERS"
  )

  cat("\nCEA PARAMETER SUMMARY\n")
  cat(strrep("=", 60), "\n", sep = "")

  for (specification in names(titles)) {

    table <- x[[specification]]

    if (is.null(table)) {
      next
    }

    cat("\n", titles[[specification]], "\n", sep = "")
    cat(strrep("-", 60), "\n", sep = "")

    print(table, row.names = FALSE, na.print = "-")
    cat("\n")
  }

  invisible(x)
}
