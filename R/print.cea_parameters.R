
#' Print CEA Parameter Definitions
#'
#' Displays the tabular summary of a cea_parameters object
#' instead of printing its complete nested structure.
#'
#' @param x An object of class \code{cea_parameters}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.cea_parameters <- function(x, ...) {

  if (!inherits(x, "cea_parameters")) {
    stop("'x' must be a 'cea_parameters' object.")
  }

  # Display the parameter summary
  print(summary(x))

  # Preserve the complete object
  invisible(x)
}
