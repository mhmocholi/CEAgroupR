#' Summarize results from a CEA bootstrap analysis
#'
#' Produces a tidy summary table of mean estimates and 95% confidence intervals
#' for each outcome parameter (overall and by subgroup).
#'
#' @param object An object of class `cea_results`.
#' @param digits Number of decimal places (default = 3).
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with numeric results (class: summary_cea_results).
#'
#' @export
summary.cea_results <- function(object, digits = 3, ...) {

  extract_summary <- function(x, level, subgroup = NA) {
    if (!inherits(x, "cea_base")) return(NULL)

    means <- colMeans(x$bootstrap_samples, na.rm = TRUE)
    ci_df <- do.call(rbind, x$boot_ci)

    tibble::tibble(
      Level = level,
      Subgroup = subgroup,
      Parameter = names(means),
      Mean = round(means, digits),
      Lower_95 = round(ci_df[, 1], digits),
      Upper_95 = round(ci_df[, 2], digits)
    )
  }

  results_overall <- extract_summary(object$Overall, "Overall", NA)

  results_subgroups <- NULL
  if (!is.null(object$Subgroups)) {
    results_subgroups <- purrr::map_dfr(names(object$Subgroups), function(var) {
      purrr::map_dfr(names(object$Subgroups[[var]]), function(lvl) {
        extract_summary(object$Subgroups[[var]][[lvl]], var, lvl)
      })
    })
  }

  out <- dplyr::bind_rows(results_overall, results_subgroups)

  # Añadimos una clase especial para su impresión formateada
  class(out) <- c("summary_cea_results", class(out))
  attr(out, "digits") <- digits
  out
}
