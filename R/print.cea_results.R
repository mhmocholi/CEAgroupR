#' Print Method for \code{cea_results} Objects
#'
#' Provides a structured, human-readable overview of cost-effectiveness
#' analysis results stored in a \code{cea_results} object. This print method
#' summarizes bootstrap-based outcomes for both overall population analyses
#' and subgroup-specific analyses, when available.
#'
#' The printed output is intended for quick inspection and interactive use,
#' and is not designed to replace formal tabular summaries or graphical
#' visualizations. Reference strategy information is retrieved from the
#' internal analysis settings attached by \code{\link{compute_icers}}.
#'
#' Although primarily intended as an internal print method, this function
#' can also be called explicitly by users to control the formatting of the
#' output, in particular the number of decimal places displayed.
#'
#' Comparison labels correspond to alternative strategies only, following
#' the standardized conventions used throughout the analytical layer of
#' CEAgroupR.
#'
#' @param x An object of class \code{cea_results}.
#' @param digits Integer specifying the number of decimal places to display.
#'   Default is 3.
#' @param ... Additional arguments (unused).
#'
#' @details
#' This method is automatically invoked when a \code{cea_results} object
#' is printed to the console. It provides a concise summary of key
#' bootstrap-based estimates to facilitate exploratory analysis and
#' diagnostic checks during interactive workflows.
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @examples
#' ## Example: Print results with increased numeric precision
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
#' print(res[[1]], digits = 5)
#'
#' @export

print.cea_results <- function(x, digits = 3, ...) {

  # Retrieve reference strategy -------------------------------------------------
  ref_group <- NA_character_

  if (!is.null(x$Overall) &&
      length(x$Overall) > 0 &&
      !is.null(attr(x$Overall[[1]], "settings_env"))) {

    ref_group <- attr(x$Overall[[1]], "settings_env")$ref_group
  }

  # Header ----------------------------------------------------------------------
  cat("------------------------------------------------------------\n")
  cat(" Cost-Effectiveness Analysis Results\n")
  cat("------------------------------------------------------------\n")

  cat("Reference strategy: ",
      if (is.na(ref_group)) "Not available" else ref_group,
      "\n\n", sep = "")

  # Helper to summarize a single cea_base object -------------------------------
  summarize_one <- function(base_obj, level = "Overall", subgroup = "") {

    if (is.null(base_obj$bootstrap_samples))
      return(NULL)

    means <- colMeans(base_obj$bootstrap_samples, na.rm = TRUE)
    lower <- sapply(base_obj$boot_ci, function(ci) ci[1])
    upper <- sapply(base_obj$boot_ci, function(ci) ci[2])

    tibble::tibble(
      Level      = level,
      Subgroup   = subgroup,
      Comparison = base_obj$comparison,
      Parameter  = names(means),
      Mean       = round(means, digits),
      Lower_95   = round(lower, digits),
      Upper_95   = round(upper, digits)
    )
  }

  results_list <- list()

  # Overall results -------------------------------------------------------------
  if (!is.null(x$Overall)) {

    if (inherits(x$Overall, "cea_base")) {

      results_list[[length(results_list) + 1]] <-
        summarize_one(x$Overall)

    } else if (inherits(x$Overall, "cea_multicomparison")) {

      for (nm in names(x$Overall)) {
        results_list[[length(results_list) + 1]] <-
          summarize_one(x$Overall[[nm]])
      }
    }
  }

  # Subgroup results ------------------------------------------------------------
  if (!is.null(x$Subgroups) && length(x$Subgroups) > 0) {

    for (var in names(x$Subgroups)) {
      for (lvl in names(x$Subgroups[[var]])) {

        obj <- x$Subgroups[[var]][[lvl]]

        if (inherits(obj, "cea_base")) {

          results_list[[length(results_list) + 1]] <-
            summarize_one(obj, level = var, subgroup = lvl)

        } else if (inherits(obj, "cea_multicomparison")) {

          for (nm in names(obj)) {
            results_list[[length(results_list) + 1]] <-
              summarize_one(obj[[nm]], level = var, subgroup = lvl)
          }
        }
      }
    }
  }

  # Combine results -------------------------------------------------------------
  results_tbl <- dplyr::bind_rows(results_list)

  if (nrow(results_tbl) == 0) {
    cat("No bootstrap summary data available.\n")
    return(invisible(x))
  }

  # Formatting ------------------------------------------------------------------
  num_cols <- sapply(results_tbl, is.numeric)

  results_tbl[num_cols] <- lapply(results_tbl[num_cols], function(col) {
    format(
      round(col, digits),
      nsmall = digits,
      justify = "right",
      scientific = FALSE
    )
  })

  results_tbl[!num_cols] <- lapply(results_tbl[!num_cols], function(col) {
    format(col, justify = "left")
  })

  print.data.frame(results_tbl, row.names = FALSE, right = TRUE)
  cat("\n")

  invisible(x)
}
