#' Print method for cea_results objects
#'
#' Displays a structured overview of all cost-effectiveness comparisons
#' (overall and subgroup-specific) stored within a \code{cea_results} object.
#'
#' The reference strategy is obtained from the shared analysis environment
#' (`settings_env`) created by \code{compute_icers()}. Comparison labels are
#' simplified to the name of the alternative strategy only, following the
#' standardized naming convention used throughout the analytical layer.
#'
#' @param x An object of class \code{cea_results}.
#' @param digits Integer number of decimal places to display (default = 3).
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#' @export
print.cea_results <- function(x, digits = 3, ...) {

  # ---- Retrieve reference strategy from settings ----
  ref_group <- NA
  if (!is.null(attr(x$Overall[[1]], "settings_env"))) {
    ref_group <- attr(x$Overall[[1]], "settings_env")$ref_group
  }

  cat("------------------------------------------------------------\n")
  cat(" Cost-Effectiveness Analysis Results\n")
  cat("------------------------------------------------------------\n")

  cat("Reference strategy: ",
      ifelse(is.na(ref_group), "Not available", ref_group), "\n\n", sep = "")

  # ---- Helper to summarize a cea_base object ----
  summarize_one <- function(base_obj, level = "Overall", subgroup = "") {

    if (is.null(base_obj$bootstrap_samples)) return(NULL)

    means <- colMeans(base_obj$bootstrap_samples, na.rm = TRUE)
    ci_list <- base_obj$boot_ci
    lower <- sapply(ci_list, function(ci) ci[1])
    upper <- sapply(ci_list, function(ci) ci[2])

    tibble::tibble(
      Level     = level,
      Subgroup  = subgroup,
      Comparison = base_obj$comparison,
      Parameter  = names(means),
      Mean       = round(means, digits),
      Lower_95   = round(lower, digits),
      Upper_95   = round(upper, digits)
    )
  }

  res_all <- list()

  # ---- Overall results ----
  if (!is.null(x$Overall)) {

    if (inherits(x$Overall, "cea_base")) {
      res_all[[length(res_all) + 1]] <- summarize_one(x$Overall)
    }

    if (inherits(x$Overall, "cea_multicomparison")) {
      for (nm in names(x$Overall)) {
        res_all[[length(res_all) + 1]] <- summarize_one(x$Overall[[nm]])
      }
    }
  }

  # ---- Subgroup results ----
  if (!is.null(x$Subgroups) && length(x$Subgroups) > 0) {
    for (var in names(x$Subgroups)) {
      for (lvl in names(x$Subgroups[[var]])) {
        obj <- x$Subgroups[[var]][[lvl]]

        if (inherits(obj, "cea_base")) {
          res_all[[length(res_all) + 1]] <-
            summarize_one(obj, level = var, subgroup = lvl)
        }

        if (inherits(obj, "cea_multicomparison")) {
          for (nm in names(obj)) {
            res_all[[length(res_all) + 1]] <-
              summarize_one(obj[[nm]], level = var, subgroup = lvl)
          }
        }
      }
    }
  }

  # ---- Consolidate printed table ----
  res_table <- dplyr::bind_rows(res_all)

  if (nrow(res_table) == 0) {
    cat("No bootstrap summary data available.\n")
    return(invisible(x))
  }

  # Numeric formatting
  num_cols <- sapply(res_table, is.numeric)
  res_table[num_cols] <- lapply(res_table[num_cols], function(col)
    format(round(col, digits),
           nsmall = digits,
           justify = "right",
           scientific = FALSE)
  )

  # Character spacing
  res_table[!num_cols] <- lapply(res_table[!num_cols], function(col)
    format(col, justify = "left")
  )

  print.data.frame(res_table, row.names = FALSE, right = TRUE)
  cat("\n")

  invisible(x)
}
