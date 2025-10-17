#' Print method for cea_results objects
#'
#' Displays the main summary table of results directly when printing a
#' `cea_results` object. It extracts means and 95% confidence intervals
#' from the stored bootstrap results for both the overall and subgroup analyses.
#'
#' @param x An object of class `cea_results`.
#' @param digits Number of decimal places to display (default = 3).
#' @param ... Additional arguments (currently unused).
#' @export
print.cea_results <- function(x, digits = 3, ...) {

  # ---- Helper: extract mean and CI from cea_base ----
  summarize_from_object <- function(base_obj, level = "Overall", subgroup = "") {
    if (is.null(base_obj$bootstrap_samples)) return(NULL)

    # Means
    means <- colMeans(base_obj$bootstrap_samples, na.rm = TRUE)

    # Confidence intervals
    cis <- base_obj$boot_ci
    lower <- sapply(cis, function(ci) ci[1])
    upper <- sapply(cis, function(ci) ci[2])

    # Build data frame
    tibble::tibble(
      Level = level,
      Subgroup = subgroup,
      Parameter = names(means),
      Mean = round(means, digits),
      Lower_95 = round(lower, digits),
      Upper_95 = round(upper, digits)
    )
  }

  # ---- Collect results for overall and subgroups ----
  res_all <- list()

  # Overall
  if (!is.null(x$Overall)) {
    res_all[[length(res_all) + 1]] <- summarize_from_object(x$Overall, "Overall", "")
  }

  # Subgroups
  if (!is.null(x$Subgroups) && length(x$Subgroups) > 0) {
    for (var in names(x$Subgroups)) {
      for (lvl in names(x$Subgroups[[var]])) {
        sub_obj <- x$Subgroups[[var]][[lvl]]
        res_all[[length(res_all) + 1]] <- summarize_from_object(sub_obj, var, lvl)
      }
    }
  }

  # ---- Combine safely ----
  res_summary <- dplyr::bind_rows(res_all)

  # ---- Header ----
  cat("Cost-effectiveness analysis results\n")
  cat("-----------------------------------\n")
  cat("The table below shows mean estimates and 95% confidence intervals\n")
  cat("for the overall sample and all available subgroups.\n\n")

  # ---- Format numeric columns ----
  num_cols <- sapply(res_summary, is.numeric)
  if (any(num_cols)) {
    res_summary[num_cols] <- lapply(
      res_summary[num_cols],
      function(col) format(round(col, digits), nsmall = digits, justify = "right")
    )
  }

  # ---- Format character columns ----
  char_cols <- !num_cols
  res_summary[char_cols] <- lapply(
    res_summary[char_cols],
    function(col) format(col, justify = "left")
  )

  # ---- Print nicely ----
  print.data.frame(res_summary, row.names = FALSE, right = TRUE)
  cat("\n")
  invisible(x)
}
