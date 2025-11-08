#' Print method for cea_results objects
#'
#' Displays a summary table of bootstrap results for each dataset and subgroup.
#' If available, the method also prints information stored in the analysis
#' settings (seed, reference group, date, etc.).
#'
#' @param x An object of class \code{cea_results}, typically one element of the
#'   list returned by \code{\link{compute_icers}}.
#' @param digits Integer. Number of decimal places to display (default = 3).
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x} after printing its summary.
#' @export
print.cea_results <- function(x, digits = 3, ...) {

  summarize_from_object <- function(base_obj, level = "Overall", subgroup = "") {
    if (is.null(base_obj$bootstrap_samples)) return(NULL)
    means <- colMeans(base_obj$bootstrap_samples, na.rm = TRUE)
    cis <- base_obj$boot_ci
    lower <- sapply(cis, function(ci) ci[1])
    upper <- sapply(cis, function(ci) ci[2])
    tibble::tibble(
      Level = level,
      Subgroup = subgroup,
      Parameter = names(means),
      Mean = round(means, digits),
      Lower_95 = round(lower, digits),
      Upper_95 = round(upper, digits)
    )
  }

  res_all <- list()
  if (!is.null(x$Overall)) {
    res_all[[length(res_all) + 1]] <- summarize_from_object(x$Overall, "Overall", "")
  }
  if (!is.null(x$Subgroups) && length(x$Subgroups) > 0) {
    for (var in names(x$Subgroups)) {
      for (lvl in names(x$Subgroups[[var]])) {
        sub_obj <- x$Subgroups[[var]][[lvl]]
        res_all[[length(res_all) + 1]] <- summarize_from_object(sub_obj, var, lvl)
      }
    }
  }

  res_summary <- dplyr::bind_rows(res_all)
  if (nrow(res_summary) == 0) {
    cat("No bootstrap summary data available.\n")
    return(invisible(x))
  }

  nmb_params <- grep("^NMB_", res_summary$Parameter, value = TRUE)
  lambda_values <- unique(gsub("NMB_", "", nmb_params))

  cat("------------------------------------------------------------\n")
  cat(" Cost-Effectiveness Analysis Results\n")
  cat("------------------------------------------------------------\n")

  if (!is.null(attr(x, "settings"))) {
    s <- attr(x, "settings")
  } else if (!is.null(x$settings)) {
    s <- x$settings
  } else {
    s <- NULL
  }

  if (!is.null(s)) {
    if (!is.null(s$ref_group)) cat("Reference group: ", s$ref_group, "\n", sep = "")
    if (!is.null(s$seed)) cat("Seed: ", s$seed, "\n", sep = "")
    if (!is.null(s$date)) cat("Date: ", format(s$date, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
    if (!is.null(s$lambda)) cat("λ thresholds: ", paste(s$lambda, collapse = ", "), "\n", sep = "")
    cat("\n")
  } else {
    if (length(lambda_values) > 0) {
      cat("Willingness-to-pay thresholds (λ): ",
          paste(lambda_values, collapse = ", "), "\n", sep = "")
    }
    cat("\n")
  }

  cat("Displayed values are bootstrap means and 95% confidence intervals.\n\n")

  num_cols <- sapply(res_summary, is.numeric)
  res_summary[num_cols] <- lapply(res_summary[num_cols], function(col)
    format(round(col, digits),
           nsmall = digits,
           justify = "right",
           scientific = FALSE))
  char_cols <- !num_cols
  res_summary[char_cols] <- lapply(res_summary[char_cols], function(col)
    format(col, justify = "left"))

  print.data.frame(res_summary, row.names = FALSE, right = TRUE)
  cat("\n")

  invisible(x)
}
