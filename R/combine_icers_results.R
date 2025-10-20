#' Combine bootstrap samples from one or several CEA analyses
#'
#' Combines the bootstrap replicates contained in one or more objects of class
#' `cea_results` or `cea_results_list` into a single tidy data frame. The resulting
#' dataset includes all bootstrap samples for the overall analysis and any available
#' subgroup analyses.
#'
#' @param ... One or more objects of class `cea_results` or a single object of class
#'   `cea_results_list`, as returned by \code{\link{icers_base}}.
#'
#' @return A tibble containing all bootstrap replicates with the following columns:
#' \itemize{
#'   \item \code{dataset}: dataset name (if multiple datasets were analysed).
#'   \item \code{level}: "Overall" or name of the subgroup variable.
#'   \item \code{subgroup}: subgroup level (if applicable).
#'   \item \code{replicate}: bootstrap replicate index.
#'   \item \code{Delta_Cost}, \code{Delta_Effect}, \code{ICER},
#'         \code{Mean_Cost_g1}, \code{Mean_Cost_g2},
#'         \code{Mean_Effect_g1}, \code{Mean_Effect_g2}, \code{NMB}.
#' }
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble tibble
#' @export
combine_icers_results <- function(...) {

  objects <- list(...)
  n_objs <- length(objects)
  if (n_objs == 0) stop("At least one object must be provided.")

  # If a single object is a cea_results_list, unpack it
  if (n_objs == 1 && inherits(objects[[1]], "cea_results_list")) {
    objects <- objects[[1]]
    n_objs <- length(objects)
  }

  # Remove any non-result elements such as $settings
  objects <- Filter(function(x)
    is.list(x) && ("Overall" %in% names(x) || "Subgroups" %in% names(x)),
    objects)

  if (length(objects) == 0)
    stop("No valid 'cea_results' objects found for combination.")

  # Helper to extract bootstrap samples from one cea_results object
  extract_bootstrap <- function(res_obj, dataset_name) {

    extract_one <- function(x, level, subgroup = NA) {
      if (is.null(x$bootstrap_samples)) return(NULL)
      df <- x$bootstrap_samples
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$replicate <- seq_len(nrow(df))
      df$level <- level
      df$subgroup <- subgroup
      df$dataset <- dataset_name
      df
    }

    # Overall
    overall_df <- extract_one(res_obj$Overall, "Overall", NA)

    # Subgroups
    sub_df <- NULL
    if (!is.null(res_obj$Subgroups) && length(res_obj$Subgroups) > 0) {
      sub_df <- dplyr::bind_rows(lapply(names(res_obj$Subgroups), function(var) {
        dplyr::bind_rows(lapply(names(res_obj$Subgroups[[var]]), function(lvl) {
          extract_one(res_obj$Subgroups[[var]][[lvl]], var, lvl)
        }))
      }))
    }

    dplyr::bind_rows(overall_df, sub_df)
  }

  # Combine all datasets
  combined_df <- dplyr::bind_rows(lapply(names(objects), function(ds_name) {
    extract_bootstrap(objects[[ds_name]], ds_name)
  }))

  # Reorder columns
  col_order <- c(
    "dataset", "level", "subgroup", "replicate",
    "Delta_Cost", "Delta_Effect", "ICER",
    "Mean_Cost_g1", "Mean_Cost_g2",
    "Mean_Effect_g1", "Mean_Effect_g2", "NMB"
  )
  existing_cols <- intersect(col_order, names(combined_df))
  combined_df <- combined_df[, existing_cols]

  tibble::as_tibble(combined_df)
}
