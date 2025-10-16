#' Combine bootstrap samples from one or several CEA analyses
#'
#' Combines the bootstrap replicates contained in one or more objects of class
#' `cea_results` into a single tidy data frame. The resulting dataset includes
#' all bootstrap samples for the overall analysis and any available subgroup
#' analyses. This function is intended to facilitate plotting and comparison
#' across different analyses.
#'
#' @param ... One or more objects of class `cea_results`, as returned by
#'   \code{\link{icers_base}}.
#' @param labels Optional character vector with custom labels for each object.
#'   If not provided, labels are generated automatically as "set_1", "set_2", etc.
#'
#' @return A tibble containing all bootstrap replicates with the following columns:
#' \itemize{
#'   \item \code{source}: name or label of the originating object.
#'   \item \code{level}: "Overall" or name of the subgroup variable.
#'   \item \code{subgroup}: subgroup level (if applicable).
#'   \item \code{replicate}: bootstrap replicate index.
#'   \item \code{Delta_Cost}, \code{Delta_Effect}, \code{ICER},
#'         \code{Mean_Cost_g1}, \code{Mean_Cost_g2},
#'         \code{Mean_Effect_g1}, \code{Mean_Effect_g2}, \code{NMB}.
#' }
#'
#' @examples
#' data(cua_base)
#' res <- icers_base(
#'   data = cua_base,
#'   group = "group",
#'   cost = "cost_total",
#'   effect = "effect",
#'   R = 100,
#'   lambda = 25000,
#'   subgroup_vars = c("diabetes", "HTA")
#' )
#' df_combined <- combine_icers_results(res)
#' head(df_combined)
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble tibble
#' @export
combine_icers_results <- function(..., labels = NULL) {

  objects <- list(...)
  n_objs <- length(objects)

  if (n_objs == 0) {
    stop("At least one 'cea_results' object must be provided.")
  }

  # Validate class of each object
  if (!all(sapply(objects, inherits, "cea_results"))) {
    stop("All inputs must be of class 'cea_results'.")
  }

  # Assign automatic labels if not provided
  if (is.null(labels)) {
    labels <- paste0("set_", seq_len(n_objs))
  }

  if (length(labels) != n_objs) {
    stop("Length of 'labels' must match the number of provided objects.")
  }

  # Helper to extract bootstrap data from one cea_results object
  extract_bootstrap <- function(res_obj, label) {

    extract_one <- function(x, level, subgroup = NA) {
      if (!inherits(x, "cea_base")) return(NULL)

      df <- x$bootstrap_samples
      df$replicate <- seq_len(nrow(df))
      df$level <- level
      df$subgroup <- subgroup
      df$source <- label
      df
    }

    # Overall
    overall_df <- extract_one(res_obj$Overall, "Overall", NA)

    # Subgroups (if any)
    sub_df <- NULL
    if (!is.null(res_obj$Subgroups)) {
      sub_df <- dplyr::bind_rows(lapply(names(res_obj$Subgroups), function(var) {
        dplyr::bind_rows(lapply(names(res_obj$Subgroups[[var]]), function(lvl) {
          extract_one(res_obj$Subgroups[[var]][[lvl]], var, lvl)
        }))
      }))
    }

    dplyr::bind_rows(overall_df, sub_df)
  }

  # Combine all results
  combined_df <- dplyr::bind_rows(lapply(seq_len(n_objs), function(i) {
    extract_bootstrap(objects[[i]], labels[i])
  }))

  # Reorder columns for readability
  combined_df <- combined_df[, c(
    "source", "level", "subgroup", "replicate",
    "Delta_Cost", "Delta_Effect", "ICER",
    "Mean_Cost_g1", "Mean_Cost_g2",
    "Mean_Effect_g1", "Mean_Effect_g2", "NMB"
  )]

  tibble::as_tibble(combined_df)
}
