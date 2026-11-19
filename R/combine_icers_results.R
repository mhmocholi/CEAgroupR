
#' Combine Bootstrap Replicates Across CEA Datasets
#'
#' Extracts and combines bootstrap replicates from one or more
#' cost-effectiveness analysis result objects into a single data frame.
#'
#' The function supports overall analyses, subgroup analyses, and
#' multiple intervention comparisons. Each replicate is identified
#' by its dataset, subgroup, comparison, and simulation method.
#'
#' @param ... One or more objects of class `cea_results`, or a
#'   `cea_results_list` containing individual CEA result objects.
#'
#' @return A tibble containing the combined bootstrap replicates,
#'   including dataset identifiers, subgroup information, intervention
#'   comparisons, replicate numbers, simulation method (`computed_by`),
#'   unique group identifiers (`group_uid`), and available economic
#'   evaluation statistics.
#'
#' @details
#' All extracted replicates are identified as bootstrap simulations.
#' The function preserves the original economic evaluation statistics
#' and constructs hierarchical identifiers to distinguish results
#' across datasets, subgroups, and comparisons.
#'
#' @seealso [combine_boot_psa()]
#'
#' @export

combine_icers_results <- function(...) {

  objects <- list(...)
  n_objs <- length(objects)
  if (n_objs == 0) stop("At least one object must be provided.")

  # ---------------------------------------------------------------
  # 0. Unpack cea_results_list if necessary
  # ---------------------------------------------------------------
  if (n_objs == 1 && inherits(objects[[1]], "cea_results_list")) {
    objects <- objects[[1]]
    n_objs <- length(objects)
  }

  # Keep only objects that look like cea_results
  objects <- Filter(function(x)
    is.list(x) && ("Overall" %in% names(x) || "Subgroups" %in% names(x)),
    objects)

  if (length(objects) == 0)
    stop("No valid 'cea_results' objects found for combination.")

  # ---------------------------------------------------------------
  # 1. Helper: extract bootstrap samples from a single cea_results
  # ---------------------------------------------------------------
  extract_bootstrap <- function(res_obj, dataset_name) {

    # ---- Helper to extract from a cea_base ----
    extract_one <- function(x, subgroup_var = NA, subgroup_level = NA, comparison = NA) {

      if (is.null(x$bootstrap_samples)) return(NULL)
      df <- x$bootstrap_samples
      if (nrow(df) == 0) return(NULL)

      df$replicate <- seq_len(nrow(df))
      df$dataset   <- dataset_name

      # Normalize subgroup_var
      df$subgroup_var <- ifelse(is.na(subgroup_var), "Overall", subgroup_var)

      # Normalize subgroup_level (always factor)
      if (is.na(subgroup_level)) {
        df$subgroup_level <- factor("Overall")
      } else {
        # Keep factor ordering if supplied
        if (is.factor(subgroup_level)) {
          df$subgroup_level <- subgroup_level
        } else if (is.numeric(subgroup_level) || is.integer(subgroup_level)) {
          df$subgroup_level <- factor(
            as.character(subgroup_level),
            levels = sort(unique(as.character(subgroup_level))),
            ordered = TRUE
          )
        } else {
          df$subgroup_level <- factor(
            as.character(subgroup_level),
            levels = unique(as.character(subgroup_level)),
            ordered = FALSE
          )
        }
      }

      # Normalize comparison
      df$comparison <- ifelse(is.na(comparison), "single_comparison", comparison)

      df
    }

    # ---- Extract Overall ----
    overall_df <- NULL
    if (!is.null(res_obj$Overall)) {

      if (inherits(res_obj$Overall, "cea_base")) {
        overall_df <- extract_one(res_obj$Overall)
      }

      if (inherits(res_obj$Overall, "cea_multicomparison")) {
        overall_df <- dplyr::bind_rows(lapply(names(res_obj$Overall), function(cmp) {
          obj <- res_obj$Overall[[cmp]]
          extract_one(obj, comparison = obj$comparison)
        }))
      }
    }

    # ---- Extract Subgroups ----
    sub_df <- NULL
    if (!is.null(res_obj$Subgroups) && length(res_obj$Subgroups) > 0) {

      sub_df <- dplyr::bind_rows(lapply(names(res_obj$Subgroups), function(var) {

        dplyr::bind_rows(lapply(names(res_obj$Subgroups[[var]]), function(lvl) {

          obj <- res_obj$Subgroups[[var]][[lvl]]

          if (inherits(obj, "cea_base")) {
            extract_one(obj,
                        subgroup_var   = var,
                        subgroup_level = lvl)
          }

          if (inherits(obj, "cea_multicomparison")) {
            dplyr::bind_rows(lapply(names(obj), function(cmp) {
              x <- obj[[cmp]]
              extract_one(x,
                          subgroup_var   = var,
                          subgroup_level = lvl,
                          comparison     = x$comparison)
            }))
          }
        }))
      }))
    }

    dplyr::bind_rows(overall_df, sub_df)
  }

  # ---------------------------------------------------------------
  # 2. Extract all bootstrap replicates from all datasets
  # ---------------------------------------------------------------
  combined_list <- lapply(names(objects), function(nm) {
    extract_bootstrap(objects[[nm]], dataset_name = nm)
  })

  combined_df <- dplyr::bind_rows(combined_list)

  if (nrow(combined_df) == 0)
    stop("No bootstrap samples found in the supplied objects.")

  # ---------------------------------------------------------------
  # 3. Identify method and build hierarchical unique identifier
  # ---------------------------------------------------------------

  combined_df$computed_by <- "bootstrap"

  combined_df$group_uid <- with(
    combined_df,
    paste(
      dataset,
      subgroup_var,
      subgroup_level,
      comparison,
      computed_by,
      sep = "_"
    )
  )

  # ---------------------------------------------------------------
  # 4. Ensure subgroup_var and subgroup_level are factors
  # ---------------------------------------------------------------
  combined_df$subgroup_var <- factor(
    combined_df$subgroup_var,
    levels = unique(combined_df$subgroup_var)
  )

  if (!is.factor(combined_df$subgroup_level)) {
    combined_df$subgroup_level <- factor(
      as.character(combined_df$subgroup_level)
    )
  }

  # ---------------------------------------------------------------
  # 5. Order columns
  # ---------------------------------------------------------------
  stat_cols <- grep(
    "^(Delta|Mean|ICER|NMB_)",
    names(combined_df),
    value = TRUE
  )

  col_order <- c(
    "dataset", "subgroup_var", "subgroup_level", "comparison",
    "group_uid", "replicate",
    "computed_by",
    stat_cols
  )

  combined_df <- combined_df[, intersect(col_order, names(combined_df))]

  tibble::as_tibble(combined_df)
}
