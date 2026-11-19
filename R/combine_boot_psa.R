
#' Combine bootstrap and PSA results
#'
#' Combines simulation replicates from compute_icers() and compute_psa()
#' while preserving the original results and settings of both methods.
#'
#' @param bootstrap A cea_results_list object from compute_icers().
#' @param psa A cea_results_list object from compute_psa().
#'
#' @return A cea_results_list object containing combined replicates,
#'   the original bootstrap-compatible structure, and method-specific
#'   results and settings.
#'
#' @export
combine_boot_psa <- function(bootstrap, psa) {

  # ---------------------------------------------------------------
  # 1. Validate input objects
  # ---------------------------------------------------------------

  validate_input <- function(x, method) {

    if (!inherits(x, "cea_results_list")) {
      stop(
        "'", method, "' must be a 'cea_results_list' object.",
        call. = FALSE
      )
    }

    if (!is.data.frame(x$combined_replicates) ||
        nrow(x$combined_replicates) == 0L) {
      stop(
        "'", method, "' must contain non-empty combined_replicates.",
        call. = FALSE
      )
    }

    required_cols <- c(
      "dataset", "subgroup_var", "subgroup_level",
      "comparison", "group_uid", "replicate",
      "computed_by", "Delta_Cost", "Delta_Effect"
    )

    missing_cols <- setdiff(
      required_cols,
      names(x$combined_replicates)
    )

    if (length(missing_cols) > 0L) {
      stop(
        "'", method, "' is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    if (anyNA(x$combined_replicates$computed_by) ||
        any(x$combined_replicates$computed_by != method)) {
      stop(
        "'", method,
        "' contains replicates with an unexpected computed_by value.",
        call. = FALSE
      )
    }

    if (anyNA(x$combined_replicates$group_uid)) {
      stop(
        "'", method, "' contains missing group_uid values.",
        call. = FALSE
      )
    }

    if (!is.list(x$settings)) {
      stop(
        "'", method, "' must contain a settings list.",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  validate_input(bootstrap, "bootstrap")
  validate_input(psa, "psa")

  # ---------------------------------------------------------------
  # 2. Check compatibility of replicate columns
  # ---------------------------------------------------------------

  boot_cols <- names(bootstrap$combined_replicates)
  psa_cols  <- names(psa$combined_replicates)

  if (!setequal(boot_cols, psa_cols)) {
    stop(
      "Bootstrap and PSA replicate columns are not identical.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------
  # 3. Check group identifiers
  # ---------------------------------------------------------------

  overlapping_uids <- intersect(
    unique(as.character(
      bootstrap$combined_replicates$group_uid
    )),
    unique(as.character(
      psa$combined_replicates$group_uid
    ))
  )

  if (length(overlapping_uids) > 0L) {
    stop(
      "Bootstrap and PSA contain overlapping group_uid values.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------
  # 4. Combine simulation replicates
  # ---------------------------------------------------------------

  combined_replicates <- dplyr::bind_rows(
    bootstrap$combined_replicates,
    psa$combined_replicates
  )

  # ---------------------------------------------------------------
  # 5. Preserve the existing bootstrap-compatible structure
  # ---------------------------------------------------------------

  result <- bootstrap

  result$combined_replicates <- combined_replicates

  # ---------------------------------------------------------------
  # 6. Preserve original method-specific information
  # ---------------------------------------------------------------

  result$settings_by_method <- list(
    bootstrap = bootstrap$settings,
    psa = psa$settings
  )

  result$results_by_method <- list(
    bootstrap = bootstrap,
    psa = psa
  )

  # ---------------------------------------------------------------
  # 7. Return the combined object
  # ---------------------------------------------------------------

  class(result) <- c("cea_results_list", "list")

  return(result)
}
