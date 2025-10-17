#' Plot incremental cost-effectiveness results (ICER plane)
#'
#' Produces a cost-effectiveness plane using bootstrap replicates from one or
#' several analyses. Supports flexible visualization modes for datasets and subgroups.
#'
#' @param data A `cea_results`, `cea_results_list`, or tibble from
#'   \code{\link{combine_icers_results}}.
#' @param mode Character. Visualization mode: one of
#'   `"overall"`, `"datasets"`, `"subgroups"`, or `"datasets_subgroups"`.
#' @param subgroups Optional character vector specifying which subgroup
#'   variables to include (for modes involving subgroups).
#' @param lambda Numeric. Willingness-to-pay threshold (optional).
#' @param facet Logical. If TRUE, facet the plot appropriately for the mode.
#' @param facet_by Character. For subgroup modes, determines the faceting scheme:
#'   `"vars"` (facet by subgroup variable) or `"levels"` (facet by subgroup level).
#' @param alpha Transparency for points (default = 0.5).
#' @param size Point size (default = 1).
#' @param palette Color palette from RColorBrewer (default = "Set2").
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_vline geom_abline
#'   facet_wrap facet_grid labs theme_bw scale_color_brewer
#' @importFrom dplyr filter
#' @export
plot.icers <- function(data,
                       mode = "overall",
                       subgroups = NULL,
                       lambda = NULL,
                       facet = FALSE,
                       facet_by = c("vars", "levels"),
                       alpha = 0.5,
                       size = 1,
                       palette = "Set2") {

  facet_by <- match.arg(facet_by)

  # ---- Handle input type ----
  if (inherits(data, "cea_results_list") || inherits(data, "cea_results")) {
    if (is.null(data$combined_replicates)) {
      stop("The object has no 'combined_replicates' element. Run icers_base() first.")
    }
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a cea_results, cea_results_list, or combined tibble.")
  }

  required_cols <- c("Delta_Cost", "Delta_Effect", "dataset", "level", "subgroup")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  valid_modes <- c("overall", "datasets", "subgroups", "datasets_subgroups")
  if (!mode %in% valid_modes) {
    stop("Invalid mode. Choose one of: ", paste(valid_modes, collapse = ", "))
  }

  # ---- Filter data ----
  if (mode %in% c("subgroups", "datasets_subgroups")) {
    if (is.null(subgroups)) {
      stop("Argument 'subgroups' must be provided for this mode.")
    }
    df <- dplyr::filter(df, level %in% subgroups)
  } else {
    df <- dplyr::filter(df, level == "Overall")
  }

  # ---- Base ggplot ----
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Delta_Effect, y = Delta_Cost)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = expression(Delta * " Effect"),
      y = expression(Delta * " Cost"),
      title = "Cost-effectiveness plane"
    )

  if (!is.null(lambda)) {
    p <- p + ggplot2::geom_abline(slope = lambda, intercept = 0, color = "red", size = 0.6)
  }

  # ---- Visualization logic by mode ----
  if (mode == "overall") {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = dataset), alpha = alpha, size = size) +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::labs(color = "Dataset")
    if (facet) p <- p + ggplot2::facet_wrap(~ dataset)
  }

  if (mode == "datasets") {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = dataset), alpha = alpha, size = size) +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::labs(color = "Dataset")
    if (facet) p <- p + ggplot2::facet_wrap(~ dataset)
  }

  if (mode == "subgroups") {
    if (facet_by == "vars") {
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = Delta_Effect, y = Delta_Cost, color = subgroup
      )) +
        ggplot2::geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_point(alpha = alpha, size = size) +
        ggplot2::facet_wrap(~ level) +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          color = "Subgroup level",
          title = "Cost-effectiveness by subgroup variable"
        )

    } else if (facet_by == "levels") {
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = Delta_Effect, y = Delta_Cost, color = level
      )) +
        ggplot2::geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_point(alpha = alpha, size = size) +
        ggplot2::facet_wrap(~ subgroup) +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          color = "Subgroup variable",
          title = "Cost-effectiveness by subgroup level"
        )
    }
  }

  if (mode == "datasets_subgroups") {

    if (facet_by == "vars") {
      # facet_grid by dataset (rows) × variable (cols), color by subgroup level
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = Delta_Effect, y = Delta_Cost, color = subgroup
      )) +
        ggplot2::geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_point(alpha = alpha, size = size) +
        ggplot2::facet_grid(dataset ~ level) +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          color = "Subgroup level",
          title = "Datasets × subgroup variables"
        )

    } else if (facet_by == "levels") {
      # facet_grid by dataset (rows) × subgroup level (cols), color by variable
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = Delta_Effect, y = Delta_Cost, color = level
      )) +
        ggplot2::geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
        ggplot2::geom_point(alpha = alpha, size = size) +
        ggplot2::facet_grid(dataset ~ subgroup) +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          color = "Subgroup variable",
          title = "Datasets × subgroup levels"
        )
    }
  }

  return(p)
}
