#' Internal layout manager for cost-effectiveness visualizations
#'
#' Generates a flexible ggplot layout structure used by all visualization
#' functions in the \pkg{CEAgroupR} package. It organises colour, shape,
#' and facet mappings for cost-effectiveness outputs derived from
#' \code{\link{compute_icers}}.
#'
#' This internal function is not exported and is used by specific plotting
#' functions such as \code{plot.icers()}, \code{plot.ceac()}, or
#' \code{plot.evpi()} to ensure consistency in layout and aesthetics.
#'
#' @noRd
ce_plot_base <- function(data,
                         color_by = "dataset",
                         shape_by = "none",
                         facet_by = "none",
                         filter_expr = NULL,
                         facet_scales = "fixed",
                         palette = "Set2",
                         theme_base = ggplot2::theme_bw()) {

  # ---- 1. Optional filtering ----
  if (!is.null(filter_expr)) {
    data <- dplyr::filter(data, !!rlang::parse_expr(filter_expr))
  }

  # ---- 2. Optionally handle 'Overall' subgroup levels ----
  # (Retained for flexibility; filtering to be handled in upper-level plots)
  if ("subgroup_level" %in% names(data)) {
    data$subgroup_level <- as.character(data$subgroup_level)
  }

  # ---- 3. Define color variable ----
  if (color_by == "both") {
    data$color_combined <- interaction(data$dataset, data$subgroup_level, drop = TRUE)
    color_var <- "color_combined"
  } else if (color_by %in% c("dataset", "subgroup_var", "subgroup_level")) {
    color_var <- color_by
  } else {
    color_var <- NULL
  }

  # ---- 4. Define shape variable ----
  if (shape_by == "both") {
    data$shape_combined <- interaction(data$dataset, data$subgroup_level, drop = TRUE)
    shape_var <- "shape_combined"
  } else if (shape_by %in% c("dataset", "subgroup_var", "subgroup_level")) {
    shape_var <- shape_by
  } else {
    shape_var <- NULL
  }

  # ---- 5. Define facet formula ----
  if (facet_by == "both") {
    facet_formula <- as.formula("dataset ~ subgroup_var")
  } else if (facet_by %in% c("dataset", "subgroup_var", "subgroup_level")) {
    facet_formula <- as.formula(paste("~", facet_by))
  } else {
    facet_formula <- NULL
  }

  # ---- 6. Build ggplot base ----
  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data$Delta_Effect,
      y = .data$Delta_Cost,
      colour = if (!is.null(color_var)) .data[[color_var]] else NULL,
      shape  = if (!is.null(shape_var)) .data[[shape_var]] else NULL
    )
  ) +
    ggplot2::scale_color_brewer(palette = palette) +
    theme_base +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  # ---- 7. Add facet structure ----
  if (!is.null(facet_formula)) {
    if (facet_by == "both") {
      p <- p + ggplot2::facet_grid(facet_formula, scales = facet_scales)
    } else {
      p <- p + ggplot2::facet_wrap(facet_formula, scales = facet_scales)
    }
  }

  return(list(
    plot = p,
    data = data,
    color_var = color_var,
    shape_var = shape_var,
    facet_formula = facet_formula
  ))
}
