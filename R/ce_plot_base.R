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

  # ---- 2. Normalise 'none' and missing aesthetics ----
  if (color_by %in% c("none", "", NA)) color_by <- NULL
  if (shape_by %in% c("none", "", NA)) shape_by <- NULL
  if (facet_by %in% c("none", "", NA)) facet_by <- NULL

  # ---- 3. Handle conflict 'both' + 'both' and legend preparation ----
  if (identical(color_by, "both") && identical(facet_by, "both")) {
    message("⚠️ Simplifying mapping: when both color_by and facet_by = 'both', ",
            "color_by is internally set to 'subgroup_level' for clarity.")
    color_by <- "subgroup_level"
  }

  legend_shape_title <- NULL
  if (!is.null(shape_by) && shape_by %in% names(data)) {
    legend_shape_title <- shape_by
  }

  # ---- 4. Ensure 'group_uid' exists ----
  if (!"group_uid" %in% names(data)) {
    data$group_uid <- with(data, paste0(dataset, "_", subgroup_var, "_", subgroup_level))
  }

  # ---- 5. Define aesthetic variables ----
  color_var <- if (!is.null(color_by) && color_by %in% names(data)) color_by else NULL
  shape_var <- if (!is.null(shape_by) && shape_by %in% names(data)) shape_by else NULL

  # ---- 6. Define facet formula ----
  facet_formula <- NULL
  if (!is.null(facet_by)) {
    if (facet_by == "both") {
      facet_formula <- as.formula("dataset ~ subgroup_var")
    } else {
      facet_formula <- as.formula(paste("~", facet_by))
    }
  }

  # ---- 7. Build safe aes mappings ----
  aes_mappings <- list(
    x = quote(Delta_Effect),
    y = quote(Delta_Cost),
    group = quote(group_uid)
  )
  if (!is.null(color_var)) aes_mappings$colour <- rlang::sym(color_var)
  if (!is.null(shape_var)) aes_mappings$shape  <- rlang::sym(shape_var)

  # ---- 8. Build ggplot base ----
  p <- ggplot2::ggplot(
    data = data,
    mapping = do.call(ggplot2::aes, aes_mappings)
  ) +
    ggplot2::scale_color_brewer(palette = palette) +
    theme_base +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  # ---- 9. Add facet structure ----
  if (!is.null(facet_formula)) {
    if (facet_by == "both") {
      p <- p + ggplot2::facet_grid(facet_formula, scales = facet_scales)
    } else {
      p <- p + ggplot2::facet_wrap(facet_formula, scales = facet_scales)
    }
  }

  # ---- 10. Update legend titles ----
  if (!is.null(color_by) && color_by %in% names(data)) {
    p <- p + ggplot2::labs(colour = color_by)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + ggplot2::labs(linetype = legend_shape_title, shape = legend_shape_title)
  }

  # ---- 11. Return ----
  return(list(
    plot = p,
    data = data,
    color_var = color_var,
    shape_var = shape_var,
    facet_formula = facet_formula,
    group_var = "group_uid"
  ))
}
