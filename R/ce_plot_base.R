#' Internal layout constructor for all cost-effectiveness visualizations
#'
#' Provides the unified aesthetic and layout engine used across all CEAgroupR
#' graphical functions. It standardizes colour mapping, optional shape mapping,
#' faceting, palette resolution, legend visibility, and returns unified sets of
#' shapes/linetypes for use by downstream plotting functions.
#'
#' This function does *not* apply any automatic defaults for `color_by`,
#' `shape_by`, or `facet_by`. Calling functions must supply these explicitly.
#'
#' @param data A tibble produced by `combine_icers_results()`.
#' @param color_by Character column mapped to colour, or `"none"`.
#' @param shape_by Character column mapped to shape/linetype, or `"none"`.
#' @param facet_by Faceting directive or `"none"`.
#' @param filter_expr Optional tidyverse-style filter expression.
#' @param facet_scales Faceting scale behaviour.
#' @param palette Named vector or Brewer palette name.
#' @param theme_base ggplot2 theme applied to the base plot.
#' @param auto_layout Ignored; retained for backward compatibility.
#'
#' @return A list with components:
#'   * `plot` – initialized ggplot object
#'   * `data` – filtered data
#'   * `color_var` – resolved colour aesthetic
#'   * `shape_var` – resolved variable for shape/linetype
#'   * `linetype_values` – named vector of valid linetype patterns (if applicable)
#'   * `shape_values` – named vector of point shapes (if applicable)
#'   * `palette_values` – colour palette values
#'   * `group_var` – internal grouping variable
#'
#' @noRd
ce_plot_base <- function(
    data,
    color_by      = NULL,
    shape_by      = NULL,
    facet_by      = NULL,
    filter_expr   = NULL,
    facet_scales  = "fixed",
    palette       = "Set2",
    theme_base    = ggplot2::theme_bw(),
    auto_layout   = TRUE
) {

  # ---------------------------------------------------------------------------
  # Internal utilities
  # ---------------------------------------------------------------------------
  generate_linetypes <- function(levels) {
    base <- c(
      "solid", "dashed", "dotdash", "dotted",
      "11", "44", "13", "3311", "F2", "1248", "8899",
      "22FF", "4488", "F0F0", "1188", "3344", "77", "FF"
    )
    out <- rep(base, length.out = length(levels))
    names(out) <- levels
    out
  }

  generate_shapes <- function(levels) {
    base <- c(16, 17, 15, 3, 7, 8, 4, 1)
    out <- rep(base, length.out = length(levels))
    names(out) <- levels
    out
  }

  # ---------------------------------------------------------------------------
  # 1. Filtering
  # ---------------------------------------------------------------------------
  if (!is.null(filter_expr)) {
    data <- dplyr::filter(data, !!rlang::parse_expr(filter_expr))
  }

  # ---------------------------------------------------------------------------
  # 2. Normalize directives
  # ---------------------------------------------------------------------------
  normalize <- function(x) if (is.null(x) || identical(x, "none")) NULL else x

  color_by <- normalize(color_by)
  shape_by <- normalize(shape_by)
  facet_by <- normalize(facet_by)

  exists_var <- function(v) !is.null(v) && v %in% names(data)

  if (!exists_var(color_by)) color_by <- NULL
  if (!exists_var(shape_by)) shape_by <- NULL

  # shape_by requires factor levels
  if (!is.null(shape_by)) data[[shape_by]] <- as.factor(data[[shape_by]])

  # ---------------------------------------------------------------------------
  # 3. Palette resolution
  # ---------------------------------------------------------------------------
  if (is.null(palette)) {
    palette_values <- grDevices::palette()
  } else if (length(palette) == 1 &&
             palette %in% rownames(RColorBrewer::brewer.pal.info)) {

    palette_values <- RColorBrewer::brewer.pal(
      RColorBrewer::brewer.pal.info[palette, "maxcolors"],
      palette
    )

  } else {
    palette_values <- palette
  }

  # ---------------------------------------------------------------------------
  # 4. Base aesthetics
  # ---------------------------------------------------------------------------
  aes_map <- list(
    x     = quote(Delta_Effect),
    y     = quote(Delta_Cost),
    group = quote(group_uid)
  )

  if (!is.null(color_by))
    aes_map$colour <- rlang::sym(color_by)

  # IMPORTANT: do NOT attach shape or linetype here
  # Those are added by the calling plotting functions.

  # ---------------------------------------------------------------------------
  # 5. Base ggplot
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_map)) +
    theme_base +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90", colour = NA)
    )

  # ---------------------------------------------------------------------------
  # 6. Facets
  # ---------------------------------------------------------------------------
  if (!is.null(facet_by)) {

    if (facet_by == "subgroup") {

      p <- p +
        ggplot2::facet_grid(
          rows   = ggplot2::vars(subgroup_level),
          cols   = ggplot2::vars(subgroup_var),
          scales = facet_scales
        )

    } else if (facet_by %in% c("dataset","comparison",
                               "subgroup_var","subgroup_level")) {

      p <- p +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_by)),
          scales = facet_scales
        )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Legend visibility (default: hide if inactive)
  # ---------------------------------------------------------------------------
  if (is.null(color_by))
    p <- p + ggplot2::guides(colour = "none")

  if (is.null(shape_by))
    p <- p + ggplot2::guides(shape  = "none", linetype = "none")

  # ---------------------------------------------------------------------------
  # 8. Shape and linetype values
  # ---------------------------------------------------------------------------
  shape_values    <- NULL
  linetype_values <- NULL

  if (!is.null(shape_by)) {
    levels_shape     <- sort(unique(data[[shape_by]]))
    shape_values     <- generate_shapes(levels_shape)
    linetype_values  <- generate_linetypes(levels_shape)
  }

  # ---------------------------------------------------------------------------
  # 9. Output bundle
  # ---------------------------------------------------------------------------
  list(
    plot            = p,
    data            = data,
    color_var       = color_by,
    shape_var       = shape_by,
    linetype_values = linetype_values,
    shape_values    = shape_values,
    palette_values  = palette_values,
    group_var       = "group_uid"
  )
}
