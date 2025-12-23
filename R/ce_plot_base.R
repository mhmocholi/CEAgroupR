#' Internal Graphical Layout Engine for CEAgroupR Visualizations
#'
#' Internal utility function that constructs the base \code{ggplot2} object
#' and resolves common aesthetic mappings used across all graphical functions
#' in the CEAgroupR package. This function ensures consistent handling of
#' colour, shape and linetype mappings, palette resolution, faceting rules
#' and legend behaviour.
#'
#' This function is not intended to be called directly by users. It is used
#' internally by \code{plot.icers}, \code{plot.ceacs}, \code{plot.evpis} and
#' \code{plot.marginals} to provide a unified graphical backbone.
#'
#' @param data A tibble containing bootstrap replicates or derived quantities
#'   used for plotting.
#'
#' @param color_by Character string specifying the variable mapped to colour,
#'   or \code{"none"} to disable colour mapping.
#'
#' @param shape_by Character string specifying the variable mapped to point
#'   shape (and, where applicable, line type), or \code{"none"} to disable
#'   shape-based grouping.
#'
#' @param facet_by Character string specifying the faceting directive, or
#'   \code{"none"} to disable faceting.
#'
#' @param filter_expr Optional tidyverse-style filtering expression applied
#'   to the input data prior to plotting.
#'
#' @param facet_scales Character string specifying facet scaling behaviour
#'   (e.g. \code{"fixed"} or \code{"free"}), passed to \code{ggplot2}.
#'
#' @param palette Colour palette name or vector used for colour mapping.
#'
#' @param shapes_palette Optional object of class \code{"cea_shapes"} defining
#'   custom shape and linetype values.
#'
#' @param theme_base A \code{ggplot2} theme applied to the base plot.
#'
#' @param auto_layout Logical; retained for backward compatibility. Currently
#'   ignored.
#'
#' @return
#' A list containing the initialized \code{ggplot} object and resolved
#' aesthetic components required by downstream plotting functions.
#'
#' @noRd
ce_plot_base <- function(
    data,
    color_by        = NULL,
    shape_by        = NULL,
    facet_by        = NULL,
    filter_expr     = NULL,
    facet_scales    = "fixed",
    palette         = "Dark2",
    shapes_palette  = NULL,
    theme_base      = ggplot2::theme_bw(),
    auto_layout     = TRUE
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
  normalize <- function(x) {
    if (is.null(x) || identical(x, "none")) NULL else x
  }

  color_by <- normalize(color_by)
  shape_by <- normalize(shape_by)
  facet_by <- normalize(facet_by)

  exists_var <- function(v) !is.null(v) && v %in% names(data)

  if (!exists_var(color_by)) color_by <- NULL
  if (!exists_var(shape_by)) shape_by <- NULL

  # shape_by requires factor levels
  if (!is.null(shape_by)) {
    data[[shape_by]] <- as.factor(data[[shape_by]])
  }

  # ---------------------------------------------------------------------------
  # 3. Palette resolution
  # ---------------------------------------------------------------------------
  if (is.null(palette)) {

    palette_values <- grDevices::palette()

  } else if (
    length(palette) == 1 &&
    palette %in% rownames(RColorBrewer::brewer.pal.info)
  ) {

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

  if (!is.null(color_by)) {
    aes_map$colour <- rlang::sym(color_by)
  }

  # Shape and linetype mapping are added by calling functions only.

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

    } else if (facet_by %in% c(
      "dataset", "comparison",
      "subgroup_var", "subgroup_level"
    )) {

      p <- p +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_by)),
          scales = facet_scales
        )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Legend visibility (hide if inactive)
  # ---------------------------------------------------------------------------
  if (is.null(color_by)) {
    p <- p + ggplot2::guides(colour = "none")
  }

  if (is.null(shape_by)) {
    p <- p + ggplot2::guides(shape = "none", linetype = "none")
  }

  # ---------------------------------------------------------------------------
  # 8. Shape and linetype values
  # ---------------------------------------------------------------------------
  shape_values    <- NULL
  linetype_values <- NULL

  if (!is.null(shape_by)) {

    lvls <- sort(unique(data[[shape_by]]))

    # Default shapes and linetypes
    shape_values    <- generate_shapes(lvls)
    linetype_values <- generate_linetypes(lvls)

    # Override with custom shapes palette if provided
    if (!is.null(shapes_palette)) {

      if (!inherits(shapes_palette, "cea_shapes")) {
        stop("'shapes_palette' must be an object of class 'cea_shapes'.")
      }

      if (!is.null(shapes_palette$shapes)) {
        shape_values <- rep(
          shapes_palette$shapes,
          length.out = length(lvls)
        )
        names(shape_values) <- lvls
      }

      if (!is.null(shapes_palette$linetypes)) {
        linetype_values <- rep(
          shapes_palette$linetypes,
          length.out = length(lvls)
        )
        names(linetype_values) <- lvls
      }
    }
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
