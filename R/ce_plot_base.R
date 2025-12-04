#' Internal Layout Constructor for Cost-Effectiveness Visualizations
#'
#' Provides the unified aesthetic and layout engine used across all
#' CEAgroupR graphical functions. This internal utility ensures consistent
#' handling of colour mapping, optional shape/linetype mapping, faceting,
#' palette resolution, and legend behaviour. It returns an initialized
#' \code{ggplot2} object along with all internally resolved aesthetic
#' components needed by downstream plotting functions.
#'
#' The function does not apply defaults for \code{color_by},
#' \code{shape_by} or \code{facet_by}. Calling functions must supply these
#' explicitly to maintain transparency and reproducibility of aesthetic
#' mappings.
#'
#' @param data A tibble produced by \code{combine_icers_results}.
#' @param color_by Character string giving the variable mapped to colour,
#'   or \code{"none"} to disable colour mapping.
#' @param shape_by Character string giving the variable mapped to point
#'   shape and linetype, or \code{"none"} to disable shape/linetype mapping.
#' @param facet_by Character string defining the faceting directive
#'   (e.g., \code{"dataset"}, \code{"comparison"}, \code{"subgroup"},
#'   \code{"subgroup_var"}, \code{"subgroup_level"}). Use \code{"none"} to
#'   disable faceting.
#' @param filter_expr Optional character string containing a tidyverse-style
#'   filter expression applied to \code{data}.
#' @param facet_scales Value passed to \code{ggplot2} facet functions
#'   (default: \code{"fixed"}).
#' @param palette A named vector of colours or a palette name recognized by
#'   \code{RColorBrewer}. Defaults to \code{"Dark2"}.
#' @param theme_base A \code{ggplot2} theme applied to the base plot.
#'   Defaults to \code{ggplot2::theme_bw()}.
#' @param auto_layout Ignored. Retained for backward compatibility.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{plot}: Initialized \code{ggplot} object.
#'   \item \code{data}: Filtered input data.
#'   \item \code{color_var}: Resolved colour aesthetic variable.
#'   \item \code{shape_var}: Resolved shape/linetype variable.
#'   \item \code{linetype_values}: Named vector of linetype patterns.
#'   \item \code{shape_values}: Named vector of point shapes.
#'   \item \code{palette_values}: Resolved colour palette.
#'   \item \code{group_var}: Internal grouping variable used for layers.
#' }
#'
#' @noRd
ce_plot_base <- function(
    data,
    color_by      = NULL,
    shape_by      = NULL,
    facet_by      = NULL,
    filter_expr   = NULL,
    facet_scales  = "fixed",
    palette       = "Dark2",
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
    shape_values    <- generate_shapes(lvls)
    linetype_values <- generate_linetypes(lvls)
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
