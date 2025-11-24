#' Internal layout constructor for cost-effectiveness visualizations
#'
#' This function provides the unified aesthetic and layout engine used by all
#' graphical functions in the CEAgroupR package. It standardizes the handling of
#' colour, shape, and facet mappings, the interpretation of user overrides, the
#' resolution of palettes, and the configuration of legends and facet behaviour.
#'
#' The design follows two core principles:
#'   1) User-specified aesthetics always override internally defined defaults.
#'   2) Setting an aesthetic to "none" disables it completely.
#'
#' Importantly, \code{ce_plot_base()} does not impose any mode-dependent
#' defaults. Instead, defaults for each visualization mode (e.g. Overall vs
#' Subgroups) are resolved in the calling function (e.g. \code{plot.icers}),
#' which sends explicit values for \code{color_by}, \code{shape_by}, and
#' \code{facet_by}. This separation of responsibilities ensures that the layout
#' engine remains stable and predictable.
#'
#' Supported faceting directives:
#'   "none"           – no faceting applied
#'   "dataset"        – facet_wrap(~ dataset)
#'   "comparison"     – facet_wrap(~ comparison)
#'   "subgroup_var"   – facet_wrap(~ subgroup_var)
#'   "subgroup_level" – facet_wrap(~ subgroup_level)
#'   "subgroup"       – facet_grid(subgroup_level ~ subgroup_var)
#'
#' @param data A tibble returned by \code{combine_icers_results()}.
#' @param color_by Character, name of the column mapped to colour. Use "none" to
#'   disable colour mapping. If \code{NULL}, no colour aesthetic is applied.
#' @param shape_by Character, name of the column mapped to shape. Use "none" to
#'   disable shape mapping. If \code{NULL}, no shape aesthetic is applied.
#' @param facet_by Character specifying the faceting directive, or "none" for
#'   no faceting.
#' @param filter_expr Optional tidyverse-style filtering expression (as a
#'   character string).
#' @param facet_scales Character, passed to the \code{scales} argument of
#'   \code{facet_wrap} or \code{facet_grid}.
#' @param palette Either a vector of colours or the name of a Brewer palette.
#' @param theme_base A ggplot2 theme object to be added to the plot.
#' @param auto_layout Logical; retained for backward compatibility but now
#'   disabled as a decision mechanism. All defaults must be explicitly supplied
#'   by the caller.
#'
#' @return A list containing:
#'   \item{plot}{A ggplot object containing the initialized layout}
#'   \item{data}{The (optionally filtered) input data}
#'   \item{color_var}{Character, resolved colour aesthetic variable}
#'   \item{shape_var}{Character, resolved shape aesthetic variable}
#'   \item{palette_values}{Resolved palette as a character vector}
#'   \item{group_var}{The grouping variable internally used ("group_uid")}
#'
#' @noRd
ce_plot_base <- function(data,
                         color_by      = NULL,
                         shape_by      = NULL,
                         facet_by      = NULL,
                         filter_expr   = NULL,
                         facet_scales  = "fixed",
                         palette       = "Set2",
                         theme_base    = ggplot2::theme_bw(),
                         auto_layout   = TRUE) {

  # ---------------------------------------------------------------------------
  # 1. Optional data filtering
  # ---------------------------------------------------------------------------
  if (!is.null(filter_expr)) {
    data <- dplyr::filter(data, !!rlang::parse_expr(filter_expr))
  }

  # ---------------------------------------------------------------------------
  # 2. Interpret user intent
  # ---------------------------------------------------------------------------
  disable_color <- identical(color_by, "none")
  disable_shape <- identical(shape_by, "none")
  disable_facet <- identical(facet_by, "none")

  normalize <- function(x) {
    if (is.null(x) || identical(x, "none")) return(NULL)
    x
  }

  color_by <- normalize(color_by)
  shape_by <- normalize(shape_by)
  facet_by <- normalize(facet_by)

  # ---------------------------------------------------------------------------
  # 3. Validate aesthetic variables
  # ---------------------------------------------------------------------------
  exists_var <- function(v) !is.null(v) && v %in% names(data)

  if (!exists_var(color_by)) color_by <- NULL
  if (!exists_var(shape_by)) shape_by <- NULL

  # shape aesthetics require factor levels
  if (!is.null(shape_by)) {
    data[[shape_by]] <- as.factor(data[[shape_by]])
  }

  # ---------------------------------------------------------------------------
  # 4. Resolve colour palette
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
  # 5. Core aesthetic mapping
  # ---------------------------------------------------------------------------
  aes_map <- list(
    x     = quote(Delta_Effect),
    y     = quote(Delta_Cost),
    group = quote(group_uid)
  )
  if (!is.null(color_by)) aes_map$colour <- rlang::sym(color_by)
  if (!is.null(shape_by)) aes_map$shape  <- rlang::sym(shape_by)

  # ---------------------------------------------------------------------------
  # 6. Initialize base plot
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_map)) +
    theme_base +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90", colour = NA)
    )

  # ---------------------------------------------------------------------------
  # 7. Faceting (explicit directive only)
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
  # 8. Configure legend visibility
  # ---------------------------------------------------------------------------
  if (is.null(color_by)) p <- p + ggplot2::guides(colour = "none")
  if (is.null(shape_by)) p <- p + ggplot2::guides(shape  = "none")

  # ---------------------------------------------------------------------------
  # 9. Output
  # ---------------------------------------------------------------------------
  list(
    plot          = p,
    data          = data,
    color_var     = color_by,
    shape_var     = shape_by,
    palette_values = palette_values,
    group_var     = "group_uid"
  )
}
