#' Plot marginal distributions of incremental outcomes (ΔCost or ΔEffect)
#'
#' Visualizes marginal distributions of incremental costs (ΔCost) or incremental
#' effects (ΔEffect) using bootstrap replicates. Supports histogram, density, and
#' boxplot geometries. Fully aligned with the unified layout architecture of
#' \code{plot.icers()}, \code{plot.ceacs()}, and \code{plot.evpis()}:
#'
#'   • Option C behaviour for \code{color_by}, \code{shape_by}, \code{facet_by}.
#'   • Dynamic filtering with \code{filter_expr}.
#'   • Flexible palette specification (colour vectors, Brewer palettes, base R).
#'   • Consistent faceting, including the special \code{"subgroup"} mode.
#'   • \code{shape_by} → linetype for histogram/density/boxplot.
#'   • Legends unified to display colour and linetype cleanly and consistently.
#'
#' @param data A \code{cea_results_list} returned by \code{compute_icers()} or
#'   a tibble returned by \code{combine_icers_results()}.
#'
#' @param variable Character: \code{"cost"} (ΔCost) or \code{"effect"} (ΔEffect).
#'
#' @param geom_type Character: \code{"histogram"}, \code{"density"}, or
#'   \code{"boxplot"}.
#'
#' @param color_by,shape_by,facet_by Character variables defining aesthetics to
#'   map to colour, linetype, and facets.
#'
#' @param mode Character, \code{"Overall"} or \code{"Subgroups"}.
#'
#' @param filter_expr Optional tidy-style filtering expression.
#'
#' @param facet_scales Facet scaling behaviour.
#'
#' @param palette A colour palette (vector, Brewer name, or \code{NULL}).
#'
#' @param bins Integer. Number of bins for histogram.
#'
#' @param ... Additional arguments passed to geom layers.
#'
#' @return A \code{ggplot} object.
#' @export
plot.marginals <- function(data,
                           variable = "cost",
                           geom_type = "histogram",
                           color_by = NULL,
                           shape_by = NULL,
                           facet_by = NULL,
                           mode = "Overall",
                           filter_expr = NULL,
                           facet_scales = "fixed",
                           palette = NULL,
                           bins = 30,
                           ...) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # -------------------------------------------------------------
  # 1. Extract replicates
  # -------------------------------------------------------------
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("'combined_replicates' not found. Run compute_icers().")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else stop("Input must be a cea_results_list or data frame.")

  if (!"group_uid" %in% names(df)) {
    df$group_uid <- with(df,
                         paste(dataset, subgroup_var, subgroup_level, comparison, sep = "_"))
  }

  # -------------------------------------------------------------
  # 2. Filtering
  # -------------------------------------------------------------
  if (!is.null(filter_expr)) {
    expr <- rlang::parse_expr(filter_expr)
    df <- tryCatch(dplyr::filter(df, !!expr),
                   error = function(e) stop("Filtering failed: ", e$message))
  }

  # -------------------------------------------------------------
  # 3. Mode filtering
  # -------------------------------------------------------------
  mode <- match.arg(mode, c("Overall", "Subgroups"))
  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), ]
  } else {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), ]
  }

  # -------------------------------------------------------------
  # 4. Facet defaults (Option C logic)
  # -------------------------------------------------------------
  if (!is.null(facet_by) && !(facet_by %in% c("", NA, "none"))) {
    facet_input <- facet_by
  } else if (mode == "Overall") {
    facet_input <- NULL
  } else {
    facet_input <- "auto"   # ce_plot_base → faceting by subgroup grid
  }

  # -------------------------------------------------------------
  # 5. Base plot with unified layout manager
  # -------------------------------------------------------------
  base <- ce_plot_base(
    data         = df,
    color_by     = color_by,
    shape_by     = shape_by,
    facet_by     = facet_input,
    facet_scales = facet_scales,
    palette      = palette,
    auto_layout  = TRUE
  )

  p         <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  # -------------------------------------------------------------
  # 6. Δ variable selection
  # -------------------------------------------------------------
  var_col <- switch(
    variable,
    "cost"   = "Delta_Cost",
    "effect" = "Delta_Effect",
    stop("variable must be 'cost' or 'effect'.")
  )

  # -------------------------------------------------------------
  # 7. User args
  # -------------------------------------------------------------
  args      <- list(...)
  alpha_val <- args$alpha %||% 0.5
  lw        <- args$linewidth %||% 0.6

  # -------------------------------------------------------------
  # 8. Palette + darkened borders
  # -------------------------------------------------------------
  use_col <- base$palette_values
  names(use_col) <- sort(unique(as.character(df[[color_var]])))
  dark_cols <- colorspace::darken(use_col, amount = 0.30)
  names(dark_cols) <- names(use_col)

  # dynamic linetype
  if (!is.null(shape_var)) {
    linetype_vals <- setNames(
      c("solid","dashed","dotdash","dotted")[seq_along(sort(unique(df[[shape_var]])))],
      sort(unique(df[[shape_var]]))
    )
  }

  # -------------------------------------------------------------
  # 9. Geometries
  # -------------------------------------------------------------

  # ---- DENSITY ----
  if (geom_type == "density") {

    aes_density <- ggplot2::aes(
      x        = .data[[var_col]],
      fill     = if (!is.null(color_var)) .data[[color_var]] else NULL,
      colour   = if (!is.null(color_var)) .data[[color_var]] else NULL,
      linetype = if (!is.null(shape_var)) .data[[shape_var]] else NULL,
      group    = group_uid
    )

    p <- p +
      ggplot2::geom_density(
        data = df,
        mapping = aes_density,
        alpha = alpha_val,
        linewidth = lw,
        inherit.aes = FALSE
      )

    # ---- HISTOGRAM ----
  } else if (geom_type == "histogram") {

    aes_hist <- ggplot2::aes(
      x        = .data[[var_col]],
      fill     = if (!is.null(color_var)) .data[[color_var]] else NULL,
      colour   = if (!is.null(color_var)) .data[[color_var]] else NULL,
      linetype = if (!is.null(shape_var)) .data[[shape_var]] else NULL,
      group    = group_uid
    )

    p <- p +
      ggplot2::geom_histogram(
        data        = df,
        mapping     = aes_hist,
        bins        = bins,
        alpha       = alpha_val,
        position    = "identity",
        linewidth   = lw,
        inherit.aes = FALSE
      )

    # ---- BOXPLOT ----
  } else if (geom_type == "boxplot") {

    aes_box <- ggplot2::aes(
      x        = if (!is.null(shape_var)) .data[[shape_var]] else "All",
      y        = .data[[var_col]],
      fill     = if (!is.null(color_var)) .data[[color_var]] else NULL,
      colour   = if (!is.null(color_var)) .data[[color_var]] else NULL,
      linetype = if (!is.null(shape_var)) .data[[shape_var]] else NULL
    )

    p <- p +
      ggplot2::geom_boxplot(
        data        = df,
        mapping     = aes_box,
        alpha       = alpha_val,
        outlier.shape = 21,
        outlier.size  = 1.5,
        inherit.aes   = FALSE
      )

  } else stop("geom_type must be 'histogram', 'density', or 'boxplot'.")

  # -------------------------------------------------------------
  # 10. Unified scales + LEGEND FIX (colour + linetype)
  # -------------------------------------------------------------
  if (!is.null(color_var)) {

    p <- p +
      ggplot2::scale_fill_manual(values = use_col,  name = color_var) +
      ggplot2::scale_colour_manual(values = dark_cols, name = color_var)

    if (!is.null(shape_var)) {

      p <- p +
        ggplot2::scale_linetype_manual(values = linetype_vals, name = shape_var) +
        ggplot2::guides(
          fill     = ggplot2::guide_legend(title = color_var),
          colour   = ggplot2::guide_legend(title = color_var),
          linetype = ggplot2::guide_legend(
            title = shape_var,
            override.aes = list(
              fill     = rep("grey80", length(linetype_vals)),
              colour   = rep("black", length(linetype_vals)),
              linetype = linetype_vals,
              size     = 0.8
            )
          )
        )

    } else {
      p <- p +
        ggplot2::guides(
          fill   = ggplot2::guide_legend(title = color_var),
          colour = ggplot2::guide_legend(title = color_var)
        )
    }
  }

  # -------------------------------------------------------------
  # 11. Axis formatting
  # -------------------------------------------------------------
  if (geom_type == "boxplot") {

    p <- p +
      ggplot2::scale_x_discrete() +
      ggplot2::scale_y_continuous(labels = scales::label_number())

  } else {

    p <- p +
      ggplot2::scale_x_continuous(labels = scales::label_number()) +
      ggplot2::scale_y_continuous(labels = scales::label_number())
  }

  # -------------------------------------------------------------
  # 12. Labels
  # -------------------------------------------------------------
  label_y    <- if (geom_type == "boxplot") "Value" else "Density"
  label_main <- paste("Distribution of Δ", tools::toTitleCase(variable), sep="")

  p <- p +
    ggplot2::labs(
      title = label_main,
      x     = paste("Δ", tools::toTitleCase(variable)),
      y     = label_y
    )

  return(p)
}
