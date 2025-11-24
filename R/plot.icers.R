#' Plot Incremental Cost-Effectiveness Plane (ICER)
#'
#' Generates an incremental cost-effectiveness (CE) plane using the bootstrap
#' replicates produced by compute_icers(). Supports multiple datasets,
#' strategies, and subgroup structures. All visual components (bootstrap clouds,
#' mean points, contour densities, and reference WTP lines) are built using the
#' unified layout engine ce_plot_base().
#'
#' Principles:
#'   1) User-specified aesthetics override defaults.
#'   2) Setting an aesthetic to "none" disables it completely.
#'
#' Default behaviour:
#'   • mode = "Overall":
#'       colour = "comparison"
#'       shape  = "dataset"
#'       facet  = "none"
#'
#'   • mode = "Subgroups":
#'       colour = "comparison"
#'       shape  = "dataset"
#'       facet  = "subgroup" (grid)
#'
#' @param data A cea_results_list object or a tibble returned by
#'   combine_icers_results().
#' @param color_by Character variable mapped to colour, or "none".
#' @param shape_by Character variable mapped to shape, or "none".
#' @param facet_by Faceting directive ("none", "dataset", "comparison",
#'   "subgroup_var", "subgroup_level", "subgroup").
#' @param mode Either "Overall" or "Subgroups".
#' @param filter_expr Optional tidyverse-style logical expression as character.
#' @param show_points Logical; draw bootstrap replicates.
#' @param show_means Logical; draw mean incremental points.
#' @param show_contours Logical; draw contour layers.
#' @param show_lambdas Logical; draw WTP threshold lines.
#' @param label_lambdas Logical; write labels for WTP lines.
#' @param contour_type "non_parametric" (KDE) or "ellipse".
#' @param contour_level Probability level(s) for contour boundaries.
#' @param facet_scales Faceting scale behaviour.
#' @param palette Manual palette vector or Brewer palette name.
#' @param ... Additional parameters forwarded to geoms.
#'
#' @return A ggplot object.
#' @export
plot.icers <- function(data,
                       color_by     = NULL,
                       shape_by     = NULL,
                       facet_by     = NULL,
                       mode         = "Overall",
                       filter_expr  = NULL,
                       show_points  = TRUE,
                       show_means   = TRUE,
                       show_contours = FALSE,
                       show_lambdas  = TRUE,
                       label_lambdas = FALSE,
                       contour_type  = "non_parametric",
                       contour_level = 0.95,
                       facet_scales  = "fixed",
                       palette       = NULL,
                       ...) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ===========================================================================
  # 1. Extract data and settings
  # ===========================================================================
  if (inherits(data, "cea_results_list")) {
    df <- data$combined_replicates
    lambda_vals <- data$settings$lambda
  } else {
    df <- data
    lambda_vals <- NULL
  }

  if (!"group_uid" %in% names(df)) {
    df$group_uid <- with(
      df, paste(dataset, subgroup_var, subgroup_level, comparison, sep = "_")
    )
  }

  # ===========================================================================
  # 2. Optional filtering
  # ===========================================================================
  if (!is.null(filter_expr)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(filter_expr))
  }

  # ===========================================================================
  # 3. Mode filtering
  # ===========================================================================
  mode <- match.arg(mode, c("Overall", "Subgroups"))

  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), ]
  } else {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), ]
  }

  # ===========================================================================
  # 4. Resolve defaults for this mode
  # ===========================================================================
  default_color  <- "comparison"
  default_shape  <- "dataset"
  default_facet  <- if (mode == "Subgroups") "subgroup" else "none"

  resolved_color <- color_by %||% default_color
  resolved_shape <- shape_by %||% default_shape
  resolved_facet <- facet_by %||% default_facet

  if (identical(color_by, "none")) resolved_color <- NULL
  if (identical(shape_by, "none")) resolved_shape <- NULL
  if (identical(facet_by, "none")) resolved_facet <- NULL

  # ===========================================================================
  # 5. Construct base layout
  # ===========================================================================
  base <- ce_plot_base(
    data         = df,
    color_by     = resolved_color,
    shape_by     = resolved_shape,
    facet_by     = resolved_facet,
    filter_expr  = NULL,
    facet_scales = facet_scales,
    palette      = palette,
    auto_layout  = TRUE
  )

  p_base    <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  valid_color <- !is.null(color_var)
  valid_shape <- !is.null(shape_var)

  # ===========================================================================
  # 6. Build cloud aesthetics
  # ===========================================================================
  aes_cloud <- ggplot2::aes(
    x = Delta_Effect,
    y = Delta_Cost
  )
  if (valid_color) aes_cloud$colour <- rlang::sym(color_var)
  if (valid_shape) aes_cloud$shape  <- rlang::sym(shape_var)

  # ===========================================================================
  # 7. Extract effective colour map from invisible layer
  # ===========================================================================
  p_pre <- p_base +
    ggplot2::geom_point(
      data = df,
      mapping = aes_cloud,
      alpha = 0,
      show.legend = FALSE
    )

  gb  <- ggplot2::ggplot_build(p_pre)
  lyr <- gb$data[[length(gb$data)]]

  if (valid_color) {
    color_map <- setNames(lyr$colour, df[[color_var]])
  } else {
    color_map <- setNames(lyr$colour[1], "fixed")
  }

  # ===========================================================================
  # 8. Initialize CE plane with reference axes
  # ===========================================================================
  p <- p_base +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70")

  # ===========================================================================
  # 9. Auto-hide legends when only one level exists
  # ===========================================================================
  if (valid_color && dplyr::n_distinct(df[[color_var]]) == 1) {
    p <- p + ggplot2::guides(colour = "none")
  }
  if (valid_shape && dplyr::n_distinct(df[[shape_var]]) == 1) {
    p <- p + ggplot2::guides(shape = "none")
  }

  # ===========================================================================
  # 10. Bootstrap clouds
  # ===========================================================================
  if (show_points) {
    usr   <- list(...)
    n_rep <- dplyr::n_distinct(df$replicate)
    alpha_auto <- min(0.9, max(0.3, 0.9 - 0.15 * log10(n_rep / 100)))
    alpha_val  <- usr$alpha %||% alpha_auto
    size_val   <- usr$size  %||% 1.2

    p <- p +
      ggplot2::geom_point(
        data = df,
        mapping = aes_cloud,
        alpha  = alpha_val,
        size   = size_val,
        show.legend = TRUE
      )
  }

  # ===========================================================================
  # 11. Mean points (panel-aware)
  # ===========================================================================
  means <- df %>%
    dplyr::group_by(dataset, subgroup_var, subgroup_level, comparison, group_uid) %>%
    dplyr::summarise(
      Delta_Effect = mean(Delta_Effect, na.rm = TRUE),
      Delta_Cost   = mean(Delta_Cost,   na.rm = TRUE),
      colour_key   = if (valid_color) dplyr::first(.data[[color_var]]) else "fixed",
      .groups = "drop"
    )

  means$border_col <- colorspace::darken(
    color_map[as.character(means$colour_key)], 0.35
  )

  if (show_means) {
    p <- p +
      ggplot2::geom_point(
        data = means,
        mapping = ggplot2::aes(
          x = Delta_Effect,
          y = Delta_Cost
        ),
        size        = 3.0,
        stroke      = 1.1,
        shape       = 21,
        fill        = NA,                     # you prefer means without fill
        colour      = means$border_col,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  # ===========================================================================
  # 12. Panel-aware contours
  # ===========================================================================
  if (show_contours) {

    contour_type  <- match.arg(contour_type, c("non_parametric", "ellipse"))
    contour_level <- pmin(pmax(contour_level, 0.001), 0.999)

    group_ids <- unique(df$group_uid)
    contour_layers <- list()

    for (gid in group_ids) {

      sub <- df[df$group_uid == gid, ]
      if (nrow(sub) < 30) next

      dataset_val       <- unique(sub$dataset)
      subgroup_var_val  <- unique(sub$subgroup_var)
      subgroup_lvl_val  <- unique(sub$subgroup_level)
      comparison_val    <- unique(sub$comparison)

      key <- if (valid_color) {
        as.character(unique(sub[[color_var]])[1])
      } else {
        "fixed"
      }

      col <- colorspace::darken(color_map[[key]], 0.25)

      if (contour_type == "non_parametric") {

        dens <- tryCatch(
          MASS::kde2d(sub$Delta_Effect, sub$Delta_Cost, n = 200),
          error = function(e) NULL
        )
        if (is.null(dens)) next

        grid <- expand.grid(
          Delta_Effect = dens$x,
          Delta_Cost   = dens$y
        )
        grid$z    <- as.vector(dens$z)
        grid$prob <- grid$z / sum(grid$z)
        grid      <- grid[order(grid$prob, decreasing = TRUE), ]
        grid$cum  <- cumsum(grid$prob)

        breaks <- sapply(contour_level, function(l)
          grid$z[which.min(abs(grid$cum - l))]
        )

        grid$dataset        <- dataset_val
        grid$subgroup_var   <- subgroup_var_val
        grid$subgroup_level <- subgroup_lvl_val
        grid$comparison     <- comparison_val

        contour_layers[[length(contour_layers) + 1]] <- list(
          type   = "kde",
          data   = grid,
          breaks = breaks,
          colour = col
        )

      } else {

        mu <- colMeans(sub[, c("Delta_Effect", "Delta_Cost")], na.rm = TRUE)
        covmat <- stats::cov(
          sub[, c("Delta_Effect", "Delta_Cost")],
          use = "pairwise.complete.obs"
        )
        eg <- eigen(covmat)

        for (lvl in contour_level) {

          chi   <- sqrt(stats::qchisq(lvl, df = 2))
          theta <- seq(0, 2 * pi, length.out = 200)

          coords <- t(mu +
                        eg$vectors %*%
                        (sqrt(eg$values) * chi *
                           rbind(cos(theta), sin(theta))))

          df_el <- data.frame(
            Delta_Effect = coords[, 1],
            Delta_Cost   = coords[, 2],
            dataset        = dataset_val,
            subgroup_var   = subgroup_var_val,
            subgroup_level = subgroup_lvl_val,
            comparison     = comparison_val
          )

          contour_layers[[length(contour_layers) + 1]] <- list(
            type   = "ellipse",
            data   = df_el,
            breaks = NULL,
            colour = col
          )
        }
      }
    }

    # Add contours to plot
    for (layer in contour_layers) {

      if (layer$type == "kde") {

        p <- p +
          ggplot2::geom_contour(
            data = layer$data,
            mapping = ggplot2::aes(
              x = Delta_Effect,
              y = Delta_Cost,
              z = z
            ),
            breaks      = layer$breaks,
            linewidth   = 0.6,
            colour      = layer$colour,
            inherit.aes = FALSE,
            show.legend = FALSE
          )

      } else {

        p <- p +
          ggplot2::geom_path(
            data = layer$data,
            mapping = ggplot2::aes(
              x = Delta_Effect,
              y = Delta_Cost
            ),
            linewidth   = 0.6,
            colour      = layer$colour,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }
    }
  }

  # ===========================================================================
  # 13. Lambda reference lines
  # ===========================================================================
  if (show_lambdas && !is.null(lambda_vals)) {

    for (l in lambda_vals) {
      p <- p +
        ggplot2::geom_abline(
          slope     = l,
          intercept = 0,
          colour    = "grey45",
          linetype  = "dashed",
          linewidth = 0.6
        )
    }

    if (label_lambdas) {

      max_x <- max(df$Delta_Effect, na.rm = TRUE)

      df_lab <- data.frame(
        x     = max_x * 0.85,
        y     = lambda_vals * max_x,
        label = paste0("lambda = ", lambda_vals)
      )

      p <- p +
        ggplot2::geom_text(
          data = df_lab,
          mapping = ggplot2::aes(x = x, y = y, label = label),
          colour  = "grey30",
          size    = 3,
          hjust   = 0,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  # ===========================================================================
  # 14. Final labels
  # ===========================================================================
  p <- p +
    ggplot2::labs(
      title = "Incremental Cost-Effectiveness Plane",
      x     = expression(Delta * " Effect"),
      y     = expression(Delta * " Cost")
    )

  return(p)
}
