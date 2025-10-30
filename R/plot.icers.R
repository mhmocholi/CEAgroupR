#' Plot Incremental Cost-Effectiveness Plane (ICER)
#'
#' Generates a cost-effectiveness (CE) plane displaying incremental costs
#' and effects obtained from bootstrap replications. Supports flexible
#' organization by dataset, subgroup variable, and subgroup level.
#' Optional elements include mean points, willingness-to-pay (λ) reference
#' lines, and either non-parametric or elliptical contour overlays.
#'
#' The layout and argument structure are consistent across all visualization
#' functions in the package. Additional ggplot2 arguments can be passed through
#' the ellipsis (...) for fine control over graphical aesthetics.
#'
#' @param data A \code{cea_results_list} or tibble returned by
#'   \code{\link{combine_icers_results}}.
#' @param color_by,shape_by,facet_by Mapping options
#'   ("dataset", "subgroup_var", "subgroup_level", "both", or "none").
#' @param show_points Logical. If TRUE, display individual bootstrap replicates.
#' @param show_means Logical. If TRUE, display mean incremental values per group.
#' @param show_contours Logical. If TRUE, add distribution contours.
#' @param contour_type Character. Either "non_parametric" (default) or "ellipse".
#' @param contour_level Numeric vector. Probability levels for contour display
#'   (default = 0.95).
#' @param show_lambdas Logical. If TRUE, add λ reference lines.
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param palette Character. Color palette name for ggplot2 (default = "Set2").
#' @param ... Additional arguments passed to ggplot2 layers (e.g., size, alpha).
#'
#' @importFrom colorspace darken
#' @return A ggplot object representing the cost-effectiveness plane.
#' @export
plot.icers <- function(data,
                       color_by = "dataset",
                       shape_by = "none",
                       facet_by = "none",
                       show_points = TRUE,
                       show_means = TRUE,
                       show_contours = FALSE,
                       contour_type = "non_parametric",
                       contour_level = 0.95,
                       show_lambdas = TRUE,
                       facet_scales = "fixed",
                       palette = "Set2",
                       ...) {

  # ---- 1. Prepare input ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a cea_results_list or a combined tibble.")
  }

  # ---- 2. Base layout ----
  base <- ce_plot_base(df,
                       color_by = color_by,
                       shape_by = shape_by,
                       facet_by = facet_by,
                       facet_scales = facet_scales,
                       palette = palette)

  df_plot <- base$data
  p <- base$plot +
    ggplot2::labs(
      title = "Incremental Cost-Effectiveness Plane",
      x = expression(Delta * " Effect"),
      y = expression(Delta * " Cost")
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70")

  # ---- Define darkened color map for means and contours ----
  gg_pal <- suppressWarnings(
    ggplot2::scale_color_brewer(palette = palette)$palette(length(unique(df[[color_by]])))
  )
  names(gg_pal) <- sort(unique(df[[color_by]]))

  # Slightly darken colors from the base palette (HCL adjustment)
  dark_map <- sapply(gg_pal, function(col) colorspace::darken(col, amount = 0.15))

  # ---- 3. Add λ reference lines ----
  lambda <- if (!is.null(data$settings$lambda)) data$settings$lambda else NULL
  if (show_lambdas && !is.null(lambda)) {
    for (l in lambda) {
      p <- p + ggplot2::geom_abline(
        slope = l, intercept = 0,
        linetype = "dotted", colour = "grey60", linewidth = 0.5
      )
    }
  }

  # ---- 4. Bootstrap points ----
  if (show_points) {
    n_repl <- length(unique(df_plot$replicate))
    alpha_auto <- min(0.9, max(0.3, 0.9 - 0.15 * log10(n_repl / 100)))

    user_args <- list(...)
    alpha_val <- if (!"alpha" %in% names(user_args)) alpha_auto else user_args$alpha
    size_val  <- if (!"size" %in% names(user_args)) 1.2 else user_args$size

    aes_points <- list(
      x = quote(Delta_Effect),
      y = quote(Delta_Cost),
      colour = rlang::sym(base$color_var)
    )
    if (!is.null(base$shape_var) && base$shape_var != "none")
      aes_points$shape <- rlang::sym(base$shape_var)

    p <- p + ggplot2::geom_point(
      data = df_plot,
      mapping = do.call(ggplot2::aes, aes_points),
      alpha = alpha_val,
      size = size_val,
      show.legend = FALSE,
      ...
    )
  }

  # ---- 5. Mean points (mapped color with darkened overlay) ----
  if (show_means) {
    grouping_vars <- c(base$color_var, base$shape_var, all.vars(base$facet_formula))
    grouping_vars <- grouping_vars[!is.na(grouping_vars) & grouping_vars != "" & grouping_vars != "none"]

    means <- if (length(grouping_vars) > 0) {
      df_plot %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::summarise(
          Delta_Effect = mean(Delta_Effect, na.rm = TRUE),
          Delta_Cost = mean(Delta_Cost, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      data.frame(
        Delta_Effect = mean(df_plot$Delta_Effect, na.rm = TRUE),
        Delta_Cost = mean(df_plot$Delta_Cost, na.rm = TRUE)
      )
    }

    for (fv in all.vars(base$facet_formula)) {
      if (!fv %in% names(means)) means[[fv]] <- unique(df_plot[[fv]])[1]
    }

    # First layer: mapped color (appears in legend)
    p <- p + ggplot2::geom_point(
      data = means,
      ggplot2::aes(
        x = Delta_Effect,
        y = Delta_Cost,
        colour = .data[[base$color_var]],
        shape  = if (!is.null(base$shape_var) && base$shape_var != "none")
          .data[[base$shape_var]] else NULL
      ),
      size = 3.5,
      stroke = 1,
      alpha = 1,
      show.legend = TRUE
    )

    # Second layer: darkened tone overlay, no legend
    for (grp in unique(means[[base$color_var]])) {
      means_grp <- means[means[[base$color_var]] == grp, , drop = FALSE]
      p <- p + ggplot2::geom_point(
        data = means_grp,
        ggplot2::aes(x = Delta_Effect, y = Delta_Cost),
        colour = dark_map[[grp]],
        size = 3.5,
        show.legend = FALSE
      )
    }
  }

  # ---- 6. Contours ----
  if (show_contours) {
    if (missing(contour_type) || is.null(contour_type)) contour_type <- "ellipse"

    group_var <- base$color_var
    facet_vars <- all.vars(base$facet_formula)
    grouping_vars <- c(group_var, facet_vars)
    grouping_vars <- grouping_vars[grouping_vars %in% names(df_plot)]

    split_data <- if (length(grouping_vars) > 0) {
      split(df_plot, f = interaction(df_plot[grouping_vars], drop = TRUE))
    } else {
      list(all_data = df_plot)
    }

    contour_layers <- list()

    for (nm in names(split_data)) {
      sub <- split_data[[nm]]
      if (nrow(sub) < 30) next

      group_values <- lapply(grouping_vars, function(gv) unique(sub[[gv]])[1])
      names(group_values) <- grouping_vars

      # Non-parametric contours
      if (contour_type == "non_parametric") {
        dens <- tryCatch(MASS::kde2d(sub$Delta_Effect, sub$Delta_Cost, n = 200),
                         error = function(e) NULL)
        if (is.null(dens)) next
        df_dens <- expand.grid(Delta_Effect = dens$x, Delta_Cost = dens$y)
        df_dens$z <- as.vector(dens$z)
        df_dens$prob <- df_dens$z / sum(df_dens$z, na.rm = TRUE)
        df_sorted <- df_dens[order(df_dens$prob, decreasing = TRUE), ]
        df_sorted$cumprob <- cumsum(df_sorted$prob)
        levels_vec <- sort(unique(contour_level))
        levels_vec <- pmin(pmax(levels_vec, 0.001), 0.999)
        z_cuts <- vapply(levels_vec, function(lvl) {
          idx <- which.min(abs(df_sorted$cumprob - lvl))
          df_sorted$z[idx]
        }, numeric(1))
        df_dens <- cbind(df_dens, as.data.frame(group_values))
        contour_layers[[nm]] <- ggplot2::geom_contour(
          data = df_dens,
          ggplot2::aes(x = Delta_Effect, y = Delta_Cost, z = z),
          colour = dark_map[as.character(unique(sub[[group_var]]))],
          breaks = z_cuts,
          linewidth = 0.6,
          show.legend = FALSE
        )
      }

      # Elliptical contours
      if (contour_type == "ellipse") {
        mu <- c(mean(sub$Delta_Effect, na.rm = TRUE),
                mean(sub$Delta_Cost,   na.rm = TRUE))
        cov_mat <- stats::cov(cbind(sub$Delta_Effect, sub$Delta_Cost),
                              use = "pairwise.complete.obs")
        eig <- eigen(cov_mat)
        levels_vec <- sort(unique(contour_level))
        levels_vec <- pmin(pmax(levels_vec, 0.001), 0.999)
        for (lvl in levels_vec) {
          chi_val <- sqrt(stats::qchisq(lvl, df = 2))
          theta <- seq(0, 2 * pi, length.out = 200)
          coords <- t(mu + eig$vectors %*%
                        (sqrt(eig$values) * chi_val *
                           rbind(cos(theta), sin(theta))))
          df_ellipse <- data.frame(
            Delta_Effect = coords[, 1],
            Delta_Cost   = coords[, 2],
            as.data.frame(group_values)
          )
          contour_layers[[paste0(nm, "_", lvl)]] <- ggplot2::geom_path(
            data = df_ellipse,
            ggplot2::aes(x = Delta_Effect, y = Delta_Cost),
            colour = dark_map[as.character(unique(sub[[group_var]]))],
            linewidth = 0.6,
            show.legend = FALSE
          )
        }
      }
    }

    if (length(contour_layers) > 0) p <- p + contour_layers
  }

  return(p)
}
