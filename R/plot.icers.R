#' Plot Incremental Cost-Effectiveness Plane (ICER)
#'
#' Generates a cost-effectiveness (CE) plane displaying incremental costs
#' and effects obtained from bootstrap replications. Fully compatible with
#' dataset, subgroup variable, and subgroup level mapping. Includes an explicit
#' visualization mode selector controlling whether to display overall, subgroup,
#' or full results, with automatic aesthetic defaults by mode.
#'
#' @param data A \code{cea_results_list} object returned by
#'   \code{\link{compute_icers}} or a tibble returned by
#'   \code{\link{combine_icers_results}}.
#' @param color_by,shape_by,facet_by Mapping options:
#'   "dataset", "subgroup_var", "subgroup_level", "both", or "none".
#'   If not provided, defaults are assigned automatically based on \code{mode}.
#' @param mode Character. Determines which part of the data to display:
#'   "Full" (Overall + Subgroups), "Overall" (only overall results),
#'   or "Subgroups" (only subgroup-specific results). Default = "Overall".
#' @param show_points,show_means,show_contours,show_lambdas Logical flags
#'   controlling display of bootstrap samples, mean points, contour layers,
#'   and willingness-to-pay (λ) reference lines.
#' @param contour_type Character. Either "non_parametric" or "ellipse".
#' @param contour_level Numeric vector. Probability levels for contour display.
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param palette Character. Color palette name for ggplot2 (default = "Set2").
#' @param ... Additional arguments passed to ggplot2 layers (e.g., size, alpha).
#'
#' @return A ggplot object representing the cost-effectiveness plane.
#' @export
plot.icers <- function(data,
                       color_by = NULL,
                       shape_by = NULL,
                       facet_by = NULL,
                       mode = "Overall",
                       show_points = TRUE,
                       show_means = TRUE,
                       show_contours = FALSE,
                       contour_type = "non_parametric",
                       contour_level = 0.95,
                       show_lambdas = TRUE,
                       facet_scales = "fixed",
                       palette = "Set2",
                       ...) {

  # ---- 1. Input validation and extraction ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- data$combined_replicates
    lambda_vals <- data$settings$lambda
  } else if (inherits(data, "data.frame")) {
    df <- data
    lambda_vals <- NULL
  } else {
    stop("Invalid input: must be a cea_results_list or combined tibble.")
  }

  # ---- 2. Ensure hierarchical identifier ----
  if (!"group_uid" %in% names(df)) {
    df$group_uid <- with(df, paste0(dataset, "_", subgroup_var, "_", subgroup_level))
  }

  # ---- 3. Determine visualization mode ----
  valid_modes <- c("Full", "Overall", "Subgroups")
  mode <- match.arg(mode, valid_modes)

  subgroup_mapped <- any(c(color_by, shape_by, facet_by) %in% c("subgroup_var", "subgroup_level"))
  if (mode == "Overall" && subgroup_mapped) {
    mode <- "Subgroups"
    message("Mode automatically set to 'Subgroups' based on mapping aesthetics.")
  }

  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), , drop = FALSE]
  } else if (mode == "Subgroups") {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), , drop = FALSE]
  }

  # ---- 4. Auto-assign aesthetic defaults by mode ----
  if (is.null(color_by) || color_by %in% c("none", "")) {
    color_by <- switch(mode,
                       "Overall" = "dataset",
                       "Subgroups" = "subgroup_level",
                       "Full" = "subgroup_level"
    )
  }

  if (is.null(shape_by) || shape_by %in% c("none", "")) {
    shape_by <- switch(mode,
                       "Overall" = "none",
                       "Subgroups" = "dataset",
                       "Full" = "dataset"
    )
  }

  if (is.null(facet_by) || facet_by %in% c("none", "")) {
    facet_by <- switch(mode,
                       "Overall" = "none",
                       "Subgroups" = "subgroup_var",
                       "Full" = "subgroup_var"
    )
  }

  # ---- 5. Active-variable filtering ----
  active_vars <- c(color_by, shape_by, facet_by)
  active_vars <- active_vars[active_vars %in% names(df) & active_vars != "none"]
  if (length(active_vars) > 0) {
    df <- df[!is.na(df[[active_vars[1]]]), , drop = FALSE]
  }

  # ---- 6. Build base layout ----
  base <- ce_plot_base(
    data = df,
    color_by = color_by,
    shape_by = shape_by,
    facet_by = facet_by,
    facet_scales = facet_scales,
    palette = palette
  )

  p <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  # ---- 7. Reference axes ----
  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70")

  # ---- 8. λ reference lines ----
  if (show_lambdas && !is.null(lambda_vals)) {
    for (l in lambda_vals) {
      p <- p + ggplot2::geom_abline(
        slope = l, intercept = 0,
        linetype = "dotted", colour = "grey60", linewidth = 0.5
      )
    }
  }

  # ---- 9. Bootstrap points (adaptive alpha) ----
  if (show_points) {
    n_repl <- length(unique(df$replicate))
    alpha_auto <- min(0.9, max(0.3, 0.9 - 0.15 * log10(n_repl / 100)))

    user_args <- list(...)
    alpha_val <- if (!"alpha" %in% names(user_args)) alpha_auto else user_args$alpha
    size_val  <- if (!"size" %in% names(user_args)) 1.2 else user_args$size

    p <- p + ggplot2::geom_point(
      data = df,
      mapping = ggplot2::aes(
        x = Delta_Effect,
        y = Delta_Cost,
        colour = .data[[color_var]],
        group = .data$group_uid,
        shape = if (!is.null(shape_var)) .data[[shape_var]] else NULL
      ),
      fill = "white",
      alpha = alpha_val,
      size = size_val,
      show.legend = TRUE
    )
  }

  # ---- 10. Mean points (darkened overlay when visible; legend always present) ----
  means <- df %>%
    dplyr::group_by(group_uid) %>%
    dplyr::summarise(
      Delta_Effect = mean(Delta_Effect, na.rm = TRUE),
      Delta_Cost = mean(Delta_Cost, na.rm = TRUE),
      dataset = dplyr::first(dataset),
      subgroup_var = dplyr::first(subgroup_var),
      subgroup_level = dplyr::first(subgroup_level),
      .groups = "drop"
    )
  means$group_uid <- means$group_uid

  # Color mapping and darkened tones
  unique_colors <- sort(unique(df[[color_var]]))
  palette_map <- RColorBrewer::brewer.pal(
    n = max(3, min(8, length(unique_colors))),
    name = palette
  )
  color_lookup <- setNames(
    rep(palette_map, length.out = length(unique_colors)),
    unique_colors
  )
  means$color_dark <- colorspace::darken(
    color_lookup[as.character(means[[color_var]])],
    amount = 0.25
  )

  # Determine fill type
  if (!is.null(shape_by) && shape_by != "none") {
    fill_val <- "white"
  } else {
    fill_val <- NA
  }

  # Primary layer (for legend only)
  p <- p + ggplot2::geom_point(
    data = means,
    mapping = ggplot2::aes(
      x = Delta_Effect,
      y = Delta_Cost,
      colour = .data[[color_var]],
      shape = if (!is.null(shape_var)) .data[[shape_var]] else NULL,
      group = .data$group_uid
    ),
    fill = fill_val,
    size = 3.8,
    stroke = 1.2,
    alpha = 0,             # Invisible but keeps legend entry
    show.legend = TRUE,
    inherit.aes = FALSE
  )

  # Darkened visible layer (if show_means = TRUE)
  if (show_means) {
    p <- p + ggplot2::geom_point(
      data = means,
      ggplot2::aes(
        x = Delta_Effect,
        y = Delta_Cost,
        shape = if (!is.null(shape_var)) .data[[shape_var]] else NULL
      ),
      colour = means$color_dark,
      fill = fill_val,
      size = 3.8,
      stroke = 1.2,
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  }


  # ---- 11. Contour layers ----
  if (show_contours) {
    unique_colors <- sort(unique(df[[color_var]]))
    palette_map <- RColorBrewer::brewer.pal(
      n = max(3, min(8, length(unique_colors))),
      name = palette
    )
    color_lookup <- setNames(
      rep(palette_map, length.out = length(unique_colors)),
      unique_colors
    )

    group_ids <- unique(df$group_uid)
    for (gid in group_ids) {
      sub_df <- df[df$group_uid == gid, , drop = FALSE]
      if (nrow(sub_df) < 30) next

      color_key <- as.character(unique(sub_df[[color_var]])[1])
      col_val <- colorspace::darken(color_lookup[[color_key]], amount = 0.25)

      dataset_val <- unique(sub_df$dataset)[1]
      subgroup_var_val <- unique(sub_df$subgroup_var)[1]
      subgroup_level_val <- unique(sub_df$subgroup_level)[1]

      if (contour_type == "ellipse") {
        mu <- colMeans(sub_df[, c("Delta_Effect", "Delta_Cost")], na.rm = TRUE)
        cov_mat <- stats::cov(sub_df[, c("Delta_Effect", "Delta_Cost")],
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
            Delta_Cost = coords[, 2],
            dataset = dataset_val,
            subgroup_var = subgroup_var_val,
            subgroup_level = subgroup_level_val
          )

          p <- p + ggplot2::geom_path(
            data = df_ellipse,
            ggplot2::aes(x = Delta_Effect, y = Delta_Cost),
            colour = ggplot2::alpha(col_val, 0.6),
            linewidth = 0.6,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
        }

      } else if (contour_type == "non_parametric") {
        dens <- tryCatch(MASS::kde2d(sub_df$Delta_Effect, sub_df$Delta_Cost, n = 200),
                         error = function(e) NULL)
        if (is.null(dens)) next
        df_dens <- expand.grid(Delta_Effect = dens$x, Delta_Cost = dens$y)
        df_dens$z <- as.vector(dens$z)
        df_dens$dataset <- dataset_val
        df_dens$subgroup_var <- subgroup_var_val
        df_dens$subgroup_level <- subgroup_level_val

        df_dens$prob <- df_dens$z / sum(df_dens$z, na.rm = TRUE)
        df_dens <- df_dens[order(df_dens$prob, decreasing = TRUE), ]
        df_dens$cumprob <- cumsum(df_dens$prob)
        levels_vec <- sort(unique(contour_level))
        levels_vec <- pmin(pmax(levels_vec, 0.001), 0.999)
        z_cuts <- vapply(levels_vec, function(lvl) {
          idx <- which.min(abs(df_dens$cumprob - lvl))
          df_dens$z[idx]
        }, numeric(1))

        p <- p + ggplot2::geom_contour(
          data = df_dens,
          ggplot2::aes(x = Delta_Effect, y = Delta_Cost, z = z),
          breaks = z_cuts,
          colour = ggplot2::alpha(col_val, 0.6),
          linewidth = 0.6,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      }
    }
  }

  # ---- 12. Final labels ----
  p <- p +
    ggplot2::labs(
      title = "Incremental Cost-Effectiveness Plane",
      x = expression(Delta * " Effect"),
      y = expression(Delta * " Cost"),
      colour = color_by,
      shape = shape_by
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(fill = NA, stroke = 1.2)
      ),
      shape = ggplot2::guide_legend(
        override.aes = list(fill = NA, stroke = 1.2)
      )
    )

  return(p)
}
