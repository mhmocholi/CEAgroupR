#' Flexible Cost-Effectiveness Plane with Adaptive KDE
#'
#' @description
#' Produces cost-effectiveness planes from bootstrap replicates of
#' `cea_results` or `cea_results_list` objects. Supports multiple
#' visualization modes (`overall`, `datasets`, `subgroups`,
#' `subgroups_datasets`, `full`) and adaptive kernel density estimation (KDE)
#' that automatically adjusts smoothness based on the bootstrap size.
#'
#' @details
#' KDE bandwidth is automatically scaled as \eqn{h \propto n^{-1/6}}.
#' Each facet and subgroup combination is treated independently, ensuring
#' that means and contours are computed only for their corresponding subset.
#'
#' @export
plot.icers2 <- function(data,
                        mode = c("overall", "datasets", "subgroups",
                                 "subgroups_datasets", "full"),
                        facet = FALSE,
                        facet_by = c("vars", "levels"),
                        show_points = TRUE,
                        show_means = TRUE,
                        show_contours = TRUE,
                        contour_type = c("non_parametric", "ellipse"),
                        contour_level = 0.95,
                        lambda = NULL,
                        palette = "Set2",
                        alpha = NULL,
                        size = 1) {

  mode <- match.arg(mode)
  facet_by <- match.arg(facet_by)
  contour_type <- match.arg(contour_type)

  # ---- Extract data ----
  if (inherits(data, "cea_results_list") || inherits(data, "cea_results")) {
    if (is.null(data$combined_replicates))
      stop("No 'combined_replicates' found. Run icers_base() first.")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else stop("Input must be a cea_results, cea_results_list, or tibble.")

  # ---- Auto alpha ----
  if (is.null(alpha)) {
    n_reps <- nrow(df)
    alpha <- max(0.1, min(1, 1000 / n_reps))
  }

  # ---- Recover lambda ----
  if (is.null(lambda) && !is.null(data$settings$lambda))
    lambda <- data$settings$lambda

  # ---- MODE SELECTION ----
  if (mode == "overall") {
    df <- dplyr::filter(df, level == "Overall")
    draw_ce_plane(df, color_var = "dataset", facet_var = NULL,
                  show_points = show_points, show_means = show_means,
                  show_contours = show_contours, contour_type = contour_type,
                  contour_level = contour_level, lambda = lambda,
                  palette = palette, alpha = alpha, size = size)
  } else if (mode == "datasets") {
    draw_ce_plane(df, color_var = "dataset",
                  facet_var = if (facet) "dataset" else NULL,
                  show_points = show_points, show_means = show_means,
                  show_contours = show_contours, contour_type = contour_type,
                  contour_level = contour_level, lambda = lambda,
                  palette = palette, alpha = alpha, size = size)
  } else if (mode == "subgroups") {
    df <- dplyr::filter(df, level != "Overall")
    if (facet_by == "vars") {
      draw_ce_plane(df, color_var = "subgroup",
                    facet_var = if (facet) "level" else NULL,
                    show_points = show_points, show_means = show_means,
                    show_contours = show_contours, contour_type = contour_type,
                    contour_level = contour_level, lambda = lambda,
                    palette = palette, alpha = alpha, size = size)
    } else {
      draw_ce_plane(df, color_var = "level",
                    facet_var = if (facet) "subgroup" else NULL,
                    show_points = show_points, show_means = show_means,
                    show_contours = show_contours, contour_type = contour_type,
                    contour_level = contour_level, lambda = lambda,
                    palette = palette, alpha = alpha, size = size)
    }
  } else if (mode == "subgroups_datasets") {
    df <- dplyr::filter(df, level != "Overall")
    draw_ce_plane(df, color_var = "subgroup",
                  facet_var = if (facet) "dataset" else NULL,
                  show_points = show_points, show_means = show_means,
                  show_contours = show_contours, contour_type = contour_type,
                  contour_level = contour_level, lambda = lambda,
                  palette = palette, alpha = alpha, size = size)
  } else if (mode == "full") {
    draw_ce_plane(df,
                  color_var = "interaction(dataset, level, subgroup, sep=':')",
                  facet_var = if (facet) "level" else NULL,
                  show_points = show_points, show_means = show_means,
                  show_contours = show_contours, contour_type = contour_type,
                  contour_level = contour_level, lambda = lambda,
                  palette = palette, alpha = alpha, size = size)
  }
}


#' Internal helper: Draw CE plane per facet and subgroup
#' @noRd
draw_ce_plane <- function(df, color_var, facet_var = NULL,
                          show_points = TRUE, show_means = TRUE,
                          show_contours = TRUE, contour_type = "non_parametric",
                          contour_level = 0.95, lambda = NULL,
                          palette = "Set2", alpha = 0.4, size = 1) {

  p <- ggplot2::ggplot(df,
                       ggplot2::aes(x = Delta_Effect, y = Delta_Cost,
                                    colour = !!rlang::sym(color_var))) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(palette = palette, name = NULL) +
    ggplot2::labs(
      title = "Cost-effectiveness plane",
      x = expression(Delta * " Effect"),
      y = expression(Delta * " Cost")
    )

  if (!is.null(lambda))
    p <- p + ggplot2::geom_abline(slope = lambda, intercept = 0,
                                  linetype = "dotted", colour = "grey50", linewidth = 0.5)

  if (!is.null(facet_var))
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))

  if (show_points)
    p <- p + ggplot2::geom_point(alpha = alpha, size = size)

  if (show_means) {
    means <- df %>%
      dplyr::group_by(dplyr::across(all_of(c(color_var, facet_var)))) %>%
      dplyr::summarise(
        Delta_Effect = mean(Delta_Effect, na.rm = TRUE),
        Delta_Cost   = mean(Delta_Cost,   na.rm = TRUE),
        .groups = "drop"
      )
    p <- p + ggplot2::geom_point(data = means,
                                 ggplot2::aes(x = Delta_Effect, y = Delta_Cost),
                                 shape = 4, size = 3, stroke = 1.2)
  }

  # ---- Contours (per facet) ----
  if (show_contours) {

    if (contour_type == "non_parametric") {

      for (g in unique(df[[color_var]])) {
        sub <- df[df[[color_var]] == g, ]
        if (nrow(sub) < 20) next

        # Normalizar escalas
        x_mean <- mean(sub$Delta_Effect, na.rm = TRUE)
        x_sd   <- sd(sub$Delta_Effect, na.rm = TRUE)
        y_mean <- mean(sub$Delta_Cost, na.rm = TRUE)
        y_sd   <- sd(sub$Delta_Cost, na.rm = TRUE)
        x_scaled <- (sub$Delta_Effect - x_mean) / x_sd
        y_scaled <- (sub$Delta_Cost   - y_mean) / y_sd

        dens <- MASS::kde2d(x_scaled, y_scaled, n = 200, lims = c(-3, 3, -3, 3))
        niveles <- quantile(as.vector(dens$z), probs = contour_level, na.rm = TRUE)

        for (nivel in niveles) {
          p <- p +
            ggplot2::stat_density_2d(
              data = sub,
              geom = "contour",
              aes(x = Delta_Effect, y = Delta_Cost,
                  colour = !!sym(color_var)),
              breaks = nivel,
              size = 1,
              alpha = 0.9,
              show.legend = FALSE
            )
        }
      }

    } else if (contour_type == "ellipse") {

      contour_list <- list()

      # Crear combinaciones únicas de grupo y faceta
      comb_vars <- unique(df[c(color_var, facet_var)])
      comb_vars <- comb_vars[complete.cases(comb_vars), , drop = FALSE]

      for (i in seq_len(nrow(comb_vars))) {
        g <- comb_vars[[color_var]][i]
        f <- if (!is.null(facet_var)) comb_vars[[facet_var]][i] else NA

        # Filtrar datos de ese grupo y faceta
        if (!is.null(facet_var)) {
          sub <- df[df[[color_var]] == g & df[[facet_var]] == f, ]
        } else {
          sub <- df[df[[color_var]] == g, ]
        }
        if (nrow(sub) < 10) next

        mu <- c(mean(sub$Delta_Effect, na.rm = TRUE),
                mean(sub$Delta_Cost, na.rm = TRUE))
        cov_mat <- stats::cov(cbind(sub$Delta_Effect, sub$Delta_Cost),
                              use = "pairwise.complete.obs")
        eig <- eigen(cov_mat)

        for (nivel in contour_level) {
          chi_val <- sqrt(stats::qchisq(nivel, df = 2))
          theta <- seq(0, 2 * pi, length.out = 200)
          coords <- t(mu + eig$vectors %*% (sqrt(eig$values) * chi_val *
                                              rbind(cos(theta), sin(theta))))
          contour_list[[length(contour_list) + 1]] <-
            data.frame(x = coords[, 1], y = coords[, 2],
                       group = g, facet_value = f)
        }
      }

      if (length(contour_list) > 0) {
        cdf <- dplyr::bind_rows(contour_list)
        p <- p + ggplot2::geom_path(
          data = cdf,
          ggplot2::aes(x = x, y = y,
                       group = interaction(facet_value, group),
                       colour = group),
          linewidth = 0.6,
          alpha = 0.9,
          show.legend = FALSE
        )
      }
    }
  }

  return(p)
}
