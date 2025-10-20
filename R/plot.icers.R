#' Plot incremental cost-effectiveness results (ICER plane)
#'
#' Produces a cost-effectiveness plane using bootstrap replicates from one or
#' several analyses. Supports flexible visualization modes for datasets and subgroups.
#'
#' @param data A `cea_results`, `cea_results_list`, or tibble from
#'   \code{\link{combine_icers_results}}.
#' @param mode Character. Visualization mode: one of
#'   `"overall"`, `"datasets"`, `"subgroups"`, or `"datasets_subgroups"`.
#' @param subgroups Optional character vector specifying which subgroup
#'   variables to include (for modes involving subgroups).
#' @param lambda Numeric. Willingness-to-pay threshold (optional).
#' @param facet Logical. If TRUE, facet the plot appropriately for the mode.
#' @param facet_by Character. For subgroup modes, determines the faceting scheme:
#'   `"vars"` (facet by subgroup variable) or `"levels"` (facet by subgroup level).
#' @param show_means Logical. If TRUE, adds a cross at the mean of each group/dataset.
#' @param show_contours Logical. If TRUE, adds 2D KDE density contours.
#' @param kde_contour Numeric vector with desired contour percentiles (default = 0.95).
#' @param kde_n Integer, grid resolution for density estimation (default = 200).
#' @param alpha Transparency for point clouds. If not specified, it is adjusted automatically.
#' @param size Point size (default = 1).
#' @param palette Color palette from RColorBrewer (default = "Set2").
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_vline geom_abline
#'   geom_path facet_wrap facet_grid labs theme_bw scale_color_brewer
#' @importFrom dplyr filter group_by summarise across all_of bind_rows
#' @importFrom MASS kde2d
#' @importFrom stats bw.SJ bw.nrd0
#' @export
plot.icers <- function(data,
                       mode = "overall",
                       subgroups = NULL,
                       lambda = NULL,
                       facet = FALSE,
                       facet_by = c("vars", "levels"),
                       show_means = TRUE,
                       show_contours = TRUE,
                       kde_contour = 0.95,
                       kde_n = 200,
                       alpha = NULL,
                       size = 1,
                       palette = "Set2") {

  facet_by <- match.arg(facet_by)

  # ---- Handle input type ----
  if (inherits(data, "cea_results_list") || inherits(data, "cea_results")) {
    if (is.null(data$combined_replicates)) {
      stop("The object has no 'combined_replicates' element. Run icers_base() first.")
    }
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a cea_results, cea_results_list, or combined tibble.")
  }

  required_cols <- c("Delta_Cost", "Delta_Effect", "dataset", "level", "subgroup")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  valid_modes <- c("overall", "datasets", "subgroups", "datasets_subgroups")
  if (!mode %in% valid_modes) {
    stop("Invalid mode. Choose one of: ", paste(valid_modes, collapse = ", "))
  }

  # ---- Filter data ----
  if (mode %in% c("subgroups", "datasets_subgroups")) {
    if (is.null(subgroups)) stop("Argument 'subgroups' must be provided for this mode.")
    df <- dplyr::filter(df, level %in% subgroups)
  } else {
    df <- dplyr::filter(df, level == "Overall")
  }

  # ---- Adaptive transparency ----
  if (is.null(alpha)) {
    if (!is.null(data$settings$R)) {
      n_reps <- data$settings$R
    } else if (!is.null(data$Overall$boot_results$R)) {
      n_reps <- data$Overall$boot_results$R
    } else if (!is.null(data[[1]]$Overall$boot_results$R)) {
      n_reps <- data[[1]]$Overall$boot_results$R
    } else {
      n_reps <- nrow(df)
    }
    alpha_points <- max(0.1, min(1, 1000 / n_reps))
  } else {
    alpha_points <- alpha
  }

  # ---- Helper: group means ----
  compute_means <- function(df, group_vars) {
    df %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::summarise(
        Delta_Effect = mean(Delta_Effect, na.rm = TRUE),
        Delta_Cost = mean(Delta_Cost, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # ---- Helper: KDE contour (stable version) ----
  kde_to_df <- function(x, y, group_id, probs = 0.95, n = 200) {
    n_points <- length(x)
    if (n_points < 10) return(NULL)

    # Bandwidth robusto: Sheather–Jones con fallback
    bw_x <- tryCatch(stats::bw.SJ(x), error = function(e) stats::bw.nrd0(x))
    bw_y <- tryCatch(stats::bw.SJ(y), error = function(e) stats::bw.nrd0(y))

    # Estimación KDE bivariada
    dens <- MASS::kde2d(x, y, n = n, h = c(bw_x, bw_y))

    # Calcular niveles de contorno por percentiles acumulados
    z <- sort(as.vector(dens$z))
    cdf <- cumsum(z) / sum(z)
    levels <- vapply(probs, function(p) approx(cdf, z, xout = 1 - p)$y, numeric(1))

    contour_df <- list()
    for (i in seq_along(levels)) {
      lines <- contourLines(dens$x, dens$y, dens$z, levels = levels[i])
      if (length(lines) == 0) next
      for (j in seq_along(lines)) {
        contour_df[[length(contour_df) + 1]] <- data.frame(
          x = lines[[j]]$x,
          y = lines[[j]]$y,
          kde_level = paste0(round(probs[i] * 100), "%"),
          group = group_id
        )
      }
    }

    if (length(contour_df) == 0) return(NULL)
    dplyr::bind_rows(contour_df)
  }

  # ---- Base plot constructor ----
  base_plot <- function(df, color_var, facet_formula = NULL) {
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = Delta_Effect, y = Delta_Cost, color = .data[[color_var]]
    )) +
      ggplot2::geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
      ggplot2::geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = expression(Delta * " Effect"),
        y = expression(Delta * " Cost"),
        title = "Cost-effectiveness plane"
      )

    if (!is.null(lambda)) {
      p <- p + ggplot2::geom_abline(
        slope = lambda, intercept = 0,
        color = "gray40", linetype = "dotted", size = 0.4
      )
    }

    if (!is.null(facet_formula)) p <- p + facet_formula
    p
  }

  # ---- Plot assembly by mode ----
  build_plot <- function(p, df, mean_vars, color_var) {

    # --- Contours (default 95%) ---
    if (show_contours) {
      contour_list <- lapply(unique(df[[color_var]]), function(gr) {
        sub_df <- df[df[[color_var]] == gr, ]
        kde_to_df(sub_df$Delta_Effect, sub_df$Delta_Cost, gr,
                  probs = kde_contour, n = kde_n)
      })
      contour_df <- do.call(rbind, contour_list)
      if (!is.null(contour_df)) {
        p <- p + ggplot2::geom_path(
          data = contour_df,
          ggplot2::aes(
            x = x, y = y,
            group = interaction(group, kde_level),
            color = group
          ),
          linewidth = 0.6, alpha = 1, show.legend = FALSE
        )
      }
    }

    # --- Points (replicates) ---
    p <- p + ggplot2::geom_point(alpha = alpha_points, size = size, show.legend = TRUE)

    # --- Means ---
    if (show_means) {
      mean_df <- compute_means(df, mean_vars)
      p <- p + ggplot2::geom_point(
        data = mean_df,
        ggplot2::aes(x = Delta_Effect, y = Delta_Cost),
        shape = 4, size = 3, stroke = 1.2, alpha = 1, show.legend = FALSE
      )
    }

    p
  }

  # ---- Mode definitions ----
  if (mode == "overall") {
    p <- base_plot(df, "dataset")
    p <- build_plot(p, df, mean_vars = "dataset", color_var = "dataset")
  }

  if (mode == "datasets") {
    facet_formula <- if (facet) ggplot2::facet_wrap(~ dataset) else NULL
    p <- base_plot(df, "dataset", facet_formula)
    p <- build_plot(p, df, mean_vars = "dataset", color_var = "dataset")
  }

  if (mode == "subgroups") {
    if (facet_by == "vars") {
      facet_formula <- ggplot2::facet_wrap(~ level)
      color_var <- "subgroup"
      mean_vars <- c("level", "subgroup")
    } else {
      facet_formula <- ggplot2::facet_wrap(~ subgroup)
      color_var <- "subgroup"
      mean_vars <- c("level", "subgroup")
    }
    p <- base_plot(df, color_var, facet_formula)
    p <- build_plot(p, df, mean_vars = mean_vars, color_var = color_var)
  }

  if (mode == "datasets_subgroups") {
    if (facet_by == "vars") {
      facet_formula <- ggplot2::facet_grid(dataset ~ level)
      color_var <- "subgroup"
      mean_vars <- c("dataset", "level", "subgroup")
    } else {
      facet_formula <- ggplot2::facet_grid(dataset ~ subgroup)
      color_var <- "subgroup"
      mean_vars <- c("dataset", "level", "subgroup")
    }
    p <- base_plot(df, color_var, facet_formula)
    p <- build_plot(p, df, mean_vars = mean_vars, color_var = color_var)
  }

  return(p)
}
