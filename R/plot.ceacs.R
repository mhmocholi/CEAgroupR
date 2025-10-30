#' Plot Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Computes and plots the probability of cost-effectiveness as a function of
#' willingness-to-pay (λ) using bootstrap replicates from one or several analyses.
#' The structure of grouping, coloring, and faceting is consistent with
#' \code{\link{plot.icers}}.
#'
#' @param data A \code{cea_results_list} or tibble returned by
#'   \code{\link{combine_icers_results}}.
#' @param color_by,shape_by,facet_by Mapping options
#'   ("dataset", "subgroup_var", "subgroup_level", "both", or "none").
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param lambda_max Numeric. Maximum λ value shown on the x-axis.
#'   If NULL, estimated automatically from ΔCost and ΔEffect.
#' @param lambda_steps Integer. Number of λ values used to build the curves
#'   (default = 100; higher = smoother curves).
#' @param palette Character. Color palette name for ggplot2 (default = "Set2").
#' @param return_data Logical. If TRUE, returns the CEAC dataframe instead of a ggplot.
#' @param ... Additional arguments passed to ggplot2 layers (e.g., linewidth, alpha).
#'
#' @importFrom colorspace darken
#' @return A ggplot object (default) or a tibble if `return_data = TRUE`.
#' @export
plot.ceacs <- function(data,
                       color_by = "dataset",
                       shape_by = "none",
                       facet_by = "none",
                       facet_scales = "fixed",
                       lambda_max = NULL,
                       lambda_steps = 100,
                       palette = "Set2",
                       return_data = FALSE,
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

  # ---- 2. Estimate λ range ----
  if (is.null(lambda_max)) {
    ratio <- with(df, Delta_Cost / Delta_Effect)
    lambda_max <- max(0, quantile(ratio, probs = 0.9, na.rm = TRUE))
    if (!is.finite(lambda_max) || lambda_max <= 0) lambda_max <- 100000
  }

  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  # ---- 3. Compute CEAC empirically from bootstrap replicates ----
  compute_ceac <- function(df) {
    probs <- numeric(length(lambda_seq))
    n <- nrow(df)
    for (i in seq_along(lambda_seq)) {
      l <- lambda_seq[i]
      probs[i] <- sum(df$Delta_Cost < l * df$Delta_Effect, na.rm = TRUE) / n
    }
    tibble::tibble(lambda = lambda_seq, prob = cummax(probs))
  }

  group_vars <- intersect(c("dataset", "subgroup_var", "subgroup_level"), names(df))

  ceac_df <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::group_modify(~ compute_ceac(.x)) %>%
    dplyr::ungroup()

  # ---- Optional: Return the CEAC data for exploration ----
  if (return_data) {
    return(ceac_df)
  }

  # ---- 4. Define darkened colors for emphasis ----
  gg_pal <- suppressWarnings(
    ggplot2::scale_color_brewer(palette = palette)$palette(length(unique(df[[color_by]])))
  )
  names(gg_pal) <- sort(unique(df[[color_by]]))
  dark_map <- sapply(gg_pal, function(col) colorspace::darken(col, amount = 0.15))

  # ---- 5. Build ggplot layout ----
  color_var <- if (color_by %in% names(ceac_df)) color_by else "dataset"
  user_args <- list(...)

  linewidth_val <- if (!"linewidth" %in% names(user_args)) 1 else user_args$linewidth
  alpha_val <- if (!"alpha" %in% names(user_args)) 1 else user_args$alpha

  p <- ggplot2::ggplot(
    ceac_df,
    ggplot2::aes(
      x = lambda, y = prob,
      colour = .data[[color_var]],
      group = .data[[color_var]]
    )
  ) +
    ggplot2::geom_line(linewidth = linewidth_val, alpha = alpha_val, show.legend = TRUE) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(limits = c(0, lambda_max), expand = c(0, 0)) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::labs(
      title = "Cost-Effectiveness Acceptability Curve (CEAC)",
      x = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y = "Probability cost-effective",
      colour = color_by
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  # ---- 6. Add darkened overlay for visual emphasis (no legend) ----
  for (grp in unique(ceac_df[[color_var]])) {
    df_grp <- ceac_df[ceac_df[[color_var]] == grp, , drop = FALSE]
    p <- p + ggplot2::geom_line(
      data = df_grp,
      ggplot2::aes(x = lambda, y = prob),
      colour = dark_map[[grp]],
      linewidth = linewidth_val * 1.1,
      show.legend = FALSE
    )
  }

  # ---- 7. Add facet structure if required ----
  if (facet_by %in% names(ceac_df) && facet_by != "none") {
    p <- p + ggplot2::facet_wrap(~ .data[[facet_by]], scales = facet_scales)
  }

  return(p)
}
