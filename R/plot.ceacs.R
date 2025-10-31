#' Plot Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Computes and plots the probability of cost-effectiveness as a function of
#' willingness-to-pay (λ) using bootstrap replicates from one or several analyses.
#' Fully compatible with the visualization layout of \code{plot.icers()}, including
#' visualization modes ("Overall", "Subgroups", "Full") and automatic mapping of
#' color, shape, and facet aesthetics.
#'
#' @param data A \code{cea_results_list} object returned by \code{\link{compute_icers}}
#'   or a tibble returned by \code{\link{combine_icers_results}}.
#' @param color_by,shape_by,facet_by Mapping options
#'   ("dataset", "subgroup_var", "subgroup_level", "both", or "none").
#'   If not specified, defaults are assigned automatically according to \code{mode}.
#' @param mode Character. Determines which part of the data to display:
#'   "Full" (Overall + Subgroups), "Overall" (only overall results),
#'   or "Subgroups" (only subgroup-specific results). Default = "Overall".
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param lambda_max Numeric. Maximum λ value shown on the x-axis.
#'   If NULL, estimated automatically from ΔCost and ΔEffect.
#' @param lambda_steps Integer. Number of λ values used to build the curves
#'   (default = 100; higher = smoother curves).
#' @param palette Character. Color palette name for ggplot2 (default = "Set2").
#' @param return_data Logical. If TRUE, returns the CEAC dataframe instead of a ggplot.
#' @param ... Additional arguments passed to ggplot2 layers (e.g., linewidth, alpha).
#'
#' @return A ggplot object (default) or a tibble if `return_data = TRUE`.
#' @export
plot.ceacs <- function(data,
                       color_by = NULL,
                       shape_by = NULL,
                       facet_by = NULL,
                       mode = "Overall",
                       facet_scales = "fixed",
                       lambda_max = NULL,
                       lambda_steps = 100,
                       palette = "Set2",
                       return_data = FALSE,
                       ...) {

  # ---- 1. Detect input type and extract combined data ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates)) {
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    }
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a 'cea_results_list' or a tibble of combined replicates.")
  }

  # ---- 2. Ensure group_uid and filter by mode ----
  df$group_uid <- with(df, paste0(dataset, "_", subgroup_var, "_", subgroup_level))

  # ---- Visualization mode filter ----
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

  # Recreate group_uid after filtering
  df$group_uid <- with(df, paste0(dataset, "_", subgroup_var, "_", subgroup_level))


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

  # ---- 5. Estimate λ range ----
  if (is.null(lambda_max)) {
    ratio <- with(df, Delta_Cost / Delta_Effect)
    ratio <- ratio[is.finite(ratio)]
    lambda_max <- max(0, quantile(abs(ratio), probs = 0.9, na.rm = TRUE))
    if (!is.finite(lambda_max) || lambda_max <= 0) lambda_max <- 100000
  }
  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  # ---- 6. Compute CEAC empirically ----
  compute_ceac <- function(df) {
    n <- nrow(df)
    probs <- numeric(length(lambda_seq))
    for (i in seq_along(lambda_seq)) {
      l <- lambda_seq[i]
      probs[i] <- sum(df$Delta_Cost < l * df$Delta_Effect, na.rm = TRUE) / n
    }
    tibble::tibble(lambda = lambda_seq, prob = cummax(probs))
  }

  ceac_df <- df %>%
    dplyr::group_by(group_uid) %>%
    dplyr::group_modify(~ compute_ceac(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      df %>%
        dplyr::select(group_uid, dataset, subgroup_var, subgroup_level) %>%
        dplyr::distinct(),
      by = "group_uid"
    )

  # ---- 7. Return data option ----
  if (return_data) return(ceac_df)

  # ---- 8. Build layout using ce_plot_base ----
  base <- ce_plot_base(
    data = ceac_df,
    color_by = color_by,
    shape_by = shape_by,
    facet_by = facet_by,
    facet_scales = facet_scales,
    palette = palette
  )

  p <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  # ---- 9. Define visual parameters ----
  user_args <- list(...)
  line_width <- if (!"linewidth" %in% names(user_args)) 1 else user_args$linewidth
  alpha_val <- if (!"alpha" %in% names(user_args)) 1 else user_args$alpha

  # ---- 10. Add CEAC curves ----
  p <- p +
    ggplot2::geom_line(
      data = ceac_df,
      mapping = ggplot2::aes(
        x = lambda,
        y = prob,
        colour = .data[[color_var]],
        group = .data$group_uid,
        linetype = if (!is.null(shape_var)) .data[[shape_var]] else NULL
      ),
      linewidth = line_width,
      alpha = alpha_val
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(
      title = "Cost-Effectiveness Acceptability Curve (CEAC)",
      x = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y = "Probability cost-effective",
      colour = color_by,
      linetype = shape_by
    )

  return(p)
}
