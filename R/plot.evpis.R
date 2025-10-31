#' Plot Expected Value of Perfect Information (EVPI)
#'
#' Computes and plots the Expected Value of Perfect Information (EVPI)
#' as a function of willingness-to-pay (λ) using bootstrap replicates.
#' Fully compatible with the visualization layout of \code{plot.icers()} and
#' \code{plot.ceacs()}, including visualization modes ("Overall", "Subgroups", "Full")
#' and automatic mapping of color, shape, and facet aesthetics.
#'
#' @param data A \code{cea_results_list} object from \code{compute_icers()} or a tibble
#'   returned by \code{combine_icers_results()}.
#' @param color_by,shape_by,facet_by Mapping options
#'   ("dataset", "subgroup_var", "subgroup_level", "both", or "none").
#'   If not specified, defaults are assigned automatically according to \code{mode}.
#' @param mode Character. Determines which part of the data to display:
#'   "Full" (Overall + Subgroups), "Overall" (only overall results),
#'   or "Subgroups" (only subgroup-specific results). Default = "Overall".
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param lambda_max Numeric. Maximum λ value shown on the x-axis. If NULL,
#'   estimated automatically from ΔCost and ΔEffect.
#' @param lambda_steps Integer. Number of λ values (default = 100).
#' @param palette Character. Color palette name (default = "Set2").
#' @param return_data Logical. If TRUE, returns the computed EVPI table.
#' @param ... Additional aesthetics (linewidth, alpha, etc.).
#'
#' @return A ggplot object (default) or tibble if `return_data = TRUE`.
#' @export
plot.evpis <- function(data,
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

  # ---- 1. Extract or validate input ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a 'cea_results_list' or combined tibble.")
  }

  # ---- 2. Ensure group_uid and apply mode filter ----
  df$group_uid <- with(df, paste0(dataset, "_", subgroup_var, "_", subgroup_level))

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

  df$group_uid <- with(df, paste0(dataset, "_", subgroup_var, "_", subgroup_level))

  # ---- 3. Auto-assign aesthetics by mode ----
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

  # ---- 4. Estimate λ range ----
  if (is.null(lambda_max)) {
    ratio <- with(df, Delta_Cost / Delta_Effect)
    ratio <- ratio[is.finite(ratio)]
    lambda_max <- max(0, quantile(abs(ratio), probs = 0.9, na.rm = TRUE))
    if (!is.finite(lambda_max) || lambda_max <= 0) lambda_max <- 100000
  }
  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  # ---- 5. Compute EVPI for each group ----
  compute_evpi <- function(df) {
    results <- lapply(lambda_seq, function(l) {
      NMB <- l * df$Delta_Effect - df$Delta_Cost
      perfect <- mean(pmax(0, NMB), na.rm = TRUE)
      current <- max(0, mean(NMB, na.rm = TRUE))
      tibble::tibble(lambda = l, EVPI = perfect - current)
    })
    dplyr::bind_rows(results)
  }

  evpi_df <- df %>%
    dplyr::group_by(group_uid) %>%
    dplyr::group_modify(~ compute_evpi(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      df %>%
        dplyr::select(group_uid, dataset, subgroup_var, subgroup_level) %>%
        dplyr::distinct(),
      by = "group_uid"
    )

  if (return_data) return(evpi_df)

  # ---- 6. Build layout ----
  base <- ce_plot_base(
    data = evpi_df,
    color_by = color_by,
    shape_by = shape_by,
    facet_by = facet_by,
    facet_scales = facet_scales,
    palette = palette
  )

  p <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  # ---- 7. Line aesthetics ----
  user_args <- list(...)
  lw <- if (!"linewidth" %in% names(user_args)) 1 else user_args$linewidth
  alpha_val <- if (!"alpha" %in% names(user_args)) 1 else user_args$alpha

  # ---- 8. Draw EVPI curves ----
  p <- p +
    ggplot2::geom_line(
      data = evpi_df,
      mapping = ggplot2::aes(
        x = lambda,
        y = EVPI,
        colour = .data[[color_var]],
        group = .data$group_uid,
        linetype = if (!is.null(shape_var)) .data[[shape_var]] else NULL
      ),
      linewidth = lw,
      alpha = alpha_val
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y = "EVPI (Monetary units)",
      colour = color_by,
      linetype = shape_by
    )

  return(p)
}
