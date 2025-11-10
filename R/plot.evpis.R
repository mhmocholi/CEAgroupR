#' Plot Expected Value of Perfect Information (EVPI)
#'
#' Computes and plots the Expected Value of Perfect Information (EVPI)
#' as a function of willingness-to-pay (λ) using bootstrap replicates.
#' Fully compatible with the visualization layout of \code{plot.icers()} and
#' \code{plot.ceacs()}, including visualization modes ("Overall", "Subgroups", "Full")
#' and automatic mapping of color, shape, and facet aesthetics.
#'
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

  # ---- 1. Extract input ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else stop("Invalid input: must be a cea_results_list or combined tibble.")

  # ---- 2. Group ID and mode filter ----
  df$group_uid <- with(df, paste0(dataset, "_", subgroup_var, "_", subgroup_level))
  valid_modes <- c("Full", "Overall", "Subgroups")
  mode <- match.arg(mode, valid_modes)

  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), , drop = FALSE]
  } else if (mode == "Subgroups") {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), , drop = FALSE]
  }

  # ---- 3. Aesthetics defaults ----
  if (is.null(color_by) || color_by %in% c("none", "")) {
    color_by <- switch(mode, "Overall" = "dataset",
                       "Subgroups" = "subgroup_level",
                       "Full" = "subgroup_level")
  }
  if (is.null(shape_by) || shape_by %in% c("none", "")) {
    shape_by <- switch(mode, "Overall" = "none",
                       "Subgroups" = "dataset",
                       "Full" = "dataset")
  }
  if (is.null(facet_by) || facet_by %in% c("none", "")) {
    facet_by <- switch(mode, "Overall" = "none",
                       "Subgroups" = "subgroup_var",
                       "Full" = "subgroup_var")
  }

  # ---- 4. λ range ----
  if (is.null(lambda_max)) {
    ratio <- with(df, Delta_Cost / Delta_Effect)
    ratio <- ratio[is.finite(ratio) & ratio > 0]
    lambda_max <- max(0, quantile(ratio, 0.9, na.rm = TRUE))
    if (!is.finite(lambda_max) || lambda_max <= 0) lambda_max <- 100000
  }
  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  # ---- 5. Compute EVPI ----
  compute_evpi <- function(df) {
    results <- lapply(lambda_seq, function(l) {
      NMB <- l * df$Delta_Effect - df$Delta_Cost
      perfect <- mean(pmax(0, NMB), na.rm = TRUE)
      current  <- max(0, mean(NMB, na.rm = TRUE))
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

  # ---- 6. Layout ----
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

  # ---- 7. Draw EVPI curves ----
  args <- list(...)
  lw <- if (!"linewidth" %in% names(args)) 1 else args$linewidth
  alpha_val <- if (!"alpha" %in% names(args)) 1 else args$alpha

  p <- p +
    ggplot2::geom_line(
      data = evpi_df,
      ggplot2::aes(
        x = lambda,
        y = EVPI,
        colour = .data[[color_var]],
        group = .data$group_uid,
        linetype = if (!is.null(shape_var)) .data[[shape_var]] else NULL
      ),
      linewidth = lw,
      alpha = alpha_val,
      show.legend = TRUE
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y = "EVPI (Monetary units)",
      colour = color_by,
      linetype = shape_by
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(alpha = 1, linewidth = 1.2)),
      linetype = ggplot2::guide_legend(override.aes = list(alpha = 1, linewidth = 1.2))
    )

  return(p)
}
