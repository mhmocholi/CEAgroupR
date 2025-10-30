#' Plot Expected Value of Perfect Information (EVPI)
#'
#' Computes and plots the Expected Value of Perfect Information (EVPI)
#' as a function of willingness-to-pay (λ) using bootstrap replicates from one or
#' several analyses. When the bootstrap replicates represent incremental
#' differences between two strategies (ΔCost, ΔEffect), the EVPI is computed as
#' the expected gain from perfect information regarding the adoption decision.
#'
#' The graphical structure, argument order, and aesthetics are identical to
#' \code{\link{plot.ceac}} and \code{\link{plot.icers}}.
#'
#' @param object A \code{cea_results_list} or tibble returned by
#'   \code{\link{combine_icers_results}}.
#' @param color_by,shape_by,facet_by Mapping options
#'   ("dataset", "subgroup_var", "subgroup_level", "both", or "none").
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param lambda_max Numeric. Maximum λ value shown on the x-axis. If NULL,
#'   estimated automatically from ΔCost and ΔEffect.
#' @param lambda_steps Integer. Number of λ values used to build the curves
#'   (default = 100; higher = smoother curves).
#' @param palette Character. Color palette name for ggplot2 (default = "Set2").
#'
#' @return A ggplot object representing the EVPI curves.
#' @export
plot.evpis <- function(object,
                       color_by = "dataset",
                       shape_by = "none",
                       facet_by = "none",
                       facet_scales = "fixed",
                       lambda_max = NULL,
                       lambda_steps = 100,
                       palette = "Set2") {

  # ---- 1. Prepare combined data ----
  if (inherits(object, "cea_results_list")) {
    if (is.null(object$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- object$combined_replicates
  } else if (inherits(object, "data.frame")) {
    df <- object
  } else {
    stop("Invalid input: must be a cea_results_list or a combined tibble.")
  }

  # Add subgroup columns if missing
  if (!"subgroup_var" %in% names(df)) df$subgroup_var <- "Overall"
  if (!"subgroup_level" %in% names(df)) df$subgroup_level <- "Overall"

  # ---- 2. Estimate λ range ----
  if (is.null(lambda_max)) {
    ratio <- with(df, Delta_Cost / Delta_Effect)
    lambda_max <- max(0, quantile(ratio, probs = 0.9, na.rm = TRUE))
    if (!is.finite(lambda_max) || lambda_max <= 0) lambda_max <- 100000
  }

  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  # ---- 3. Compute EVPI (incremental definition) ----
  evpi_df <- df %>%
    dplyr::group_by(dataset, subgroup_var, subgroup_level) %>%
    dplyr::group_modify(~ {

      message("---- Calculating EVPI ----")
      message("Dataset: ", unique(.x$dataset))
      message("Subgroup: ", unique(.x$subgroup_var), " = ", unique(.x$subgroup_level))

      # Initialize a list to store intermediate results
      results_list <- vector("list", length(lambda_seq))

      for (i in seq_along(lambda_seq)) {
        l <- lambda_seq[i]

        NMB <- l * .x$Delta_Effect - .x$Delta_Cost

        # Perfect information: expected value of the optimal choice
        perfect_info <- mean(pmax(0, NMB), na.rm = TRUE)

        # Current information: NMB using expected values
        current_info <- max(0, mean(NMB, na.rm = TRUE))

        EVPI_value <- perfect_info - current_info

        # Store each iteration as a row tibble
        results_list[[i]] <- tibble::tibble(
          lambda = l,
          EVPI = EVPI_value
        )

        # Print current step to console
        message(
          sprintf("λ = %-8.0f | EVPI = %.4f | mean(ΔE)=%.5f | mean(ΔC)=%.2f",
                  l,
                  EVPI_value,
                  mean(.x$Delta_Effect, na.rm = TRUE),
                  mean(.x$Delta_Cost, na.rm = TRUE))
        )
      }

      # Combine all λ iterations into one tibble
      dplyr::bind_rows(results_list) %>%
        dplyr::mutate(
          dataset = unique(.x$dataset),
          subgroup_var = unique(.x$subgroup_var),
          subgroup_level = unique(.x$subgroup_level)
        )
    }) %>%
    dplyr::ungroup()


  # ---- 4. Build ggplot (identical geometry style to plot.ceac) ----
  color_var <- if (color_by %in% names(evpi_df)) color_by else "dataset"

  p <- ggplot2::ggplot(
    evpi_df,
    ggplot2::aes(
      x = lambda,
      y = EVPI,
      colour = .data[[color_var]],
      group = .data[[color_var]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +  # only simple lines
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(limits = c(0, lambda_max), expand = c(0, 0)) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y = "EVPI (Monetary units)",
      colour = "Dataset"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )

  if (facet_by %in% names(evpi_df) && facet_by != "none") {
    p <- p + ggplot2::facet_wrap(~ .data[[facet_by]], scales = facet_scales)
  }

  return(p)
}
