#' Plot incremental cost or effect distributions
#'
#' Visualizes the marginal distributions of incremental costs or effects (ΔCost, ΔEffect)
#' from bootstrap replications. Shares the same layout, modes, and mapping logic as
#' \code{plot.icers()}, \code{plot.ceacs()}, and \code{plot.evpis()}.
#'
#' @param data A \code{cea_results_list} or a tibble returned by
#'   \code{\link{combine_icers_results}} containing incremental results.
#' @param variable Character. Either "cost" (ΔCost) or "effect" (ΔEffect).
#' @param geom_type Character. One of "histogram", "density", or "boxplot". Default = "histogram".
#' @param color_by,shape_by,facet_by Mapping options.
#' @param mode Character. "Overall" (default), "Subgroups", or "Full".
#' @param facet_scales Character. Facet scaling ("fixed", "free", etc.).
#' @param palette Character. Color palette name.
#' @param bins Integer. Number of bins for histograms (default = 30).
#' @param ... Additional arguments passed to ggplot2 layers.
#'
#' @return A ggplot object.
#' @export
plot.marginals <- function(data,
                           variable = "cost",
                           geom_type = "histogram",
                           color_by = NULL,
                           shape_by = NULL,
                           facet_by = NULL,
                           mode = "Overall",
                           facet_scales = "fixed",
                           palette = "Set2",
                           bins = 30,
                           ...) {

  # ---- 1. Validate and extract combined data ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else stop("Invalid input: must be a cea_results_list or combined tibble.")

  # ---- 2. Ensure group identifier ----
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

  # ---- 4. Auto-assign aesthetics ----
  if (is.null(color_by) || color_by %in% c("none", "")) {
    color_by <- switch(mode,
                       "Overall" = "dataset",
                       "Subgroups" = "subgroup_level",
                       "Full" = "subgroup_level")
  }
  if (is.null(facet_by) || facet_by %in% c("none", "")) {
    facet_by <- switch(mode,
                       "Overall" = "none",
                       "Subgroups" = "subgroup_var",
                       "Full" = "subgroup_var")
  }

  # ---- 5. Base layout ----
  base <- ce_plot_base(
    data = df,
    color_by = color_by,
    shape_by = "none",
    facet_by = facet_by,
    facet_scales = facet_scales,
    palette = palette
  )
  p <- base$plot
  color_var <- base$color_var

  # ---- 6. Variable selection ----
  var_col <- switch(variable,
                    "cost" = "Delta_Cost",
                    "effect" = "Delta_Effect",
                    stop("variable must be 'cost' or 'effect'.")
  )

  # ---- 7. Determine transparency ----
  n_groups <- length(unique(df[[color_by]]))
  user_args <- list(...)
  alpha_val <- if (!"alpha" %in% names(user_args))
    if (n_groups > 3) 0.4 else 0.6
  else user_args$alpha

  # ---- 8. Geometry selection ----
  if (geom_type == "density") {
    line_w <- if (!"linewidth" %in% names(user_args)) 0.6 else user_args$linewidth
    p <- p + ggplot2::geom_density(
      data = df,
      mapping = ggplot2::aes(
        x = .data[[var_col]],
        colour = .data[[color_var]],
        fill   = .data[[color_var]],
        group  = .data[[color_var]]
      ),
      alpha = alpha_val,
      linewidth = line_w,
      inherit.aes = FALSE
    )

  } else if (geom_type == "histogram") {
    p <- p + ggplot2::geom_histogram(
      data = df,
      mapping = ggplot2::aes(
        x = .data[[var_col]],
        fill = .data[[color_var]],
        colour = .data[[color_var]],
        group = .data[[color_var]]
      ),
      bins = bins,
      alpha = alpha_val,
      position = "identity",
      inherit.aes = FALSE
    )

  } else if (geom_type == "boxplot") {
    p <- p + ggplot2::geom_boxplot(
      data = df,
      mapping = ggplot2::aes(
        x = .data[[color_var]],
        y = .data[[var_col]],
        fill = .data[[color_var]],
        colour = .data[[color_var]]
      ),
      alpha = alpha_val,
      outlier.shape = 21,
      inherit.aes = FALSE
    )
  } else stop("geom_type must be 'histogram', 'density', or 'boxplot'.")

  # ---- 9. Unified color scales ----
  p <- p +
    ggplot2::scale_fill_brewer(palette = palette, name = color_by) +
    ggplot2::scale_colour_brewer(palette = palette, guide = "none") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(colour = NA, alpha = 0.8)
      )
    )

  # ---- 10. Labels ----
  label_y <- if (geom_type == "boxplot") "Value" else "Density"
  label_title <- paste("Distribution of Δ", tools::toTitleCase(variable), sep = "")

  p <- p +
    ggplot2::labs(
      title = label_title,
      x = paste("Δ", tools::toTitleCase(variable)),
      y = label_y,
      colour = color_by
    )

  return(p)
}
