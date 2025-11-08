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
                           ...) {

  # ---- 1. Validate and extract combined data ----
  if (inherits(data, "cea_results_list")) {
    if (is.null(data$combined_replicates))
      stop("The object has no 'combined_replicates' element. Run compute_icers() first.")
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a cea_results_list or combined tibble.")
  }

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

  # ---- 4. Auto-assign aesthetics by mode ----
  if (is.null(color_by) || color_by %in% c("none", "")) {
    color_by <- switch(mode,
                       "Overall" = "dataset",
                       "Subgroups" = "subgroup_level",
                       "Full" = "subgroup_level"
    )
  }

  if (is.null(facet_by) || facet_by %in% c("none", "")) {
    facet_by <- switch(mode,
                       "Overall" = "none",
                       "Subgroups" = "subgroup_var",
                       "Full" = "subgroup_var"
    )
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

  # ---- 6. Select variable to plot ----
  var_col <- switch(variable,
                    "cost" = "Delta_Cost",
                    "effect" = "Delta_Effect",
                    stop("variable must be 'cost' or 'effect'.")
  )

  # ---- 7. Add geometry (synchronized colour/fill palettes) ----
  user_args <- list(...)
  alpha_val <- if (!"alpha" %in% names(user_args)) 0.5 else user_args$alpha

  if (geom_type == "density") {
    p <- p + ggplot2::geom_density(
      data = df,
      mapping = ggplot2::aes(
        x = .data[[var_col]],
        colour = .data[[color_var]],
        fill   = .data[[color_var]],
        group  = .data[[color_var]]
      ),
      alpha = alpha_val,
      linewidth = 1,
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
      bins = 30,
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
  } else {
    stop("geom_type must be 'histogram', 'density', or 'boxplot'.")
  }

  # ---- 8. Colour scales (unified for colour and fill) ----
  p <- p +
    ggplot2::scale_color_brewer(palette = palette, name = color_by) +
    ggplot2::scale_fill_brewer(palette = palette, name = color_by) +
    ggplot2::guides(fill = "none")

  # ---- 9. Labels ----
  label_y <- if (geom_type == "boxplot") "Value" else "Density"
  label_title <- paste("Distribution of Δ", tools::toTitleCase(variable), sep = "")

  p <- p +
    ggplot2::labs(
      title = label_title,
      x = paste("Δ", tools::toTitleCase(variable)),
      y = label_y,
      colour = color_by
    )
  # ---- 10. Optional: pairwise comparison annotations (by geometry type, only in Subgroups mode) ----
  if (mode == "Subgroups" && !is.null(list(...)$compare) && list(...)$compare == TRUE) {
    alpha_sig <- if (!is.null(list(...)$alpha_sig)) list(...)$alpha_sig else 0.05
    var_col <- switch(variable,
                      "cost" = "Delta_Cost",
                      "effect" = "Delta_Effect"
    )

    group_var <- df[[color_by]]
    unique_groups <- unique(group_var)

    if (length(unique_groups) > 1) {
      combos <- combn(unique_groups, 2, simplify = FALSE)
      results <- lapply(combos, function(grp) {
        x <- df[df[[color_by]] == grp[1], var_col, drop = TRUE]
        y <- df[df[[color_by]] == grp[2], var_col, drop = TRUE]
        pval <- tryCatch(stats::t.test(x, y)$p.value, error = function(e) NA)
        mean1 <- mean(x, na.rm = TRUE)
        mean2 <- mean(y, na.rm = TRUE)
        data.frame(group1 = grp[1], group2 = grp[2],
                   mean1 = mean1, mean2 = mean2, pval = pval)
      })
      results_df <- do.call(rbind, results)
      results_sig <- results_df[!is.na(results_df$pval) & results_df$pval < alpha_sig, ]

      if (nrow(results_sig) > 0) {
        # Format p-values
        results_sig$p_label <- ifelse(
          results_sig$pval < 0.001,
          "p < 0.001",
          paste0("p = ", formatC(results_sig$pval, digits = 3, format = "f"))
        )

        if (geom_type == "boxplot") {
          # --- Standard positioning for boxplots ---
          y_max <- max(df[[var_col]], na.rm = TRUE)
          range_y <- max(df[[var_col]], na.rm = TRUE) - min(df[[var_col]], na.rm = TRUE)
          step <- 0.10 * range_y
          results_sig$y_pos <- y_max + seq_along(results_sig$pval) * step

          p <- p +
            ggplot2::geom_segment(
              data = results_sig,
              ggplot2::aes(
                x = match(group1, unique_groups),
                xend = match(group2, unique_groups),
                y = y_pos,
                yend = y_pos
              ),
              colour = "black",
              linewidth = 0.8,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_text(
              data = results_sig,
              ggplot2::aes(
                x = (match(group1, unique_groups) + match(group2, unique_groups)) / 2,
                y = y_pos + (step / 3),
                label = p_label
              ),
              size = 3.2,
              inherit.aes = FALSE
            )

        } else if (geom_type %in% c("density", "histogram")) {
          # --- Draw lines between group means for continuous geometries ---
          y_top <- max(ggplot2::ggplot_build(p)$data[[1]]$y, na.rm = TRUE)
          step <- 0.10 * y_top
          results_sig$y_pos <- y_top + seq_along(results_sig$pval) * step

          p <- p +
            ggplot2::geom_segment(
              data = results_sig,
              ggplot2::aes(
                x = mean1,
                xend = mean2,
                y = y_pos,
                yend = y_pos
              ),
              colour = "black",
              linewidth = 0.8,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_text(
              data = results_sig,
              ggplot2::aes(
                x = (mean1 + mean2) / 2,
                y = y_pos + (step / 3),
                label = p_label
              ),
              size = 3.2,
              inherit.aes = FALSE
            )
        }
      }
    }
  } else if (mode != "Subgroups" && !is.null(list(...)$compare) && list(...)$compare == TRUE) {
    message("Statistical comparison lines are only available in mode = 'Subgroups'.")
  }

  return(p)
}
