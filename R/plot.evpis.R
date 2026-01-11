#' Plot Expected Value of Perfect Information (EVPI)
#'
#' Produces Expected Value of Perfect Information (EVPI) curves across a range
#' of willingness-to-pay (WTP) thresholds using bootstrap replicates generated
#' by \code{\link{compute_icers}}. The EVPI quantifies the expected gain from
#' eliminating decision uncertainty, and represents the difference between
#' the expected net benefit under perfect information and the expected net
#' benefit under current uncertainty.
#'
#' EVPI curves provide a value-of-information perspective on decision-making
#' and are commonly used to assess the potential benefit of additional
#' research. The function supports multi-strategy and multi-dataset analyses,
#' explicit subgroup stratification, and full user control over aesthetic
#' mappings.
#'
#' @inheritParams plot.icers
#'
#' @param lambda_max Optional numeric value specifying the maximum
#'   willingness-to-pay threshold displayed on the x-axis. If \code{NULL},
#'   a heuristic based on the distribution of incremental cost-effectiveness
#'   ratios is used to provide a robust default.
#'
#' @param lambda_steps Integer specifying the number of WTP evaluation points
#'   used to compute EVPI values.
#'
#' @param return_data Logical; if \code{TRUE}, returns a tidy data frame
#'   containing EVPI values instead of a \code{ggplot} object.
#'
#' @details
#' This function relies on the internal plotting engine
#' \code{\link{ce_plot_base}} to ensure consistent aesthetics and behaviour
#' across all CEAgroupR visualizations. It is typically used alongside
#' \code{\link{plot.icers}} and \code{\link{plot.ceacs}} to provide a
#' complementary value-of-information perspective on uncertainty in
#' cost-effectiveness analyses.
#'
#' @return
#' A \code{ggplot} object representing the EVPI curve, or a tidy data frame
#' containing EVPI values when \code{return_data = TRUE}.
#'
#' @examples
#' ## Example 1: EVPI curve for a single dataset
#' res <- compute_icers(
#'   data      = cua_base,
#'   group     = "group",
#'   cost      = "cost_total",
#'   effect    = "effect",
#'   ref_group = "g0",
#'   R         = 50,
#'   seed      = 123
#' )
#'
#' plot.evpis(res)
#'
#' ## Example 2: EVPI with subgroup stratification
#' plot.evpis(
#'   res,
#'   mode     = "Subgroups",
#'   facet_by = "subgroup"
#' )
#' @export plot.evpis
#' @export
plot.evpis <- function(
    data,
    color_by     = NULL,
    shape_by     = NULL,
    facet_by     = NULL,
    mode         = "Overall",
    filter_expr  = NULL,
    facet_scales = "fixed",
    lambda_max   = NULL,
    lambda_steps = 100,
    palette      = "Dark2",
    shapes_palette = NULL,
    return_data  = FALSE,
    ...
) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ============================================================
  # 1. Input extraction
  # ============================================================
  if (inherits(data, "cea_results_list")) {

    if (is.null(data$combined_replicates))
      stop("'combined_replicates' missing. Run compute_icers() first.")

    df <- data$combined_replicates

  } else if (inherits(data, "data.frame")) {

    df <- data

  } else {
    stop("Input must be a cea_results_list or a data frame.")
  }

  if (!"group_uid" %in% names(df)) {
    df$group_uid <- with(
      df, paste(dataset, subgroup_var, subgroup_level, comparison, sep = "_")
    )
  }

  # ============================================================
  # 2. Filtering
  # ============================================================
  if (!is.null(filter_expr)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(filter_expr))
  }

  # ============================================================
  # 3. Mode filtering
  # ============================================================
  mode <- match.arg(mode, c("Overall", "Subgroups"))

  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), ]
  } else {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), ]
  }

  # ============================================================
  # 4. Lambda grid
  # ============================================================
  if (is.null(lambda_max)) {

    ratio <- df$Delta_Cost / df$Delta_Effect
    ratio <- ratio[is.finite(ratio)]

    lambda_max <- suppressWarnings(
      max(0, stats::quantile(abs(ratio), 0.9, na.rm = TRUE))
    )

    if (!is.finite(lambda_max) || lambda_max <= 0)
      lambda_max <- 100000
  }

  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  # ============================================================
  # 5. EVPI computation
  # ============================================================
  compute_evpi_group <- function(sub_df) {

    dplyr::bind_rows(lapply(lambda_seq, function(l) {

      nmb <- l * sub_df$Delta_Effect - sub_df$Delta_Cost

      perfect <- mean(pmax(0, nmb), na.rm = TRUE)
      current <- max(0, mean(nmb, na.rm = TRUE))

      tibble::tibble(
        lambda = l,
        EVPI   = perfect - current
      )
    }))
  }

  evpi_df <- df %>%
    dplyr::group_by(group_uid) %>%
    dplyr::group_modify(~ compute_evpi_group(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      df %>% dplyr::distinct(
        group_uid, dataset, subgroup_var, subgroup_level, comparison
      ),
      by = "group_uid"
    )

  if (return_data)
    return(evpi_df)

  # ============================================================
  # 6. Default aesthetics (overridden by user)
  # ============================================================
  default_color <- "comparison"
  default_shape <- "dataset"
  default_facet <- if (mode == "Subgroups") "subgroup" else "none"

  resolved_color <- color_by %||% default_color
  resolved_shape <- shape_by %||% default_shape
  resolved_facet <- facet_by %||% default_facet

  if (identical(color_by, "none")) resolved_color <- NULL
  if (identical(shape_by,  "none")) resolved_shape <- NULL
  if (identical(facet_by,  "none")) resolved_facet <- NULL

  # ============================================================
  # 7. Base layout via ce_plot_base()
  # ============================================================
  base <- ce_plot_base(
    data           = evpi_df,
    color_by       = resolved_color,
    shape_by       = resolved_shape,
    facet_by       = resolved_facet,
    facet_scales   = facet_scales,
    palette        = palette,
    shapes_palette = shapes_palette
  )

  p <- base$plot
  color_var       <- base$color_var
  shape_var       <- base$shape_var
  palette_values  <- base$palette_values
  linetype_values <- base$linetype_values

  args      <- list(...)
  lw        <- args$linewidth %||% 1
  alpha_val <- args$alpha     %||% 1

  # ============================================================
  # 8. Scales BEFORE drawing
  # ============================================================
  if (!is.null(color_var))
    p <- p + ggplot2::scale_colour_manual(values = palette_values)

  if (!is.null(shape_var)) {
    if (!is.null(linetype_values)) {
      p <- p + ggplot2::scale_linetype_manual(values = linetype_values)
    } else {
      p <- p + ggplot2::scale_linetype_discrete()
    }
  }

  # Hide legends with only one level
  if (!is.null(color_var) &&
      dplyr::n_distinct(evpi_df[[color_var]]) == 1)
    p <- p + ggplot2::guides(colour = "none")

  if (!is.null(shape_var) &&
      dplyr::n_distinct(evpi_df[[shape_var]]) == 1)
    p <- p + ggplot2::guides(linetype = "none")

  # ============================================================
  # 9. Invisible layer to activate aesthetics
  # ============================================================
  aes_act <- ggplot2::aes(x = lambda, y = EVPI)

  if (!is.null(color_var))
    aes_act$colour <- rlang::sym(color_var)

  if (!is.null(shape_var))
    aes_act$linetype <- rlang::sym(shape_var)

  p <- p + ggplot2::geom_point(
    data        = evpi_df,
    mapping     = aes_act,
    alpha       = 0,
    inherit.aes = FALSE
  )

  # ============================================================
  # 10. EVPI curve layer
  # ============================================================
  aes_evpi <- ggplot2::aes(
    x     = lambda,
    y     = EVPI,
    group = group_uid
  )

  if (!is.null(color_var))
    aes_evpi$colour <- rlang::sym(color_var)

  if (!is.null(shape_var))
    aes_evpi$linetype <- rlang::sym(shape_var)

  p <- p +
    ggplot2::geom_line(
      data    = evpi_df,
      mapping = aes_evpi,
      linewidth = lw,
      alpha     = alpha_val
    )

  # ============================================================
  # 11. Axes and labels
  # ============================================================
  x_breaks <- pretty(lambda_seq, 5)

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = scales::label_number()
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = scales::label_number()
    ) +
    ggplot2::labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x     = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y     = "EVPI"
    )

  return(p)
}
