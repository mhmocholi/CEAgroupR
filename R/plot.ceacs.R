#' Plot Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Produces Cost-Effectiveness Acceptability Curves (CEACs) derived from bootstrap
#' replicates produced by compute_icers(). CEACs represent, for each value of the
#' willingness-to-pay threshold (lambda), the probability that an alternative
#' strategy is cost-effective. The function supports multiple strategies,
#' datasets, and subgroup structures, and follows the unified graphical
#' architecture used in plot.icers().
#'
#' @inheritParams plot.icers
#' @param lambda_max Maximum lambda displayed on the x-axis.
#' @param lambda_steps Number of lambda values used to construct CEACs.
#' @param show_acceptabilities Logical; draw CEAC curves.
#' @param show_frontier Logical; draw the acceptability frontier.
#' @param accept_limit Numeric; acceptability threshold (horizontal reference).
#' @param show_accept_limit Logical; draw accept_limit reference line.
#' @param show_lambdas Logical; draw vertical reference lines.
#' @param lambdas Explicit lambda reference values.
#' @param return_data Logical; return CEAC table instead of plotting.
#'
#' @return A ggplot object or a list of CEAC tables.
#' @export
plot.ceacs <- function(data,
                       color_by = NULL,
                       shape_by = NULL,
                       facet_by = NULL,
                       mode = "Overall",
                       filter_expr = NULL,
                       facet_scales = "fixed",
                       lambda_max = NULL,
                       lambda_steps = 100,
                       show_acceptabilities = TRUE,
                       show_frontier = FALSE,
                       accept_limit = 0.95,
                       show_accept_limit = FALSE,
                       show_lambdas = FALSE,
                       lambdas = NULL,
                       palette = NULL,
                       return_data = FALSE,
                       ...) {

  `%||%` <- function(a,b) if (!is.null(a)) a else b

  # ========================================================================
  # 1. Extract data and ensure structure
  # ========================================================================
  if (inherits(data, "cea_results_list")) {

    if (is.null(data$combined_replicates))
      stop("'combined_replicates' not found. Run compute_icers().")

    df <- data$combined_replicates
    default_lambdas <- data$settings$lambda

  } else if (inherits(data, "data.frame")) {

    df <- data
    default_lambdas <- NULL

  } else stop("Input must be a cea_results_list or data frame.")

  if (!"group_uid" %in% names(df)) {
    df$group_uid <- with(df,
                         paste(dataset, subgroup_var, subgroup_level, comparison, sep = "_")
    )
  }

  # ========================================================================
  # 2. Filtering
  # ========================================================================
  if (!is.null(filter_expr)) {
    parsed <- tryCatch(
      rlang::parse_expr(filter_expr),
      error = function(e) stop("Invalid filter_expr: ", e$message)
    )
    df <- tryCatch(
      dplyr::filter(df, !!parsed),
      error = function(e) stop("Filtering failed: ", e$message)
    )
  }

  # ========================================================================
  # 3. Mode filtering
  # ========================================================================
  mode <- match.arg(mode, c("Overall","Subgroups"))

  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), ]
  } else {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), ]
  }

  # ========================================================================
  # 4. Determine lambda range
  # ========================================================================
  if (is.null(lambda_max)) {
    ratio <- with(df, Delta_Cost / Delta_Effect)
    ratio <- ratio[is.finite(ratio)]
    lambda_max <- suppressWarnings(
      max(0, quantile(abs(ratio), 0.9, na.rm = TRUE))
    )
    if (!is.finite(lambda_max) || lambda_max <= 0)
      lambda_max <- 100000
  }

  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  if (is.null(lambdas))
    lambdas <- default_lambdas

  # ========================================================================
  # 5. Compute CEAC curves
  # ========================================================================
  compute_ceac <- function(sub_df) {
    tibble::tibble(
      lambda = lambda_seq,
      prob = sapply(
        lambda_seq,
        function(l) mean(sub_df$Delta_Cost < l * sub_df$Delta_Effect, na.rm = TRUE)
      )
    )
  }

  ceac_df <- df %>%
    dplyr::group_by(group_uid) %>%
    dplyr::group_modify(~ compute_ceac(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      df %>%
        dplyr::select(group_uid, dataset, subgroup_var,
                      subgroup_level, comparison) %>%
        dplyr::distinct(),
      by = "group_uid"
    )

  # ========================================================================
  # 6. Frontier calculation
  # ========================================================================
  frontier_df <- ceac_df %>%
    dplyr::group_by(dataset, subgroup_var, subgroup_level, lambda) %>%
    dplyr::summarise(prob = max(prob, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(prob = pmin(prob + 0.0025, 1))

  if (return_data)
    return(list(ceac = ceac_df, frontier = frontier_df))

  # ========================================================================
  # 7. Resolve facet defaults
  # ========================================================================
  if (!is.null(facet_by) && !(facet_by %in% c("","none",NA))) {
    facet_input <- facet_by
  } else if (mode == "Subgroups") {
    facet_input <- "subgroup"
  } else {
    facet_input <- NULL
  }

  # ========================================================================
  # 8. Build unified base layout
  # ========================================================================
  base <- ce_plot_base(
    data         = ceac_df,
    color_by     = color_by,
    shape_by     = shape_by,
    facet_by     = facet_input,
    facet_scales = facet_scales,
    palette      = palette,
    auto_layout  = TRUE
  )

  p <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  args <- list(...)
  alpha_val <- args$alpha %||% 1
  lw        <- args$linewidth %||% 1

  # ========================================================================
  # 9. Inject aesthetics into global mapping (critical fix)
  # ========================================================================
  if (!is.null(color_var))
    p$mapping$colour <- rlang::sym(color_var)

  if (!is.null(shape_var))
    p$mapping$linetype <- rlang::sym(shape_var)

  # ========================================================================
  # 10. Activate scales for aesthetics
  # ========================================================================
  if (!is.null(color_var)) {
    p <- p + ggplot2::scale_colour_manual(values = base$palette_values)
  }

  if (!is.null(shape_var)) {
    p <- p + ggplot2::scale_linetype_discrete()
  }

  # Auto-hide legends
  if (!is.null(color_var) &&
      dplyr::n_distinct(ceac_df[[color_var]]) == 1) {
    p <- p + ggplot2::guides(colour = "none")
  }

  if (!is.null(shape_var) &&
      dplyr::n_distinct(ceac_df[[shape_var]]) == 1) {
    p <- p + ggplot2::guides(linetype = "none")
  }

  # ========================================================================
  # 11. Acceptability frontier
  # ========================================================================
  if (show_frontier) {
    datasets <- unique(frontier_df$dataset)
    linetypes <- setNames(
      c("solid","dashed","dotdash","dotted")[seq_along(datasets)],
      datasets
    )
    for (ds in datasets) {
      df_sub <- frontier_df[frontier_df$dataset == ds,]
      p <- p +
        ggplot2::geom_line(
          data = df_sub,
          mapping = ggplot2::aes(
            x = lambda,
            y = prob,
            group = interaction(dataset, subgroup_var, subgroup_level)
          ),
          colour      = "black",
          linewidth   = 1.4,
          alpha       = 0.7,
          linetype    = linetypes[[ds]],
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  # ========================================================================
  # 12. CEAC curves (use global aesthetics)
  # ========================================================================
  if (show_acceptabilities) {
    p <- p +
      ggplot2::geom_line(
        data   = ceac_df,
        mapping = ggplot2::aes(
          x = lambda,
          y = prob,
          group = group_uid
        ),
        linewidth   = lw,
        alpha       = alpha_val,
        show.legend = TRUE
      )
  }

  # ========================================================================
  # 13. Acceptability limit
  # ========================================================================
  if (show_accept_limit) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = accept_limit,
        colour     = "grey40",
        linetype   = "dashed",
        linewidth  = 0.6
      )
  }

  # ========================================================================
  # 14. Lambda reference lines
  # ========================================================================
  if (show_lambdas && !is.null(lambdas)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = lambdas,
        colour     = "grey50",
        linetype   = "dashed",
        linewidth  = 0.6
      )
  }

  # ========================================================================
  # 15. Axis formatting & labels
  # ========================================================================
  x_breaks <- sort(unique(c(pretty(lambda_seq,5), lambdas)))
  y_breaks <- sort(unique(c(0,0.25,0.50,0.75,accept_limit,1)))

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = scales::label_number()
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0,1),
      breaks = y_breaks,
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::labs(
      title = "Cost-Effectiveness Acceptability Curve (CEAC)",
      x     = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y     = "Probability cost-effective"
    )

  return(p)
}
