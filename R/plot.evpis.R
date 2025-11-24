#' Plot Expected Value of Perfect Information (EVPI)
#'
#' Computes and plots the Expected Value of Perfect Information (EVPI) as a
#' function of willingness-to-pay (λ) using bootstrap replicates returned by
#' \code{\link{compute_icers}}. EVPI is defined as the expected gain from
#' eliminating all uncertainty in the decision problem.
#'
#' This function follows the unified graphical architecture used in
#' \code{plot.icers()} and \code{plot.ceacs()}:
#'   * Option C behaviour for aesthetic mappings (\code{color_by},
#'     \code{shape_by}, \code{facet_by}).
#'   * Full support for dynamic filtering using \code{filter_expr}.
#'   * Flexible palette handling (user-defined vectors, Brewer palettes, or
#'     base R palette).
#'   * Faceting rules consistent with subgroup structures, including the special
#'     \code{"subgroup"} grid.
#'   * EVPI curves use linetype when \code{shape_by} is specified.
#'   * λ values are shown using axis ticks rather than floating text labels.
#'
#' @param data A \code{cea_results_list} from \code{compute_icers()} or a tibble
#'   returned by \code{combine_icers_results()}.
#'
#' @param color_by,shape_by,facet_by Character variables for aesthetic mappings.
#'   Supported: \code{"dataset"}, \code{"comparison"}, \code{"subgroup_var"},
#'   \code{"subgroup_level"}, \code{"subgroup"}, or \code{"none"}.
#'   Missing values follow Option C.
#'
#' @param mode Character, either \code{"Overall"} or \code{"Subgroups"}.
#'
#' @param filter_expr Optional tidy-style filter expression applied to
#'   bootstrap replicates prior to computing EVPI.
#'
#' @param facet_scales Facet scaling behaviour.
#'
#' @param lambda_max Maximum λ on the x-axis. If \code{NULL}, a heuristic based
#'   on \eqn{|ΔCost/ΔEffect|} is used.
#'
#' @param lambda_steps Number of λ values used to compute EVPI.
#'
#' @param show_lambdas Logical; whether to draw vertical λ reference lines.
#'
#' @param lambdas Optional numeric λ values. If \code{NULL}, values used during
#'   \code{compute_icers()} are used when available.
#'
#' @param palette Colour palette: vector of colours, a Brewer palette name, or
#'   \code{NULL} for base R palette.
#'
#' @param return_data Logical; if \code{TRUE}, returns the EVPI table instead of
#'   a plot.
#'
#' @param ... Additional arguments passed to \code{geom_line()}.
#'
#' @return A \code{ggplot} object or a tibble of EVPI values.
#'
#' @export
plot.evpis <- function(data,
                       color_by = NULL,
                       shape_by = NULL,
                       facet_by = NULL,
                       mode = "Overall",
                       filter_expr = NULL,
                       facet_scales = "fixed",
                       lambda_max = NULL,
                       lambda_steps = 100,
                       show_lambdas = FALSE,
                       lambdas = NULL,
                       palette = NULL,
                       return_data = FALSE,
                       ...) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # -------------------------------------------------------------
  # 1. Extract input data
  # -------------------------------------------------------------
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
                         paste(dataset, subgroup_var, subgroup_level, comparison, sep = "_"))
  }

  # -------------------------------------------------------------
  # 2. Filtering
  # -------------------------------------------------------------
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

  # -------------------------------------------------------------
  # 3. Mode filtering
  # -------------------------------------------------------------
  mode <- match.arg(mode, c("Overall", "Subgroups"))

  if (mode == "Overall") {
    df <- df[df$subgroup_var == "Overall" | is.na(df$subgroup_var), ]
  } else {
    df <- df[df$subgroup_var != "Overall" & !is.na(df$subgroup_var), ]
  }

  # -------------------------------------------------------------
  # 4. Lambda range
  # -------------------------------------------------------------
  if (is.null(lambda_max)) {

    ratio <- with(df, Delta_Cost / Delta_Effect)
    ratio <- ratio[is.finite(ratio)]

    lambda_max <- max(0, quantile(abs(ratio), 0.9, na.rm = TRUE))

    if (!is.finite(lambda_max) || lambda_max <= 0)
      lambda_max <- 100000
  }

  lambda_seq <- seq(0, lambda_max, length.out = lambda_steps)

  if (is.null(lambdas))
    lambdas <- default_lambdas

  # -------------------------------------------------------------
  # 5. Compute EVPI
  # -------------------------------------------------------------
  compute_evpi <- function(sub_df) {

    results <- lapply(lambda_seq, function(l) {

      NMB     <- l * sub_df$Delta_Effect - sub_df$Delta_Cost
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
        dplyr::select(group_uid, dataset, subgroup_var, subgroup_level, comparison) %>%
        dplyr::distinct(),
      by = "group_uid"
    )

  if (return_data)
    return(evpi_df)

  # -------------------------------------------------------------
  # 6. Facet default (Option C)
  # -------------------------------------------------------------
  if (!is.null(facet_by) && !(facet_by %in% c("", NA, "none"))) {

    facet_input <- facet_by

  } else if (mode == "Overall") {

    facet_input <- NULL

  } else {

    facet_input <- "auto"  # → subgroup
  }

  # -------------------------------------------------------------
  # 7. Base layout
  # -------------------------------------------------------------
  base <- ce_plot_base(
    data         = evpi_df,
    color_by     = color_by,
    shape_by     = shape_by,
    facet_by     = facet_input,
    facet_scales = facet_scales,
    palette      = palette,
    auto_layout  = TRUE
  )

  p         <- base$plot
  color_var <- base$color_var
  shape_var <- base$shape_var

  args     <- list(...)
  alpha_val <- args$alpha %||% 1
  lw        <- args$linewidth %||% 1

  # -------------------------------------------------------------
  # 8. EVPI curves
  # -------------------------------------------------------------
  aes_evpi <- ggplot2::aes(x = lambda, y = EVPI, group = group_uid)

  if (!is.null(color_var))
    aes_evpi$colour <- rlang::sym(color_var)

  if (!is.null(shape_var))
    aes_evpi$linetype <- rlang::sym(shape_var)

  p <- p +
    ggplot2::geom_line(
      data = evpi_df,
      mapping = aes_evpi,
      linewidth = lw,
      alpha     = alpha_val
    )

  # -------------------------------------------------------------
  # 9. Lambda reference lines
  # -------------------------------------------------------------
  if (show_lambdas && !is.null(lambdas)) {

    p <- p +
      ggplot2::geom_vline(
        xintercept = lambdas,
        linetype   = "dashed",
        colour     = "grey50",
        linewidth  = 0.6
      )
  }

  # -------------------------------------------------------------
  # 10. Axis formatting
  # -------------------------------------------------------------
  x_breaks <- sort(unique(c(pretty(lambda_seq, 5), lambdas)))

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = scales::label_number()
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0,0),
      labels = scales::label_number()
    ) +
    ggplot2::labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x = expression(Willingness * "-to-" * Pay ~ (lambda)),
      y = "EVPI (Monetary units)"
    )

  return(p)
}
