#' Plot Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Computes and plots the probability of cost-effectiveness as a function of
#' willingness-to-pay (lambda) using bootstrap replicates from one or several
#' analyses. The structure of modes and faceting matches that of
#' \code{\link{plot.icers}}.
#'
#' @param data A `cea_results`, `cea_results_list`, or tibble from
#'   \code{\link{combine_icers_results}}.
#' @param mode Character. Visualization mode: one of
#'   `"overall"`, `"subgroups"`, or `"datasets_subgroups"`.
#' @param subgroups Optional character vector specifying which subgroup
#'   variables to include (for subgroup modes).
#' @param lambda_range Numeric vector of length 2 giving the range of lambda
#'   (default = c(0, 100000)).
#' @param steps Integer. Number of lambda points (default = 100).
#' @param facet Logical. If TRUE, facet the plot appropriately for the mode.
#' @param facet_by Character. For subgroup modes, determines faceting scheme:
#'   `"vars"` (facet by subgroup variable) or `"levels"` (facet by subgroup level).
#' @param palette Color palette from RColorBrewer (default = "Set2").
#'
#' @return A ggplot object representing the cost-effectiveness acceptability curves.
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_bw facet_wrap facet_grid
#'   scale_color_brewer labs
#' @importFrom dplyr filter group_by summarise
#' @export
plot.ceac <- function(data,
                      mode = "overall",
                      subgroups = NULL,
                      lambda_range = c(0, 100000),
                      steps = 100,
                      facet = FALSE,
                      facet_by = c("vars", "levels"),
                      palette = "Set2") {

  facet_by <- match.arg(facet_by)

  # ---- Handle input type ----
  if (inherits(data, "cea_results_list") || inherits(data, "cea_results")) {
    if (is.null(data$combined_replicates)) {
      stop("The object has no 'combined_replicates' element. Run icers_base() first.")
    }
    df <- data$combined_replicates
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop("Invalid input: must be a cea_results, cea_results_list, or combined tibble.")
  }

  required_cols <- c("Delta_Cost", "Delta_Effect", "dataset", "level", "subgroup")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  valid_modes <- c("overall", "subgroups", "datasets_subgroups")
  if (!mode %in% valid_modes) {
    stop("Invalid mode. Choose one of: ", paste(valid_modes, collapse = ", "))
  }

  # ---- Filter data ----
  if (mode %in% c("subgroups", "datasets_subgroups")) {
    if (is.null(subgroups)) stop("Argument 'subgroups' must be provided for this mode.")
    df <- dplyr::filter(df, level %in% subgroups)
  } else {
    df <- dplyr::filter(df, level == "Overall")
  }

  # ---- Compute CEAC ----
  lambda_seq <- seq(lambda_range[1], lambda_range[2], length.out = steps)

  compute_ceac <- function(df) {
    probs <- sapply(lambda_seq, function(l) {
      mean((l * df$Delta_Effect - df$Delta_Cost) > 0, na.rm = TRUE)
    })
    data.frame(lambda = lambda_seq, prob = probs)
  }

  # ---- Group structure for calculation ----
  group_vars <- c()
  if (mode == "overall") group_vars <- "dataset"
  if (mode == "subgroups") group_vars <- c("level", "subgroup")
  if (mode == "datasets_subgroups") group_vars <- c("dataset", "level", "subgroup")

  ceac_df <- df |>
    dplyr::group_by(dplyr::across(all_of(group_vars))) |>
    dplyr::group_modify(~ compute_ceac(.x)) |>
    dplyr::ungroup()

  # ---- Build plot ----
  p <- ggplot2::ggplot(ceac_df, ggplot2::aes(x = lambda, y = prob))

  if (mode == "overall") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = dataset)) +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = expression(lambda ~ "(Willingness-to-pay)"),
        y = "Probability cost-effective",
        color = "Dataset",
        title = "Cost-Effectiveness Acceptability Curves"
      )
    if (facet) p <- p + ggplot2::facet_wrap(~ dataset)
  }

  if (mode == "subgroups") {
    if (facet_by == "vars") {
      p <- ggplot2::ggplot(ceac_df, ggplot2::aes(x = lambda, y = prob, color = subgroup)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::facet_wrap(~ level) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          x = expression(lambda),
          y = "Probability cost-effective",
          color = "Level",
          title = "CEAC by subgroup variable"
        )
    } else {
      p <- ggplot2::ggplot(ceac_df, ggplot2::aes(x = lambda, y = prob, color = level)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::facet_wrap(~ subgroup) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          x = expression(lambda),
          y = "Probability cost-effective",
          color = "Variable",
          title = "CEAC by subgroup level"
        )
    }
  }

  if (mode == "datasets_subgroups") {
    if (facet_by == "vars") {
      p <- ggplot2::ggplot(ceac_df, ggplot2::aes(x = lambda, y = prob, color = subgroup)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::facet_grid(dataset ~ level) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          x = expression(lambda),
          y = "Probability cost-effective",
          color = "Level",
          title = "CEAC by dataset × subgroup variable"
        )
    } else {
      p <- ggplot2::ggplot(ceac_df, ggplot2::aes(x = lambda, y = prob, color = level)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::facet_grid(dataset ~ subgroup) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          x = expression(lambda),
          y = "Probability cost-effective",
          color = "Variable",
          title = "CEAC by dataset × subgroup level"
        )
    }
  }

  return(p)
}
