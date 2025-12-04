#' Summarize Descriptive Statistics for Cost-Effectiveness Analyses
#'
#' Produces descriptive statistics for each strategy comparison within one or
#' several datasets. The function computes mean and standard deviation of cost
#' and effectiveness, incremental values (delta cost, delta effect), the
#' Incremental Cost-Effectiveness Ratio (ICER), and Net Monetary Benefit (NMB)
#' for one reference strategy contrasted with alternative strategies.
#'
#' Subgroup variables, when provided, are processed independently. Subgroup
#' levels are automatically normalized as factors:
#' \itemize{
#'   \item Numeric or integer levels are converted to ordered factors.
#'   \item Character levels preserve their observed order as unordered factors.
#' }
#'
#' When applied to a list of datasets, summaries are computed for each
#' dataset and aggregated in a single tidy tibble.
#'
#' @param data A data frame or a named list of data frames containing cost and
#'   effectiveness variables.
#' @param group Character string specifying the strategy variable.
#' @param cost Character string specifying the cost variable.
#' @param effect Character string specifying the effectiveness (e.g., QALYs)
#'   variable.
#' @param subgroup_vars Optional character vector with subgroup variable names.
#'   Each subgroup is summarized independently.
#' @param lambda Numeric vector of willingness-to-pay thresholds used to compute
#'   Net Monetary Benefit (NMB).
#' @param ref_group Character string identifying the reference strategy.
#' @param alt_groups Optional character vector defining the alternative
#'   strategies. If \code{NULL}, all non-reference strategies observed in the
#'   dataset are used.
#'
#' @return
#' A tibble containing descriptive summaries for each dataset, subgroup and
#' strategy comparison. The output includes:
#' \itemize{
#'   \item \code{dataset}: Dataset identifier.
#'   \item \code{subgroup_var}: Subgroup variable ("Overall" if none).
#'   \item \code{subgroup_level}: Subgroup level as a factor.
#'   \item \code{comparison}: Alternative strategy compared with the reference.
#'   \item \code{n_ref}, \code{n_alt}: Sample sizes per strategy.
#'   \item \code{mean_cost_ref}, \code{mean_cost_alt}: Mean total cost.
#'   \item \code{sd_cost_ref}, \code{sd_cost_alt}: Standard deviations of cost.
#'   \item \code{mean_eff_ref}, \code{mean_eff_alt}: Mean effectiveness.
#'   \item \code{sd_eff_ref}, \code{sd_eff_alt}: Standard deviations of effectiveness.
#'   \item \code{delta_cost}, \code{delta_effect}: Incremental values.
#'   \item \code{ICER}: Incremental Cost-Effectiveness Ratio.
#'   \item \code{NMB_*}: Net Monetary Benefit for each supplied WTP threshold.
#' }
#'
#' @importFrom stats sd
#' @export

summarize_cea_data <- function(data, group, cost, effect,
                               subgroup_vars = NULL, lambda = 25000,
                               ref_group = NULL, alt_groups = NULL) {

  # -------------------------------------------------------------------
  # 1. Helper: compute summary for one comparison
  # -------------------------------------------------------------------
  compute_summary <- function(df_sub, dataset, subgroup_var, subgroup_level,
                              ref_group, alt_group) {

    # ---- Normalize subgroup_level to factor ----
    if (is.numeric(subgroup_level) || is.integer(subgroup_level)) {

      subgroup_level <- factor(
        subgroup_level,
        levels  = sort(unique(subgroup_level)),
        ordered = TRUE
      )

    } else {

      subgroup_level <- factor(
        as.character(subgroup_level),
        levels = unique(as.character(subgroup_level)),
        ordered = FALSE
      )
    }

    # ---- Standard comparison logic ----
    df_sub[[group]] <- as.factor(df_sub[[group]])
    df_sub <- df_sub[df_sub[[group]] %in% c(ref_group, alt_group), , drop = FALSE]
    df_sub[[group]] <- factor(df_sub[[group]], levels = c(ref_group, alt_group))

    if (length(unique(df_sub[[group]])) < 2)
      return(NULL)

    n_ref <- sum(df_sub[[group]] == ref_group)
    n_alt <- sum(df_sub[[group]] == alt_group)
    if (n_alt == 0) return(NULL)

    m_cost <- tapply(df_sub[[cost]],   df_sub[[group]], mean, na.rm = TRUE)
    sd_cost <- tapply(df_sub[[cost]],  df_sub[[group]], sd,   na.rm = TRUE)
    m_eff  <- tapply(df_sub[[effect]], df_sub[[group]], mean, na.rm = TRUE)
    sd_eff <- tapply(df_sub[[effect]], df_sub[[group]], sd,   na.rm = TRUE)

    delta_cost <- m_cost[2] - m_cost[1]
    delta_eff  <- m_eff[2]  - m_eff[1]
    icer <- ifelse(delta_eff == 0, NA, delta_cost / delta_eff)

    nmb_vals <- sapply(lambda, function(l)
      l * delta_eff - delta_cost
    )
    names(nmb_vals) <- paste0("NMB_", lambda)

    tibble::tibble(
      dataset        = dataset,
      subgroup_var   = subgroup_var,
      subgroup_level = subgroup_level,
      comparison     = alt_group,
      n_ref = n_ref,
      n_alt = n_alt,
      mean_cost_ref = as.numeric(m_cost[1]),
      sd_cost_ref   = as.numeric(sd_cost[1]),
      mean_cost_alt = as.numeric(m_cost[2]),
      sd_cost_alt   = as.numeric(sd_cost[2]),
      mean_eff_ref  = as.numeric(m_eff[1]),
      sd_eff_ref    = as.numeric(sd_eff[1]),
      mean_eff_alt  = as.numeric(m_eff[2]),
      sd_eff_alt    = as.numeric(sd_eff[2]),
      delta_cost    = as.numeric(delta_cost),
      delta_effect  = as.numeric(delta_eff),
      ICER          = as.numeric(icer),
      !!!as.list(nmb_vals)
    )
  }

  # -------------------------------------------------------------------
  # 2. Helper: summarize a dataset (Overall + Subgroups)
  # -------------------------------------------------------------------
  summarize_one <- function(df, dataset_name) {

    df[[group]] <- as.factor(df[[group]])
    levels_g <- levels(df[[group]])

    if (is.null(ref_group)) ref_group <- levels_g[1]
    if (!ref_group %in% levels_g)
      stop("Reference group not found in dataset ", dataset_name)

    alts <- if (is.null(alt_groups)) setdiff(levels_g, ref_group)
    else intersect(levels_g, alt_groups)

    # ---- Overall summary ----
    overall <- dplyr::bind_rows(lapply(alts, function(alt)
      compute_summary(df, dataset_name,
                      subgroup_var = "Overall",
                      subgroup_level = "Overall",
                      ref_group, alt)
    ))

    # ---- Subgroup summaries ----
    subgroup_part <- NULL

    if (!is.null(subgroup_vars)) {

      subgroup_part <- dplyr::bind_rows(lapply(subgroup_vars, function(v) {

        lvls <- unique(df[[v]])

        dplyr::bind_rows(lapply(lvls, function(lvl) {

          # Normalize subgroup level to factor
          subgroup_level_factor <-
            if (is.numeric(lvl) || is.integer(lvl)) {
              factor(lvl,
                     levels  = sort(lvls),
                     ordered = TRUE)
            } else {
              factor(as.character(lvl),
                     levels = unique(as.character(lvls)))
            }

          dplyr::bind_rows(lapply(alts, function(alt)

            compute_summary(
              df[df[[v]] == lvl, , drop = FALSE],
              dataset_name,
              subgroup_var   = v,
              subgroup_level = subgroup_level_factor,
              ref_group,
              alt
            )
          ))
        }))
      }))
    }

    dplyr::bind_rows(overall, subgroup_part)
  }

  # -------------------------------------------------------------------
  # 3. Single dataset or list
  # -------------------------------------------------------------------
  if (is.data.frame(data)) {

    summarize_one(data, "single_dataset")

  } else {

    dplyr::bind_rows(lapply(names(data), function(dn)
      summarize_one(data[[dn]], dn)
    ))
  }
}
