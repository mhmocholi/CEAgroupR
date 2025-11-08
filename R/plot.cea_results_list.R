#' Plot method for cea_results_list objects
#'
#' Generic dispatcher for visualizations of cost-effectiveness analyses.
#' Redirects the call to the appropriate specific plotting function
#' (ICER, CEAC, EVPI, or Marginals) depending on the arguments provided.
#'
#' @param x An object of class \code{cea_results_list}.
#' @param type Character. One of "icers", "ceacs", "evpis", or "marginals".
#'   If not specified, defaults to "icers".
#' @param ... Additional arguments passed to the corresponding plotting method.
#'
#' @return A ggplot object.
#' @export
plot.cea_results_list <- function(x, type = "icers", ...) {
  type <- match.arg(type, c("icers", "ceacs", "evpis", "marginals"))

  switch(type,
         "icers" = plot.icers(x, ...),
         "ceacs" = plot.ceacs(x, ...),
         "evpis" = plot.evpis(x, ...),
         "marginals" = plot.marginals(x, ...)
  )
}
