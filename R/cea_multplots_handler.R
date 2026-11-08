
#' Create a Collection of CEA Plots
#'
#' Internal helper that assigns a common S3 class
#' to a named list of ggplot objects and stores
#' information about the plot type.
#'
#' @param plots A list of ggplot objects.
#' @param plot_type Character string describing the plot type.
#'
#' @return An object of class "cea_multplots".
#'
#' @keywords internal
cea_multplots_handler <- function(
    plots,
    plot_type = "CEA plots"
) {

  if (!is.list(plots) ||
      !all(vapply(plots, inherits, logical(1), "ggplot"))) {
    stop("`plots` must be a list of ggplot objects.")
  }

  if (!is.character(plot_type) ||
      length(plot_type) != 1L ||
      is.na(plot_type) ||
      !nzchar(trimws(plot_type))) {
    stop("`plot_type` must be a non-empty character string.")
  }

  class(plots) <- c("cea_multplots", "list")

  attr(plots, "plot_type") <- plot_type

  return(plots)
}


#' Print a Collection of CEA Plots
#'
#' Displays ggplot objects sequentially in interactive sessions.
#' Users can press ENTER to advance or type q to stop.
#' In non-interactive sessions, all plots are printed without pauses.
#'
#' @param x An object of class "cea_multplots".
#' @param ... Additional arguments (currently unused).
#'
#' @return The original object, invisibly.
#'
#' @export
print.cea_multplots <- function(x, ...) {

  n <- length(x)

  if (n == 0L) {
    message("CEAgroupR | No plots to display.")
    return(invisible(x))
  }

  plot_names <- names(x)

  if (is.null(plot_names)) {
    plot_names <- paste0("Plot_", seq_len(n))
  }

  # Retrieve plot type metadata
  plot_type <- attr(x, "plot_type")

  if (is.null(plot_type)) {
    plot_type <- "CEA plots"
  }

  interactive_mode <- interactive()

  cat("\nCEAgroupR | Multiple plots viewer\n")
  cat("--------------------------------\n")
  cat("Plot type:", plot_type, "\n")
  cat("Total plots:", n, "\n\n")

  for (i in seq_len(n)) {

    cat(
      "Plot", i, "of", n,
      "|", plot_names[i], "\n"
    )

    print(x[[i]])

    if (interactive_mode && i < n) {

      response <- readline(
        prompt = "Press ENTER for next plot (q to quit): "
      )

      if (tolower(trimws(response)) == "q") {
        cat("\nPlot viewer closed.\n")
        break
      }
    }
  }

  cat("\nTip: Access individual plots using:\n")
  cat('  your_object[["', plot_names[1], '"]]\n', sep = "")
  cat("  (Replace 'your_object' with your R object name.)\n")

  invisible(x)
}
