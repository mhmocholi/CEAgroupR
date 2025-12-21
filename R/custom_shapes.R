#' Construct a Custom Shape and Linetype Specification
#'
#' Creates a validated object defining custom shape and/or linetype values
#' to be used by the CEAgroupR graphical engine. This constructor does not
#' perform any aesthetic mapping or recycling; it only validates inputs and
#' stores them in a structured object for later use.
#'
#' The resulting object is intended to be passed to graphical functions
#' (e.g., \code{plot.ceacs}, \code{plot.icers}) via a dedicated argument and
#' resolved internally by the plotting engine.
#'
#' @param shapes Optional numeric vector of ggplot2 shape codes
#'   (e.g., \code{c(16, 17, 15)}).
#' @param linetypes Optional numeric or character vector of ggplot2 linetype
#'   specifications (e.g., \code{c("solid", "dashed")} or \code{c(1, 2)}).
#'
#' @return An object of class \code{"cea_shapes"} containing the validated
#'   shape and linetype specifications.
#'
#' @export
custom_shapes <- function(
    shapes    = NULL,
    linetypes = NULL
) {

  # ------------------------------------------------------------
  # Input validation
  # ------------------------------------------------------------
  if (is.null(shapes) && is.null(linetypes)) {
    stop("At least one of 'shapes' or 'linetypes' must be provided.")
  }

  if (!is.null(shapes)) {
    if (!is.numeric(shapes) || any(!is.finite(shapes))) {
      stop("'shapes' must be a numeric vector of valid ggplot2 shape codes.")
    }
  }

  if (!is.null(linetypes)) {
    if (!(is.numeric(linetypes) || is.character(linetypes))) {
      stop("'linetypes' must be a numeric or character vector.")
    }
  }

  # ------------------------------------------------------------
  # Construct object
  # ------------------------------------------------------------
  out <- list(
    shapes    = shapes,
    linetypes = linetypes
  )

  class(out) <- "cea_shapes"
  out
}
