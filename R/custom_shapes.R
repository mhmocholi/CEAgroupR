#' Define Custom Shape and Linetype Palettes for CEAgroupR Plots
#'
#' Constructs a custom shape and/or linetype palette to be used across
#' graphical functions in the CEAgroupR package. This utility allows users
#' to explicitly control the set of point shapes and line types used when
#' grouping results by a given variable (e.g. dataset or subgroup).
#'
#' Custom shape palettes are particularly useful in complex visualizations
#' where default ggplot2 shapes or linetypes may be difficult to distinguish,
#' such as multi-dataset or multi-subgroup analyses.
#'
#' @param shapes Optional numeric vector specifying ggplot2 shape codes
#'   to be used for point-based geometries. If \code{NULL}, default shapes
#'   are used.
#'
#' @param linetypes Optional character or numeric vector specifying ggplot2
#'   line types to be used for line-based geometries (e.g. contours). If
#'   \code{NULL}, default linetypes are used.
#'
#' @details
#' The object returned by this function can be passed to graphical functions
#' such as \code{\link{plot.icers}}, \code{\link{plot.ceacs}} and
#' \code{\link{plot.evpis}} via the \code{shapes_palette} argument. Shape and
#' linetype values are recycled if the number of groups exceeds the length
#' of the supplied vectors.
#'
#' @return
#' An object of class \code{"cea_shapes"} containing custom shape and/or
#' linetype definitions for use in CEAgroupR plots.
#'
#' @examples
#' ## Define a custom set of shapes and linetypes
#' my_shapes <- custom_shapes(
#'   shapes    = c(16, 17, 15),
#'   linetypes = c("solid", "dashed", "dotted")
#' )
#'
#' ## Use custom shapes in an ICER plane
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
#' plot.icers(
#'   res,
#'   shape_by       = "dataset",
#'   shapes_palette = my_shapes
#' )
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
