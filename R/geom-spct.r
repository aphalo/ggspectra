#' Ribbons and area plots.
#'
#' For each continuous x value, \code{geom_interval} displays a y interval.
#' \code{geom_area} is a special case of \code{geom_ribbon}, where the
#' minimum of the range is fixed to 0.
#'
#' An area plot is the continuous analog of a stacked bar chart (see
#' \code{\link{geom_bar}}), and can be used to show how composition of the
#' whole varies over the range of x.  Choosing the order in which different
#' components is stacked is very important, as it becomes increasing hard to
#' see the individual pattern as you move up the stack.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A data frame. If specified, overrides the default data frame defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If TRUE silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @section Aesthetics:
#' See \code{\link[ggplot2]{geom_ribbon}}
#'
#' @seealso
#'   \code{\link[ggplot2]{geom_bar}} for discrete intervals (bars),
#'   \code{\link[ggplot2]{geom_linerange}} for discrete intervals (lines),
#'   \code{\link[ggplot2]{geom_polygon}} for general polygons
#' @inheritParams geom_point
#' @export
#' @export
geom_spct <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSpct,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSpct <- ggproto("GeomSpct", GeomRibbon,
  default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    transform(data, ymin = 0, ymax = y)
  }
)

