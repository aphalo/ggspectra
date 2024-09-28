#' Calculate colours from wavelength.
#'
#' \code{stat_color} computes color definitions according to human vision.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param x.colour.transform function Applied to \code{x} values before computing
#'   matching colours.
#'
#' @return The original data frame with a variable with color definitions added.
#'
#' @section Computed variable:
#' \describe{
#'   \item{wl.color}{color corresponding to x-value giving wavelength in
#'   nanometres.}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{color}{..wl.color..}
#'   \item{fill}{..wl.color..}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity}
#' }
#'
#' @seealso \code{\link[photobiology]{color_of}}, which is used internally.
#'
#' @examples
#'
#' ggplot(sun.spct) + geom_line() +
#'   stat_color() + scale_color_identity()
#'
#' @export
#' @family stats functions
#'
stat_color <- function(mapping = NULL,
                       data = NULL,
                       geom = "point",
                       position = "identity",
                       ...,
                       chroma.type = "CMF",
                       x.colour.transform = I,
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = TRUE) {
  if (!is.function(x.colour.transform)) {
    stop("'transform' arguments must be function defintions")
  }

  ggplot2::layer(
    stat = StatColor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(chroma.type = chroma.type,
                  x.colour.transform = x.colour.transform,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatColor <-
  ggplot2::ggproto(
    "StatColor",
    ggplot2::Stat,
    compute_group = function(data,
                             scales,
                             chroma.type,
                             x.colour.transform) {
      data[["wl.color"]] <-
        photobiology::fast_color_of_wl(colour-transform(data[["x"]]),
                                       chroma.type = chroma.type)
      data
    },
    default_aes = ggplot2::aes(color = after_stat(wl.color),
                               fill = after_stat(wl.color)),
    required_aes = c("x", "y")
  )
