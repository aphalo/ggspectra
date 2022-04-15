#' Calculate colours from wavelength.
#'
#' \code{stat_wl_strip} computes color definitions according to human vision.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points
#'    on this layer
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
#' @param na.rm	a logical value indicating whether NA values should be
#'   stripped before the computation proceeds.
#' @param w.band waveband object or a list of such objects or NULL.
#' @param length.out The number of steps to use to simulate a continuous
#'   range of colours when w.band == NULL.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#'
#' @return generic_spect object with new \code{x} values plus other computed
#'   variables described below.
#'
#' @section Computed variables:
#' \describe{
#'    \item{x}{(w.low + wl.high) / 2}
#'    \item{wl.low}{boundary of waveband}
#'    \item{wl.high}{boundary of waveband}
#'    \item{wl.color}{color corresponding to wavelength}
#'    \item{wb.color}{color corresponding to waveband}
#'    \item{wb.name}{label of w.band}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{x}{..x..}
#'   \item{label}{as.character(..wb.f..)}
#'   \item{xmin}{..wl.low..}
#'   \item{xmax}{..wl.high..}
#'   \item{fill}{..wb.color..}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#' }
#'
#' @note This stat uses a panel function and ignores grouping as it is meant to
#'   be used for annotations.
#'
#' @seealso \code{\link[photobiology]{color_of}}, which is used internally.
#'
#' @examples
#'
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) + geom_line() +
#'   stat_wl_strip(ymax = -0.02, ymin = -0.04) +
#'   scale_fill_identity()
#'
#' # on some graphic devices the output may show spurious vertical lines
#' ggplot(sun.spct) + wl_guide(alpha = 0.33, color = NA) + geom_line()
#'
#' @export
#' @family stats functions
#'
stat_wl_strip <- function(mapping = NULL,
                          data = NULL,
                          geom = "rect",
                          w.band = NULL,
                          length.out = 150,
                          chroma.type = "CMF",
                          position = "identity",
                          na.rm = TRUE,
                          show.legend = FALSE,
                          inherit.aes = TRUE,
                          ...) {
  ggplot2::layer(
    stat = StatColorGuide, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(chroma.type = chroma.type,
                  w.band = w.band,
                  length.out = length.out,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatColorGuide <-
  ggplot2::ggproto("StatColorGuide", ggplot2::Stat,
                   compute_panel = function(data,
                                            scales,
                                            w.band,
                                            length.out,
                                            chroma.type) {
                     if (length(w.band) == 0) {
                       w.band <- split_bands(range(data[["x"]]), length.out = length.out)
                     } else {
                       w.band <- trim_waveband(w.band = w.band, range = data[["x"]], trim = TRUE)
                     }

                     z <- fast_wb2rect_spct(w.band = w.band, chroma.type = chroma.type)
                     names(z)[1] <- "x"
                     z
                    },
                   default_aes = ggplot2::aes(xmin = after_stat(wl.low),
                                              xmax = after_stat(wl.high),
                                              label = as.character(after_stat(wb.f)),
                                              fill = after_stat(wb.color)),
                   required_aes = c("x")
  )

#' @rdname stat_wl_strip
#' @param ymin,ymax numeric used as aesthetics for plotting the guide.
#'
#' @export
#'
wl_guide <- function(mapping = NULL,
                     data = NULL,
                     chroma.type = "CMF",
                     w.band = NULL,
                     length.out = 150,
                     ymin = -Inf,
                     ymax = Inf,
                     position = "identity",
                     na.rm = FALSE,
                     show.legend = FALSE,
                     inherit.aes = TRUE,
                     ...) {
  list(stat_wl_strip(mapping = mapping,
                     data = data,
                     geom = "rect",
                     w.band = w.band,
                     chroma.type = chroma.type,
                     length.out = length.out,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes,
                     ymin = ymin,
                     ymax = ymax,
                     ...),
       scale_fill_identity()
  )
}
