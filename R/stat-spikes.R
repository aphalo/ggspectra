#' Find spikes
#'
#' \code{stat_spikes} finds at which \code{x} positions spikes are located.
#' Spikes can be either upwards or downwards from the baseline.
#' \strong{Axis flipping is currently not supported.}
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
#' @param z.threshold numeric Modified Z values larger than \code{z.threshold}
#'   are considered to be spikes.
#' @param max.spike.width integer Wider regions with high Z values are not detected as
#'   spikes.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param label.fmt,x.label.fmt,y.label.fmt character  strings giving a format
#'   definition for construction of character strings labels with function
#'   \code{\link{sprintf}} from \code{x} and/or \code{y} values.
#' @param x.label.transform,y.label.transform,x.colour.transform function Applied
#'   to \code{x} or \code{y} values when constructing the character labels or
#'   computing matching colours.
#'
#' @return A data frame of observations found in the data matching the criterion
#'   of being part of a spike. That is to say, the returned data frame not only
#'   includes the observation at the tip of the spike but also those on its
#'   shoulders.
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{x-value at the peak (or valley) as numeric}
#'   \item{y}{y-value at the peak (or valley) as numeric}
#'   \item{x.label}{x-value of observations in the spike formatted as character}
#'   \item{y.label}{y-value of observations in the spike formatted as character}
#'   \item{wl.color}{color definition calculated by assuming that x-values are
#'   wavelengths expressed in nanometres.}
#'   \item{BW.color}{color definition that is either "black" or "white", to ensure
#'   high contrast to \code{wl.color}.}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{label}{stat(x.label)}
#'   \item{xintercept}{stat(x)}
#'   \item{yintercept}{stat(y)}
#'   \item{fill}{stat(wl.color)}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity}
#' }
#'
#' @seealso \code{\link[photobiology]{find_spikes}}, which is used internally,
#'   for a description of the algorithm used.
#'
#' @details This stat uses \code{geom_point} by default as it is the geom most
#'   likely to work well in almost any situation without need of tweaking. The
#'   default aesthetics set by this stat allow its direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The formatting of the labels
#'   returned can be controlled by the user.
#'
#' @note This stat works nicely together with geoms \code{geom_text_repel} and
#'   \code{geom_label_repel} from package \code{\link[ggrepel]{ggrepel}} to
#'   solve the problem of overlapping labels
#'   by displacing them. To discard overlapping labels use \code{check_overlap =
#'   TRUE} as argument to \code{geom_text}.
#'
#'   By default the labels are character values suitable to be plotted as is,
#'   but with a suitable \code{label.fmt} argument labels suitable for parsing
#'   by the geoms (e.g., into expressions containing greek letters or super or
#'   subscripts) can be also easily obtained.
#'
#' @examples
#'
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#'
#' # two spurious(?) spikes
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_spikes(colour = "red", alpha = 0.3)
#'
#' # no spikes detected
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_spikes(colour = "red", alpha = 0.3,
#'               max.spike.width = 3,
#'               z.threshold = 12)
#'
#' # small noise spikes detected
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   stat_spikes(colour = "red", alpha = 0.3)
#'
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   stat_spikes(colour = "red", alpha = 0.3) +
#'   stat_spikes(geom = "text", colour = "red", check_overlap = TRUE,
#'              vjust = -0.5, label.fmt = "%3.0f nm")
#'
#' ggplot(white_led.raw_spct, aes(w.length, counts_2)) +
#'   geom_line() +
#'   stat_spikes(colour = "red", alpha = 0.3,
#'               max.spike.width = 3,
#'               z.threshold = 12)
#'
#' @export
#'
#' @family stats functions
#'
stat_spikes <- function(mapping = NULL,
                        data = NULL,
                        geom = "point",
                        position = "identity",
                        ...,
                        z.threshold = 9,
                        max.spike.width = 8,
                        chroma.type = "CMF",
                        label.fmt = "%.3g",
                        x.label.fmt = label.fmt,
                        y.label.fmt = label.fmt,
                        x.label.transform = function(x) {x},
                        y.label.transform = function(x) {x},
                        x.colour.transform = x.label.transform,
                        na.rm = FALSE,
                        show.legend = FALSE,
                        inherit.aes = TRUE) {
  if (!(is.function(x.label.transform) &&
        is.function(y.label.transform) &&
        is.function(x.colour.transform))) {
    stop("'transform' arguments must be function defintions")
  }

  ggplot2::layer(
    stat = StatSpikes, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(z.threshold = z.threshold,
                  max.spike.width = max.spike.width,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  x.label.transform = x.label.transform,
                  y.label.transform = y.label.transform,
                  x.colour.transform = x.colour.transform,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSpikes <-
  ggplot2::ggproto("StatSpikes", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            z.threshold,
                                            max.spike.width,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt,
                                            x.label.transform,
                                            y.label.transform,
                                            x.colour.transform) {

                     photobiology::check_wl_stepsize(data[["x"]])

                     force(z.threshold)
                     force(max.spike.width)

                     spikes.df <-
                       data[photobiology::find_spikes(data[["y"]],
                                                      x.is.delta = FALSE,
                                                      z.threshold = z.threshold,
                                                      max.spike.width = max.spike.width),
                            , drop = FALSE]
                     spikes.df[["x.label"]] <-
                       sprintf(x.label.fmt, x.label.transform(spikes.df[["x"]]))
                     spikes.df[["y.label"]] <-
                       sprintf(y.label.fmt, y.label.transform(spikes.df[["y"]]))
                     spikes.df[["wl.color"]] <-
                       photobiology::fast_color_of_wl(x.colour.transform(spikes.df[["x"]]),
                                                      chroma.type = chroma.type)
                     spikes.df[["BW.color"]] <-
                       black_or_white(spikes.df[["wl.color"]])
                     spikes.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y),
                                              hjust = 0.5,
                                              vjust = 0.5),
                   required_aes = c("x", "y")
  )

