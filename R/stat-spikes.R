#' Find spikes
#'
#' \code{stat_spikes} finds at which x positions spikes are located. Spikes
#' can be either upwards or downwards from the baseline.
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
#' @param label.fmt character  string giving a format definition for converting
#'   values into character strings by means of function \code{\link{sprintf}}.
#' @param x.label.fmt character  string giving a format definition for converting
#'   $x$-values into character strings by means of function \code{\link{sprintf}}.
#' @param y.label.fmt character  string giving a format definition for converting
#'   $y$-values into character strings by means of function \code{\link{sprintf}}.
#'
#' @return A data frame with one row for each peak (or valley) found in the
#'   data.
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{x-value at the peak (or valley) as numeric}
#'   \item{y}{y-value at the peak (or valley) as numeric}
#'   \item{x.label}{x-value at the peak (or valley) formatted as character}
#'   \item{y.label}{y-value at the peak (or valley) formatted as character}
#'   \item{wl.color}{color definition calculated by assuming that x-values are
#'   wavelengths expressed in nanometres.}
#'   \item{BW.color}{color definition that either "black" or "white", to ensure
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
#'   default aesthetics set by this stat allows its direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The formatting of the labels
#'   returned can be controlled by the user.
#'
#' @note This stat works nicely together with geoms
#'   \code{geom_text_repel} and
#'   \code{geom_label_repel} from package
#'   \code{\link[ggrepel]{ggrepel}} to solve the problem of overlapping labels
#'   by displacing them. To discard overlapping labels use \code{check_overlap =
#'   TRUE} as argument to \code{geom_text}.
#'  By default the labels are character values suitable to be plotted as is, but
#'  with a suitable \code{label.fmt} labels suitable for parsing by the geoms
#'  (e.g. into expressions containing greek letters or super or subscripts) can
#'  be also easily obtained.
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
                        label.fmt = "%.3g",
                        x.label.fmt = label.fmt,
                        y.label.fmt = label.fmt,
                        na.rm = FALSE,
                        show.legend = FALSE,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatSpikes, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(z.threshold = z.threshold,
                  max.spike.width = max.spike.width,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
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
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     force(z.threshold)
                     force(max.spike.width)
                     spikes.df <-
                       data[photobiology::find_spikes(data$y,
                                                      x.is.delta = FALSE,
                                                      z.threshold = z.threshold,
                                                      max.spike.width = max.spike.width),
                            , drop = FALSE]
                     dplyr::mutate(spikes.df,
                                   x.label = sprintf(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y),
                                   wl.color = photobiology::color_of(x, type = "CMF"),
                                   BW.color = black_or_white(photobiology::color_of(x, type = "CMF")))
                   },
                   default_aes = ggplot2::aes(label = stat(x.label),
                                              fill = stat(wl.color),
                                              xintercept = stat(x),
                                              yintercept = stat(y),
                                              hjust = 0.5,
                                              vjust = 0.5),
                   required_aes = c("x", "y")
  )

