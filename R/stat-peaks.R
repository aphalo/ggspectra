#' Find peaks and valleys.
#'
#' \code{stat_peaks} finds at which x positions the global y maximun or local
#' y maxima are located. \code{stat_valleys} finds at which x positions the
#' global y minimum or local y minima located. They both support filtering
#' of relevant peaks. \strong{Axis flipping is currently not supported.}
#'
#' @inheritParams photobiology::peaks
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
#' @param refine.wl logical Flag indicating if peak or valleys locations should
#'   be refined by fitting a function.
#' @param method character String with the name of a method used for peak
#'   fitting. Currently only spline interpolation is implemented.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param label.fmt,x.label.fmt,y.label.fmt character  strings giving a format
#'   definition for construction of character strings labels with function
#'   \code{\link{sprintf}} from \code{x} and/or \code{y} values.
#' @param x.label.transform,y.label.transform,x.colour.transform function Applied
#'   to \code{x} or \code{y} values when constructing the character labels or
#'   computing matching colours.
#'
#' @return A data frame with one row for each peak (or valley) found in the
#'   data. If \code{refine.wl = FALSE}, the returned rows have \code{x} and
#'   \code{y} matching those in a row in the input \code{data}. If
#'   \code{refine.wl = TRUE}, interpolation based on a fitted spline is used to
#'   compute new \code{x} and \code{y} values.
#'
#' @section Computed and copied variables in the returned data frame:
#' \describe{
#'   \item{x}{x-value at the peak (or valley) as numeric}
#'   \item{y}{y-value at the peak (or valley) as numeric}
#'   \item{x.label}{x-value at the peak (or valley) formatted as character}
#'   \item{y.label}{y-value at the peak (or valley) formatted as character}
#'   \item{wl.color}{color definition calculated by assuming that x-values are
#'   wavelengths expressed in nanometres.}
#'   \item{BW.color}{color definition, either "black" or "white", as needed to
#'   ensure high contrast to \code{wl.color}.}
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
#' @seealso \code{\link[photobiology]{find_peaks}}, which is used internally.
#'
#' @details These stats use \code{geom_point} by default as it is the geom most
#'   likely to work well in almost any situation without need of tweaking. The
#'   default aesthetics set by these stats allow their direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The formatting of the labels
#'   returned can be controlled by the user.
#'
#'   Two tests make it possible to ignore irrelevant peaks or valleys. One test
#'   controlled by (\code{global.threshold}) is based on the absolute
#'   height/depth  of peaks/valleys and can be used in all cases to ignore
#'   globally low peaks and shallow valleys. A second test controlled by
#'   (\code{local.threshold}) is available when the window defined by `span`
#'   does not include all observations and can be used to ignore peaks/valleys
#'   that are not locally prominent. In this second approach the height/depth of
#'   each peak/valley is compared to a summary computed from other values within
#'   the window where it was found. In this second case, the reference value
#'   used is the summary indicated by \code{local.reference}. The values
#'   \code{global.threshold} and \code{local.threshold} if bare numeric are
#'   relative to the range of \emph{y}. Thresholds for ignoring too small peaks
#'   are applied after peaks are searched for, and threshold values can in some
#'   cases result in no peaks being displayed.
#' @note Parameter \code{ignore_threshold} was renamed \code{global.threshold}
#'   in version 0.3.16.
#'
#' @note These stats work nicely together with geoms \code{geom_text_repel} and
#'   \code{geom_label_repel} from package \code{\link[ggrepel]{ggrepel}} to
#'   solve the problem of overlapping labels
#'   by displacing them. To discard overlapping labels use \code{check_overlap =
#'   TRUE} as argument to \code{geom_text}.
#'
#'   By default the labels are character values ready to be added as is, but
#'   with a suitable \code{label.fmt} labels suitable for parsing by the geoms
#'   (e.g. into expressions containing Greek letters or super or subscripts) can
#'   be also easily obtained.
#'
#' @examples
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#'
#' # PEAKS
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks()
#'
#' # threshold relative to data range [0..1]
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(global.threshold = 0.6) # 0.6 * range of data
#'
#' # threshold in data units
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(global.threshold = I(0.4))
#'
#' # threshold in data units
#' ggplot(sun.spct, unit.out = "photon") +
#'   geom_line() +
#'   stat_peaks(global.threshold = I(2e-6)) # Q in mol m-2 s-1
#'
#' # VALLEYS
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_valleys()
#'
#' # discard multiple maxima or minima
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_valleys(strict = TRUE)
#'
#' # threshold relative to data range [0..1]
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_valleys(global.threshold = 0.6)
#'
#' # reverse threshold relative to data range [-1..0]
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_valleys(global.threshold = -0.9)
#'
#' # threshold in data units using I()
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_valleys(global.threshold = I(0.6), strict = TRUE)
#'
#' # USING OTHER COMPUTED VALUES
#'
#' # colours matching the wavelength at peaks
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 51, size = 2.7,
#'              mapping = aes(colour = after_stat(wl.colour))) +
#'   scale_color_identity()
#'
#' # labels for local maxima
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 51, geom = "point", colour = "red") +
#'   stat_peaks(span = 51, geom = "text", colour = "red",
#'              vjust = -0.4, label.fmt = "%3.2f nm")
#'
#' # labels for local fitted peaks
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 51, geom = "point", colour = "red", refine.wl = TRUE) +
#'   stat_peaks(span = 51, geom = "text", colour = "red",
#'              vjust = -0.4, label.fmt = "%3.2f nm",
#'              refine.wl = TRUE)
#'
#' # fitted peaks and valleys
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 31, geom = "point", colour = "red", refine.wl = TRUE) +
#'   stat_peaks(mapping = aes(fill = after_stat(wl.colour), color = after_stat(BW.colour)),
#'              span = 31, geom = "label",
#'              size = 3, vjust = -0.2, label.fmt = "%.4g nm",
#'              refine.wl = TRUE) +
#'   stat_valleys(span = 51, geom = "point", colour = "blue", refine.wl = TRUE) +
#'   stat_valleys(mapping = aes(fill = after_stat(wl.colour), color = after_stat(BW.colour)),
#'                span = 51, geom = "label",
#'                size = 3, vjust = 1.2, label.fmt = "%.4g nm",
#'                refine.wl = TRUE) +
#'   expand_limits(y = 0.85) + # make room for label
#'   scale_fill_identity() +
#'   scale_color_identity()
#'
#' @export
#' @family stats functions
#'
stat_peaks <- function(mapping = NULL,
                       data = NULL,
                       geom = "point",
                       position = "identity",
                       ...,
                       span = 5,
                       global.threshold = 0.01,
                       local.threshold = NULL,
                       local.reference = "median",
                       strict = FALSE,
                       refine.wl = FALSE,
                       method = "spline",
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
    stop("'transform' arguments must be function definitions")
  }

  ggplot2::layer(
    stat = StatPeaks, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  global.threshold = global.threshold,
                  local.threshold = local.threshold,
                  local.reference = local.reference,
                  strict = strict,
                  refine.wl = refine.wl,
                  method = method,
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

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @keywords internal
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatPeaks <-
  ggplot2::ggproto("StatPeaks", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            span,
                                            global.threshold,
                                            local.threshold,
                                            local.reference,
                                            strict,
                                            refine.wl,
                                            method,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt,
                                            x.label.transform,
                                            y.label.transform,
                                            x.colour.transform) {

                     peaks.df <-
                       photobiology::peaks(data,
                                           x.var.name = "x",
                                           y.var.name = "y",
                                           span = span,
                                           global.threshold = global.threshold,
                                           local.threshold = local.threshold,
                                           local.reference = local.reference,
                                           threshold.range = NULL,
                                           strict = strict,
                                           refine.wl = refine.wl,
                                           method = method,
                                           na.rm = FALSE)
                     peaks.df[["x.label"]] <-
                       sprintf(x.label.fmt, x.label.transform(peaks.df[["x"]]))
                     peaks.df[["y.label"]] <-
                       sprintf(y.label.fmt, y.label.transform(peaks.df[["y"]]))
                     peaks.df[["wl.color"]] <-
                       photobiology::fast_color_of_wl(x.colour.transform(peaks.df[["x"]]),
                                                      chroma.type = chroma.type)
                     peaks.df[["BW.color"]] <-
                       black_or_white(peaks.df[["wl.color"]])
                     peaks.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )

#' @rdname stat_peaks
#'
#' @export
#'
stat_valleys <- function(mapping = NULL,
                         data = NULL,
                         geom = "point",
                         position = "identity",
                         ...,
                         span = 5,
                         global.threshold = 0.01,
                         local.threshold = NULL,
                         local.reference = "median",
                         strict = FALSE,
                         refine.wl = FALSE,
                         method = "spline",
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
    stop("'transform' arguments must be function definitions")
  }

  ggplot2::layer(
    stat = StatValleys, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  global.threshold = global.threshold,
                  local.threshold = local.threshold,
                  local.reference = local.reference,
                  strict = strict,
                  refine.wl = refine.wl,
                  method = method,
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
#'
#' @export
#'
StatValleys <-
  ggplot2::ggproto("StatValleys", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            span,
                                            global.threshold,
                                            local.threshold,
                                            local.reference,
                                            strict,
                                            refine.wl,
                                            method,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt,
                                            x.label.transform,
                                            y.label.transform,
                                            x.colour.transform) {

                     valleys.df <-
                       photobiology::valleys(data,
                                             x.var.name = "x",
                                             y.var.name = "y",
                                             span = span,
                                             global.threshold = global.threshold,
                                             local.threshold = local.threshold,
                                             local.reference = local.reference,
                                             threshold.range = NULL,
                                             strict = strict,
                                             refine.wl = refine.wl,
                                             method = method,
                                             na.rm = FALSE)
                     valleys.df[["x.label"]] <-
                       sprintf(x.label.fmt, x.label.transform(valleys.df[["x"]]))
                     valleys.df[["y.label"]] <-
                       sprintf(y.label.fmt, y.label.transform(valleys.df[["y"]]))
                     valleys.df[["wl.color"]] <-
                       photobiology::fast_color_of_wl(x.colour.transform(valleys.df[["x"]]),
                                                      chroma.type = chroma.type)
                     valleys.df[["BW.color"]] <-
                       black_or_white(valleys.df[["wl.color"]])
                     valleys.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )
