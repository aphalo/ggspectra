#' Find peaks and valleys.
#'
#' \code{stat_peaks} finds at which x positions local maxima are located. If
#' you want find local minima, you can use \code{stat_valleys} instead.
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
#' @param ignore_threshold numeric For peaks, value between 0.0 and 1.0
#'   indicating the relative size of peaks compared to tallest peak threshold
#'   below which peaks will be ignored, while negative values between 0.0 and
#'   -1.0 set a threshold so that the tallest peaks are ignored, instead of the
#'   shortest. For valleys, value between 0.0 and 1.0 indicating the relative
#'   depth of valleys below which valleys will be ignored, while negative values
#'   between 0.0 and -1.0 set a threshold so that the deeper valleys are
#'   ignored, instead of the shallower ones.
#' @param span integer A peak is defined as an element in a sequence which is
#'   greater than all other elements within a window of width \code{span}
#'   centered at that element. Use \code{NULL} for the global peak. Valleys are
#'   the reverse.
#' @param strict logical If \code{TRUE}, an element must be strictly greater
#'   than all other values in its window to be considered a peak.
#' @param refine.wl logical Flag indicating if peak or valleys locations should
#'   be refined by fitting a function.
#' @param method character String with the name of a method used for peak
#'   fitting. Currently only spline interpolation is implemented.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
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
#' @note These stats work nicely together with geoms
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
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_valleys()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 51, geom = "point", colour = "red") +
#'   stat_peaks(span = 51, geom = "text", colour = "red",
#'              vjust = -0.4, label.fmt = "%3.2f nm")
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 51, geom = "point", colour = "red", refine.wl = TRUE) +
#'   stat_peaks(span = 51, geom = "text", colour = "red",
#'              vjust = -0.4, label.fmt = "%3.2f nm",
#'              refine.wl = TRUE)
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 51, geom = "point", colour = "red", refine.wl = TRUE) +
#'   stat_peaks(mapping = aes(fill = stat(wl.colour), color = stat(BW.colour)),
#'              span = 51, geom = "label",
#'              size = 3, vjust = -0.2, label.fmt = "%.3g nm",
#'              refine.wl = TRUE) +
#'   stat_valleys(span = 71, geom = "point", colour = "blue", refine.wl = TRUE) +
#'   stat_valleys(mapping = aes(fill = stat(wl.colour), color = stat(BW.colour)),
#'                span = 71, geom = "label",
#'                size = 3, vjust = 1.2, label.fmt = "%.3g nm",
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
                       ignore_threshold = 0.01,
                       strict = is.null(span),
                       refine.wl = FALSE,
                       method = "spline",
                       chroma.type = "CMF",
                       label.fmt = "%.3g",
                       x.label.fmt = label.fmt,
                       y.label.fmt = label.fmt,
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatPeaks, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  refine.wl = refine.wl,
                  method = method,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
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
                                            ignore_threshold,
                                            strict,
                                            refine.wl,
                                            method,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     peaks.df <- photobiology::peaks(data,
                                                     x.var.name = "x",
                                                     y.var.name = "y",
                                                     span = span,
                                                     ignore_threshold = ignore_threshold,
                                                     strict = strict,
                                                     refine.wl = refine.wl,
                                                     method = method,
                                                     na.rm = FALSE)
                     dplyr::mutate(peaks.df,
                                   x.label = sprintf(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y),
                                   wl.color = photobiology::fast_color_of_wl(x, chroma.type = chroma.type),
                                   BW.color = black_or_white(wl.color))
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
                         ignore_threshold = -0.01,
                         strict = is.null(span),
                         refine.wl = FALSE,
                         method = "spline",
                         chroma.type = "CMF",
                         label.fmt = "%.3g",
                         x.label.fmt = label.fmt,
                         y.label.fmt = label.fmt,
                         na.rm = FALSE,
                         show.legend = FALSE,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatValleys, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  refine.wl = refine.wl,
                  method = method,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
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
                                            ignore_threshold,
                                            strict,
                                            refine.wl,
                                            method,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     valleys.df <- photobiology::valleys(data,
                                                         x.var.name = "x",
                                                         y.var.name = "y",
                                                         span = span,
                                                         ignore_threshold = ignore_threshold,
                                                         strict = strict,
                                                         refine.wl = refine.wl,
                                                         method = method,
                                                         na.rm = FALSE)
                     dplyr::mutate(valleys.df,
                                   x.label = sprintf(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y),
                                   wl.color = photobiology::fast_color_of_wl(x, chroma.type = chroma.type),
                                   BW.color = black_or_white(wl.color))
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )
