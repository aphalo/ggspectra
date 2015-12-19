#' Find peaks and valleys.
#'
#' \code{stat_peaks} finds at which x positions local maxima are located. If
#' you want find local minima, you can use \code{\link{stat_valleys}} instead.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
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
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param ... other arguments passed on to \code{\link{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' @param ignore_threshold numeric value between 0.0 and 1.0 indicating the size
#'   threshold below which peaks will be ignored.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element. The default value is 5, meaning that a peak is bigger than two
#'   consequtive neighbors on each side. Default: 5.
#' @param strict logical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: FALSE.
#' @param label.fmt character  string giving a format definition for converting
#'   $x$-values into character strings by means of function \code{\link{sprintf}}.
#' @section Computed variables:
#' \describe{
#'   \item{x.label}{x-value at the peak}
#' }
#' @seealso \code{\link[photobiology]{find_peaks}}, which is used internally.
#'
#' @examples
#' library(photobiology)
#' library(ggplot2)
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_peaks()
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_valleys()
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_peaks(span = 21, geom = "point", colour = "red") +
#'   stat_valleys(span = 21, geom = "point", colour = "blue") +
#'   stat_peaks(span = 51, geom = "text", colour = "red",
#'              vjust = -0.3, label.fmt = "%3.0f nm") +
#'   stat_valleys(span = 51, geom = "text", colour = "blue",
#'                vjust = 1.2, label.fmt = "%3.0f nm")
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_point() +
#'   stat_peaks(span = 5, geom = "line", colour = "red")
#' @export
#' @family stats functions
#'
stat_peaks <- function(mapping = NULL, data = NULL, geom = "point",
                       span = 5, ignore_threshold = 0, strict = FALSE,
                       label.fmt = "%3.1f",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPeaks, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  label.fmt = label.fmt,
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
#' Each of the \code{Stat*} objects is a \code{\link{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatPeaks <- ggproto("StatPeaks", Stat,
                     compute_group = function(data,
                                              scales,
                                              span,
                                              ignore_threshold,
                                              strict,
                                              label.fmt) {
                     peaks.df <- data[photobiology::find_peaks(data$y,
                                      span = span,
                                      ignore_threshold = ignore_threshold,
                                      strict = strict), , drop = FALSE]
                     peaks.df$x.label <- sprintf(label.fmt, peaks.df$x)
                     peaks.df
                     },
                     default_aes = aes(label = ..x.label..),
                     required_aes = c("x", "y")
)

#' @rdname stat_peaks
#'
#' @export
#'
stat_valleys <- function(mapping = NULL, data = NULL, geom = "point",
                         span = 5, ignore_threshold = 0, strict = FALSE,
                         label.fmt = "%3.1f",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatValleys, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  label.fmt = label.fmt,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#'
#' @export
#'
StatValleys <- ggproto("StatValleys", Stat,
                       compute_group = function(data,
                                                scales,
                                                span,
                                                ignore_threshold,
                                                strict,
                                                label.fmt) {
                         valleys.df <- data[photobiology::find_peaks(-data$y,
                                                     span = span,
                                                     ignore_threshold = ignore_threshold,
                                                     strict = strict), , drop = FALSE]
                         valleys.df$x.label <- sprintf(label.fmt, valleys.df$x)
                         valleys.df
                       },
                       default_aes = aes(label = ..x.label..),
                       required_aes = c("x", "y")
)

