#' @details
#' Pakage \code{ggspectra} is a package with extensions to ggplot2 for plotting
#' spectral data. It defines plot() especializations for the classes in
#' package photobiology. It adds new statistics useful when the x-aesthetic
#' is mapped to a numeric variable giving wavelengths in nanometres. It also
#' adds a geom suitable for plotting spectral data and specializations of
#' method \code{ggplot} for objects of the spectral classes defined in
#' package \package{photobiology}.
#'
#' @references
#' \code{ggplot2} web site at \url{http://ggplot2.org/}\cr
#' \code{ggplot2} source code at \url{https://github.com/hadley/ggplot2}\cr
#' Function \code{multiplot} from \url{http://www.cookbook-r.com/}
#'
#' @author Pedro J. Aphalo
#'
#' @import photobiology photobiologyWavebands ggplot2
#'
#' @note
#' This package is a rewrite of package \code{photobiologygg} making use of the
#' new features of \code{ggplot2} 2.0.0 which makes writing this kind of
#' extensions really easy.
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' # maximum
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#' stat_peaks(span = NULL)
#'
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#' stat_peaks(span = 21, geom = "text")
#'
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_valleys(span = 21, geom = "text")
#'
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_peaks(span = 21, geom = "point", colour = "red") +
#'   stat_valleys(span = 21, geom = "point", colour = "blue") +
#'   stat_peaks(span = 51, geom = "text", colour = "red", vjust = -0.3,
#'              label.fmt = "%3.0f nm") +
#'   stat_valleys(span = 51, geom = "text", colour = "blue", vjust = 1.2,
#'                label.fmt = "%3.0f nm")
#'
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_color() + scale_color_identity()
#'
#' plot(sun.spct)
#'
"_PACKAGE"
