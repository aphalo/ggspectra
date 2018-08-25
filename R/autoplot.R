#' Plot a spectrum.
#'
#' This method returns a ggplot object with an annotated plot of an object of a
#' class derived from \code{generic_spct} or of a class derived from
#' \code{generic_mspct} for which a \code{plot()} method exists.
#'
#' @param object An R object derived from class \code{generic_spct} or derived
#'   from class \code{generic_mspct}.
#' @param ... Named arguments passed to \code{plot()} methods.
#'
#' @details Support for \code{autoplot()} method for consistency
#'   with package 'ggplot2'. Please consult the documentation of the
#'   \code{plot()} methods for details about use of these autoplot methods.
#'   They are implemented as simple wrappers that forward the call to
#'   \code{plot()}.
#'
#' @note The generic for this method is defined in package 'ggplot2' and
#'   specializations for objects of diverse classes are provided by 'ggplot2'
#'   and other packages.
#'
#' @seealso \code{\link{plot.calibration_spct}},  \code{\link{plot.cps_spct}},
#'  \code{\link{plot.filter_spct}}, \code{\link{plot.raw_spct}},
#'   \code{\link{plot.response_spct}},  \code{\link{plot.source_spct}} and
#'    \code{\link{plot.waveband}}.
#'
#' @return a \code{ggplot} object.
#'
#' @examples
#' library(photobiology)
#' ggplot2::autoplot(sun.spct, annotations = "")
#'
#' @export
#'
#' @family plot functions
#'
#' @rdname autoplot
#'
autoplot.generic_spct <- function(object, ...) {
  plot(x = object, ...)
}

#' @rdname autoplot
#'
#' @export
#'
autoplot.generic_mspct <- function(object, ...) {
  plot(x = object, ...)
}

#' @rdname autoplot
#'
#' @export
#'
autoplot.waveband <- function(object, ...) {
  plot(x = object, ...)
}

#' Warn if plot methods is called.
#'
#' No automatic plot method is possible for objects of class \code{geenric_spct}
#' as this class is meant to be used only as a pure base class for derivation.
#'
#' @return an empty \code{ggplot} object.
#'
#' @keywords internal
#'
#' @export
#'
plot.generic_spct <- function(x, ...) {
  warning("No specialized plot() method exist for objects of class generic_spct")
  ggplot()
}

#' @rdname plot.generic_spct
#'
#' @export
#'
plot.generic_mspct <- function(x, ...) {
  warning("No specialized plot() method exist for objects of class generic_mspct")
  ggplot()
}

