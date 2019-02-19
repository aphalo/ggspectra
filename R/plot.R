#' Create a complete ggplot for a spectrum.
#'
#' This method returns a ggplot object with an annotated plot of an object of a
#' class derived from \code{generic_spct} or of a class derived from
#' \code{generic_mspct} for which a \code{plot()} method exists. It is implemented
#' as a wrapper of \code{autoplot()}. This function is available for backwards
#' compatibility, but new code should call this same function using
#' method \code{autoplot()} instead.
#'
#' @param x An R object derived from class \code{generic_spct} or derived
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
#' @seealso \code{\link{autoplot.calibration_spct}},  \code{\link{autoplot.cps_spct}},
#'  \code{\link{autoplot.filter_spct}}, \code{\link{autoplot.raw_spct}},
#'   \code{\link{autoplot.response_spct}},  \code{\link{autoplot.source_spct}} and
#'    \code{\link{autoplot.waveband}}.
#'
#' @return a \code{ggplot} object.
#'
#' @examples
#'
#' plot(sun.spct, annotations = "") # deprecated syntax
#' autoplot(sun.spct, annotations = "") # preferred syntax
#'
#' @export
#'
#' @family autoplot functions
#'
#' @rdname plot
#'
plot.generic_spct <- function(x, ...) {
  autoplot(object = x,
           object.label = deparse(substitute(x)),
           ...)
}

#' @rdname plot
#'
#' @export
#'
plot.generic_mspct <- function(x,
                               ...) {
  autoplot(object = x,
           object.label = deparse(substitute(x)),
           ...)
}

#' @rdname plot
#'
#' @export
#'
plot.waveband <- function(x, ...) {
  autoplot(object = x,
           object.label = deparse(substitute(x)),
           ...)
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
autoplot.generic_spct <- function(object, ...) {
  warning("No specialized autoplot() method exist for objects of class generic_spct.")
  ggplot()
}

#' @rdname autoplot.generic_spct
#'
#' @export
#'
autoplot.generic_mspct <- function(object, ...) {
  warning("No specialized autoplot() method exist for objects of class generic_mspct.")
  ggplot()
}

