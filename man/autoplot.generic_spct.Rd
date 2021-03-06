% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot-generic-spct.R
\name{autoplot.generic_spct}
\alias{autoplot.generic_spct}
\alias{autoplot.generic_mspct}
\title{Warn if autoplot method is called for generic_spct}
\usage{
\method{autoplot}{generic_spct}(object, ...)

\method{autoplot}{generic_mspct}(object, ...)
}
\value{
an empty \code{ggplot} object, triggering a warning.
}
\description{
No automatic plot method is possible for objects of class \code{generic_spct}
as this class is meant to be used only as a pure base class for derivation.
As \code{generic_mspct} objects can contain an assortment of objects
including \code{generic_spct} and classes derived from \code{generic_spct}
the spectra are unlikely be suitable for plotting in the same ggplot.
}
\keyword{internal}
