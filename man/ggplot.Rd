% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot-methods.R
\name{ggplot}
\alias{ggplot}
\alias{ggplot.source_spct}
\alias{ggplot.response_spct}
\alias{ggplot.filter_spct}
\alias{ggplot.reflector_spct}
\alias{ggplot.cps_spct}
\alias{ggplot.calibration_spct}
\alias{ggplot.raw_spct}
\alias{ggplot.object_spct}
\alias{ggplot.generic_spct}
\alias{ggplot.generic_mspct}
\alias{ggplot.filter_mspct}
\alias{ggplot.source_mspct}
\alias{ggplot.object_mspct}
\title{Create a new ggplot plot from spectral data.}
\usage{
\method{ggplot}{source_spct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  unit.out = getOption("photobiology.radiation.unit", default = "energy"),
  environment = parent.frame()
)

\method{ggplot}{response_spct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  unit.out = getOption("photobiology.radiation.unit", default = "energy"),
  environment = parent.frame()
)

\method{ggplot}{filter_spct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  plot.qty = getOption("photobiology.filter.qty", default = "transmittance"),
  environment = parent.frame()
)

\method{ggplot}{reflector_spct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  plot.qty = NULL,
  environment = parent.frame()
)

\method{ggplot}{cps_spct}(data, mapping = NULL, ..., range = NULL, environment = parent.frame())

\method{ggplot}{calibration_spct}(data, mapping = NULL, ..., range = NULL, environment = parent.frame())

\method{ggplot}{raw_spct}(data, mapping = NULL, ..., range = NULL, environment = parent.frame())

\method{ggplot}{object_spct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  plot.qty = getOption("photobiology.object.qty", default = "all"),
  environment = parent.frame()
)

\method{ggplot}{generic_spct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  spct_class,
  environment = parent.frame()
)

\method{ggplot}{generic_mspct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  idfactor = TRUE,
  environment = parent.frame()
)

\method{ggplot}{filter_mspct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  plot.qty = getOption("photobiology.filter.qty", default = "transmittance"),
  idfactor = TRUE,
  environment = parent.frame()
)

\method{ggplot}{source_mspct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  unit.out = getOption("photobiology.radiation.unit", default = "energy"),
  idfactor = TRUE,
  environment = parent.frame()
)

\method{ggplot}{object_mspct}(
  data,
  mapping = NULL,
  ...,
  range = NULL,
  plot.qty = getOption("photobiology.object.qty", default = ifelse(length(data) > 1L,
    "as.is", "all")),
  idfactor = TRUE,
  environment = parent.frame()
)
}
\arguments{
\item{data}{Default spectrum dataset to use for plot. If not a spectrum, the
methods used will be those defined in package \code{ggplot2}. See
\code{\link[ggplot2]{ggplot}}. If not specified, must be supplied in each
layer added to the plot.}

\item{mapping}{Default list of aesthetic mappings to use for plot. If not
specified, in the case of spectral objects, a default mapping will be used.}

\item{...}{Other arguments passed on to methods.}

\item{range}{an R object on which range() returns a vector of length 2, with
min and max wavelengths (nm).}

\item{unit.out}{character string indicating type of units to use for
plotting spectral irradiance or spectral response, \code{"photon"} or
\code{"energy"}.}

\item{environment}{If a variable defined in the aesthetic mapping is not
found in the data, ggplot will look for it in this environment. It defaults
to using the environment in which \code{ggplot()} is called. The use of
these parameter has been deprecated in 'ggplot2' in favour of "tidy
evaluation".}

\item{plot.qty}{character string One of \code{"transmittance"},
\code{"absorptance"} or \code{"absorbance"} for \code{filter_spct} objects,
and in addition to these \code{"reflectance"}, \code{"all"} or
\code{"as.is"} for \code{object_spct} objects.}

\item{spct_class}{character Class into which a \code{generic_spct} object
will be converted before plotting. The column names in data should
match those expected by the class constructor (see
\code{\link[photobiology]{setGenericSpct}}); other arguments should be
passed by name). If the argument is \code{"generic_spct"}, \code{"tibble"}
or \code{"data.frame"} no aesthetic mapping will be set auutomatically.}

\item{idfactor}{logical or character Generates an index column of
\code{factor} type. Default is (\code{idfactor=TRUE}) for both lists and
\code{_mspct} objects. If \code{idfactor=TRUE} then the column is auto
named \code{spct.idx}. Alternatively the column name can be directly
provided to \code{idfactor} as a character string.}
}
\value{
A ggplot object, containing data and mapping of data to aesthetics
  but no plot layers.
}
\description{
\code{\link[ggplot2]{ggplot}} methods initialize a ggplot object. They can be
used to declare the input data object for a graphic and to optionally specify
the set of plot aesthetics intended to be common throughout all subsequent
layers unless specifically overridden. The method specializations from
package 'ggspectra' support the classes for storage of spectral data from
package '\link[photobiology:photobiology-package]{photobiology}'.
}
\details{
\code{ggplot} is typically used to construct a plot incrementally, using
the \code{+} operator to add layers to the existing ggplot object. This is
advantageous in that the code is explicit about which layers are added and
the order in which they are added. For complex graphics with multiple layers,
initialization with \code{ggplot} is recommended.

We show seven common ways to invoke \code{ggplot} methods for spectra and
collections of spectra:
\itemize{
   \item \code{ggplot(spct)}
   \item \code{ggplot(spct, unit.out = <unit.to.use>)}
   \item \code{ggplot(spct, plot.qty = <quantity.to.plot>)}
   \item \code{ggplot(spct, range = <wavelength.range>)}
   \item \code{ggplot(spct) + aes(<other aesthetics>)}
   \item \code{ggplot(spct, aes(x, y, <other aesthetics>))}
   \item \code{ggplot(spct, aes())}
  }
The first approach is recommended if all layers use the same data and the same
set of automatic default x and y aesthetics. The second, third and fourth use
automatic default x and y aesthetics but first transform or trim the spectral
data to be plotted. The fifth uses automatic default x and y aesthetics and
adds mappings for other aesthetics. These patterns can be combined as needed.
The sixth overrides the default automatic mapping, while the seventh
delays the mapping of aesthetics and can be convenient when using different
mappings for different geoms.

When using the default automatic mapping to \emph{x} and \emph{y}
  aesthetics, unit or quantity conversions are done on the fly according to
  the arguments passed to parameters \code{unit.out} and \code{plot.qty}. In
  contrast, if a mapping for \emph{x} and/or \emph{y} aesthetics is passed as
  an argument to parameter \code{mapping}, the arguments to parameters
  \code{unit.out} and \code{plot.qty} are ignored and all the mapped
  variables should be present in the spectral object passed as argument to
  data.

  The current implementation merges the default mapping for \emph{x} and
  \emph{y} aesthetics with the user supplied mapping if it only contains
  mappings to aesthetics other than \emph{x} or \emph{y} or an empty
  mapping. In addition, when the user does not pass an argument to
  \code{mapping}, not even an empty one, if the object contains
  multiple spectra, a mapping of the indexing factor to the \code{group}
  aesthetic is added. The name of the id factor is retrieved
  from the \code{data} object metadata.

  Differently to objects of other spectral classes, objects of class
  \code{\link[photobiology]{object_spct}} contain data for multiple physical
  quantities. Thus, in the case of class \code{object_spct}, the special
  arguments \code{"all"} and \code{"as.is"} can be passed as argument to
  \code{plot.qty}. Where \code{all}, the defaul indicates that the data are
  to be converted into long form and indexed with a factor named
  \code{variable}, to allow stacking or faceting. In contrast, \code{"as.is"}
  indicates that data for the different quantities should remain in separate
  variables (=columns) when added to the plot object. \code{"reflectance"}
  passed as argument to \code{plot.qty} triggers conversion of the
  \code{object_spct} object passed as argument to \code{data} into a
  \code{\link[photobiology]{reflector_spct}} object and \code{"absorbance"},
  \code{"absorptance"} and \code{"reflectance"}, trigger conversion into a
  \code{\link[photobiology]{filter_spct}} object. After conversion the
  objects are forwarded to the matching \code{ggplot} method.

  The methods for collections of spectra accept arguments
  through additional. When plotting collections of spectra a factor named as
  indicated by the argument passed to parameter \code{idfactor}, or
  \code{"spct.idx"} by default, is added using as levels the names of the
  individual members of the collection. The spectral object is forwarded
  to the \code{ggplot} method matching its new class.

  \emph{Heterogeneous generic collections of spectra containing members
  belonging to more than one class are not supported.}
}
\note{
plot.qty is ignored for reflectors.
}
\examples{
# source
ggplot(sun.spct) + geom_line()
ggplot(sun.spct, unit.out = "photon") + geom_line()

# multiple spectra in long form
ggplot(sun_evening.spct) + geom_line()
ggplot(sun_evening.spct, aes(linetype = spct.idx)) + geom_line()

# collection of spectra
ggplot(sun_evening.mspct, idfactor = "step") +
  geom_line()
ggplot(sun_evening.mspct, idfactor = "step", aes(colour = step)) +
  geom_line()

# filter
ggplot(yellow_gel.spct) + geom_line()
ggplot(yellow_gel.spct, plot.qty = "absorbance") + geom_line()

# object
ggplot(Ler_leaf.spct) + facet_grid(~variable) + geom_line()
ggplot(Ler_leaf.spct) + aes(fill = variable) + geom_area()
ggplot(Ler_leaf.spct) + aes(linetype = variable) + geom_line()
ggplot(Ler_leaf.spct, plot.qty = "absorptance") + geom_line()

}
\seealso{
Method \code{link[ggspectra]{autoplot}} provides further automation
  of plot creation. Function \code{\link[photobiology]{rbindspct}} is used to
  convert collections of spectra into "long-form" spectral objects. The
  generic of method \code{link[ggplot2](ggplot)} is defined in package
  'ggplot2'.
}
