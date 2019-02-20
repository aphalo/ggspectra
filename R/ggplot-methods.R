#' Create a new ggplot plot from spectral data.
#'
#' \code{ggplot()} initializes a ggplot object. It can be used to declare the
#' input spectral object for a graphic and to optionally specify the set of plot
#' aesthetics intended to be common throughout all subsequent layers unless
#' specifically overridden.
#'
#' \code{ggplot()} is typically used to construct a plot incrementally, using
#' the \code{+} operator to add layers to the existing ggplot object. This is
#' advantageous in that the code is explicit about which layers are added and
#' the order in which they are added. For complex graphics with multiple layers,
#' initialization with \code{ggplot} is recommended.
#'
#' We show seven common ways to invoke \code{ggplot} for spectra and
#' collections of spectra:
#' \itemize{
#'    \item \code{ggplot(spct)}
#'    \item \code{ggplot(spct, unit.out = <unit.to.use>)}
#'    \item \code{ggplot(spct, plot.qty = <quantity.to.plot>)}
#'    \item \code{ggplot(spct, range = <wavelength.range>)}
#'    \item \code{ggplot(spct) + aes(<other aesthetics>)}
#'    \item \code{ggplot(spct, aes(x, y, <other aesthetics>))}
#'    \item \code{ggplot(spct, aes())}
#'   }
#' The first method is recommended if all layers use the same data and the same
#' set of automatic default x and y aesthetics. The second, third and fourth use
#' automatic default x and y aesthetics but first transform or trim the spectral
#' data to be plotted. The fifth uses automatic default x and y aesthetics and
#' adds mappings for other aesthetics. These patterns can be combined as needed.
#' The sixth disables the use of a default automatic mapping, while the seventh
#' delays the mapping of aesthetics and can be convenient when using different
#' mappings for different geoms.
#'
#' @param data Default spectrum dataset to use for plot. If not a spectrum, the
#'   methods used will be those defined in package \code{ggplot2}. See
#'   \code{\link[ggplot2]{ggplot}}. If not specified, must be suppled in each
#'   layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot. If not
#'   specified, in the case of spectral objects, a default mapping will be used.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param unit.out character string indicating type of units to use for
#'   plotting spectral irradiance or spectral response, \code{"photon"} or
#'   \code{"energy"}.
#' @param ... Other arguments passed on to methods.
#' @param environment If a variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which \code{ggplot()} is called.
#'
#' @section Collections of spectra: The method for collections of spectra
#'   accepts arguments for the same parameters as the corresponding methods for
#'   single spectra. Heterogeneous generic collections of spectra are not
#'   supported. When plotting collections of spectra the factor \code{spct.idx}
#'   contains as levels the names of the individual members of the collection,
#'   and can be mapped to aesthetics or used for faceting.
#'
#' @export
#' @examples
#'
#' ggplot(sun.spct) + geom_line()
#' ggplot(sun.spct, unit.out = "photon") + geom_line()
#'
#' ggplot(yellow_gel.spct) + geom_line()
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") + geom_line()
#'
#' ggplot(Ler_leaf.spct) + facet_grid(~variable) + geom_line()
#' ggplot(Ler_leaf.spct) + aes(linetype = variable) + geom_line()
#'
#' @note Current implementation does not merge the default mapping with user
#' supplied mapping. If user supplies a mapping, it is used as is, and
#' variables should be present in the spectral object. In contrast, when
#' using the default mapping, unit conversion is done on the fly when needed.
#' To add to the default mapping, \code{aes()} can be used by itself to compose
#' the ggplot.
#'
#' @name ggplot
#'
ggplot.source_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           unit.out = getOption("photobiology.radiation.unit", default = "energy"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
    if (unit.out == "energy") {
      data <- q2e(data)
      mapping <- aes_(~w.length, ~s.e.irrad)
    } else if (unit.out %in% c("photon", "quantum")) {
      data <- e2q(data)
      mapping <- aes_(~w.length, ~s.q.irrad)
    } else {
      stop("Invalid 'unit.out' argument value: '", unit.out, "'")
    }
  }
  rmDerivedSpct(data)
  ggplot(data = data, mapping = mapping, ...,
                    environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.response_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           unit.out = getOption("photobiology.radiation.unit", default = "energy"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
      if (unit.out == "energy") {
        data <- q2e(data)
        mapping <- aes_(~w.length, ~s.e.response)
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- e2q(data)
        mapping <- aes_(~w.length, ~s.q.response)
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
    }
    rmDerivedSpct(data)
    ggplot(data = data, mapping = mapping, ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @param plot.qty character string one of \code{"transmittance"} or
#'   \code{"absorbance"} for \code{filter_spct}, and one of
#'   \code{"transmittance"}, \code{"reflectance"} or \code{"all"} for
#'   \code{object_spct}.
#'
#' @export
#'
ggplot.filter_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           plot.qty = getOption("photobiology.filter.qty", default = "transmittance"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
    if (plot.qty == "transmittance") {
      data <- A2T(data)
      mapping <- aes_(~w.length, ~Tfr)
    } else if (plot.qty == "absorptance") {
      data <- T2Afr(data)
      mapping <- aes_(~w.length, ~Afr)
    } else if (plot.qty == "absorbance") {
      data <- T2A(data)
      mapping <- aes_(~w.length, ~A)
    } else {
      stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
    }
  }
  rmDerivedSpct(data)
  ggplot(data = data, mapping = mapping, ...,
         environment = environment)
  }

#' @rdname ggplot
#'
#' @note plot.qty is ignored for reflectors.
#'
#' @export
#'
ggplot.reflector_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           plot.qty = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
      mapping <- aes_(~w.length, ~Rfr)
    }
    rmDerivedSpct(data)
    ggplot(data = data, mapping = mapping, ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.cps_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
      if ("cps" %in% names(data)) {
        mapping <- aes_(~w.length, ~cps)
      } else {
        mapping <- aes_(~w.length, ~cps_1)
      }
    }
    rmDerivedSpct(data)
    ggplot(data = data, mapping = mapping, ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.calibration_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
      if ("irrad.mult" %in% names(data)) {
        mapping <- aes_(~w.length, ~irrad.mult)
      } else {
        mapping <- aes_(~w.length, ~irrad.mult_1)
      }
    }
    rmDerivedSpct(data)
    ggplot(data = data, mapping = mapping, ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.raw_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    if (is.null(mapping)) {
      if ("counts" %in% names(data)) {
        mapping <- aes_(~w.length, ~counts)
      } else {
        mapping <- aes_(~w.length, ~counts_1)
      }
    }
    rmDerivedSpct(data)
    ggplot(data = data, mapping = mapping, ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.object_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           plot.qty = getOption("photobiology.object.qty", default = "all"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    # If plotting a single variable, we use other methods
    if (plot.qty == "reflectance") {
      return(ggplot(as.reflector_spct(data)))
    } else if (plot.qty %in% c("transmittance", "absorbance", "absorptance")) {
      return(ggplot(as.filter_spct(data)))
    } else if (plot.qty != "all") {
      stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
    }
    # compute absorptance
    data[["Afr"]] <- 1.0 - data[["Tfr"]] - data[["Rfr"]]
    if (any((data[["Afr"]]) < -0.01)) {
      message("Bad data or fluorescence.")
    }
    # melt data into long form
    molten.data <-
      tidyr::gather_(dplyr::select_(data, "w.length", "Tfr", "Afr", "Rfr"),
                     "variable", "value", c("Tfr", "Afr", "Rfr"))
    # if not supplied create a mapping
    if (is.null(mapping)) {
      mapping <- aes_(~w.length, ~value)
    }
    # convert to a tibble so that dispatch goes to ggplot2::ggplot.data.frame()
    rmDerivedSpct(molten.data)
    ggplot(data = molten.data, mapping = mapping, ...,
           environment = environment)
  }

# collections of spectra --------------------------------------------------

#' @rdname ggplot
#'
#' @export
#'
ggplot.generic_mspct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    spct <- rbindspct(data)
    ggplot(data = spct, mapping = mapping, ...)
  }
