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
#'   \code{\link[ggplot2]{ggplot}}. If not specified, must be supplied in each
#'   layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot. If not
#'   specified, in the case of spectral objects, a default mapping will be used.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min and max wavelengths (nm).
#' @param unit.out character string indicating type of units to use for
#'   plotting spectral irradiance or spectral response, \code{"photon"} or
#'   \code{"energy"}.
#' @param plot.qty character string One of \code{"transmittance"},
#'   \code{"absorptance"} or \code{"absorbance"} for \code{filter_spct} objects,
#'   and in addition to these \code{"reflectance"}, \code{"all"} or
#'   \code{"as.is"} for \code{object_spct} objects.
#' @param ... Other arguments passed on to methods.
#' @param environment If a variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which \code{ggplot()} is called.
#'
#' @section Object spectra: In the case of class object_spct, the arguments
#'   \code{"all"} and \code{"as.is"} if passed to \code{plot.qty}, indicate in
#'   the first case that the data are to be converted into long form, to allow
#'   stacking, while in the second case \code{data} is copied unchanged to the
#'   plot object. \code{"reflectance"} passed to \code{plot.qty} converts
#'   \code{data} into a \code{replector_spct} object and \code{"absorbance"},
#'   \code{"absorptance"} and \code{"reflectance"}, convert \code{data} into a
#'   \code{filter_spct}.
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
#'   supplied mapping. If user supplies a mapping, it is used as is, and
#'   variables should be present in the spectral object. In contrast, when using
#'   the default mapping, unit or quantity conversions are done on the fly when
#'   needed. To add to the default mapping, \code{aes()} can be used by itself
#'   to compose the ggplot. In all cases, except when an \code{object_spct} is
#'   converted into long form, the data member of the returned plot object
#'   retains its class and attributes.
#'
#' @name ggplot
#'
ggplot.source_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           environment = parent.frame()) {

    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      if (unit.out == "energy") {
        data <- photobiology::q2e(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.e.irrad"]])
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- photobiology::e2q(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.q.irrad"]])
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
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
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      if (unit.out == "energy") {
        data <- photobiology::q2e(data)
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.e.response"]])
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- photobiology::e2q(data)
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.q.response"]])
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.filter_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           plot.qty = getOption("photobiology.filter.qty", default = "transmittance"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      if (plot.qty == "transmittance") {
        data <- photobiology::any2T(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["Tfr"]])
      } else if (plot.qty == "absorptance") {
        data <- photobiology::any2Afr(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["Afr"]])
      } else if (plot.qty == "absorbance") {
        data <- photobiology::any2A(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["A"]])
      } else {
        stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
      }
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
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
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      mapping <- ggplot2::aes(.data[["w.length"]], .data[["Rfr"]])
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.cps_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      if ("cps" %in% names(data)) {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["cps"]])
      } else {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["cps_1"]])
      }
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.calibration_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      if ("irrad.mult" %in% names(data)) {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["irrad.mult"]])
      } else {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["irrad.mult_1"]])
      }
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.raw_spct <-
  function(data, mapping = NULL, ..., range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    default.mapping <- is.null(mapping)
    if (default.mapping) {
      if ("counts" %in% names(data)) {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["counts"]])
      } else {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["counts_1"]])
      }
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (default.mapping) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1) {
        p + ggplot2::aes(linetype = photobiology::getIdFactor(p[["data"]]))
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.object_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = getOption("photobiology.object.qty", default = "all"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    # If plotting a single variable, we use other methods
    if (plot.qty == "reflectance") {
      return(ggplot(data = photobiology::as.reflector_spct(data),
                    mapping = mapping,
                    ...))
    } else if (plot.qty %in% c("transmittance", "absorbance", "absorptance")) {
      return(ggplot(data = photobiology::as.filter_spct(data),
                    mapping = mapping,
                    plot.qty = plot.qty,
                    ...))
    } else if (!plot.qty %in% c("all", "as.is")) {
      stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
    }
    # compute absorptance
    data[["Afr"]] <- 1.0 - data[["Tfr"]] - data[["Rfr"]]
    if (any((data[["Afr"]]) < -0.01)) {
      message("Bad data or fluorescence.")
    }
    if (plot.qty == "as.is") {
      data.class <- class(data)
      photobiology::rmDerivedSpct(data)
      p <- ggplot2::ggplot(data = data, mapping = mapping,
                           environment = environment)
      class(p[["data"]]) <- data.class
    } else {
      num.spectra <- photobiology::getMultipleWl(data)
      if (num.spectra > 1) {
        stop("Multiple spectra in long form not supported.")
      }
      # Attributes will be lost when melting the tibble or data frame
      data.attributes <- photobiology::get_attributes(data)
      # Once molten it will not pass checks as object_spct
      molten.tb <- photobiology::spct_wide2long(data, idfactor = "variable", rm.spct.class = TRUE)
      # if not supplied create a mapping
      if (is.null(mapping)) {
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["value"]])
      }
      # roundabout way of retaining the attributes without calling any
      # private (not exported) method or function from 'ggplot2'
      p <- ggplot2::ggplot(data = molten.tb, mapping = mapping, ...,
                           environment = environment)
      attributes(p$data) <- c(attributes(p$data), data.attributes)
    }
    p
  }

#' @rdname ggplot
#'
#' @param spct_class character Class into which a \code{generic_spct} object
#'   will be converted before plotting. The column names in data should
#'   match those expected by the class constructor (see
#'   \code{\link[photobiology]{setGenericSpct}}); other arguments should be
#'   passed by name).
#'
#' @export
#'
ggplot.generic_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           spct_class,
           environment = parent.frame()) {
    # redundant, used as a kludge to silence NOTE in CRAN checks
#    stopifnot(tibble::is_tibble(data))

    if (!spct_class %in% photobiology::spct_classes()) {
      stop("Invalid 'spct_class' argument: \"", spct_class)
    }

    # this list could be defined in package 'photobiology'
    funs <- list(filter_spct = photobiology::setFilterSpct,
                 reflector_spct = photobiology::setReflectorSpct,
                 object_spct = photobiology::setObjectSpct,
                 source_spct = photobiology::setSourceSpct,
                 response_spct = photobiology::setResponseSpct,
                 chroma_spct = photobiology::setChromaSpct,
                 raw_spct = photobiology::setRawSpct,
                 cps_spct = photobiology::setCpsSpct,
                 calibration_spct = photobiology::setCalibrationSpct,
                 generic_spct = tibble::as_tibble,
                 tibble = tibble::as_tibble)
    spct <- funs[[spct_class]](data, ...)

    # dispatched to one of the methods defined above
    ggplot2::ggplot(data = spct, mapping = mapping, range = range,
                    environment = environment)
  }


# collections of spectra --------------------------------------------------

#' @rdname ggplot
#'
#' @export
#'
ggplot.generic_mspct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    spct <- photobiology::rbindspct(data)
    ggplot2::ggplot(data = spct, mapping = mapping, ..., environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.filter_mspct <-
  function(data, mapping = NULL, ..., range = NULL,
           plot.qty = getOption("photobiology.filter.qty", default = "transmittance"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    # conversion before binding as filter properties can differ
    if (is.null(mapping)) {
      if (plot.qty == "transmittance") {
        data <- photobiology::any2T(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["Tfr"]])
      } else if (plot.qty == "absorptance") {
        data <- photobiology::any2Afr(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["Afr"]])
      } else if (plot.qty == "absorbance") {
        data <- photobiology::any2A(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["A"]])
      } else {
        stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
      }
    }
    spct <- photobiology::rbindspct(data)
    ggplot(data = spct,
           mapping = mapping,
           plot.qty = plot.qty,
           ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.source_mspct <-
  function(data, mapping = NULL, ..., range = NULL,
           unit.out = getOption("photobiology.radiation.unit", default = "energy"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    # conversion before binding, to avoid multiple conversions
    if (is.null(mapping)) {
      if (unit.out == "energy") {
        data <- photobiology::q2e(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.e.irrad"]])
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- photobiology::e2q(data, action = "replace")
        mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.q.irrad"]])
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
    }
    spct <- photobiology::rbindspct(data)
    ggplot(data = spct,
           mapping = mapping,
           unit.out = unit.out,
           ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.object_mspct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = getOption("photobiology.object.qty",
                                default = ifelse(length(data) > 1L,
                                                 "as.is", "all")),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data, range = range, use.hinges = TRUE, fill = NULL)
    }
    spct <- photobiology::rbindspct(data)
    ggplot(data = spct,
           mapping = mapping,
           ...,
           plot.qty = plot.qty,
           environment = environment)
  }
